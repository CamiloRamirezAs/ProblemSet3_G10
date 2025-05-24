library(SuperLearner)
library(xgboost)
library(dplyr)
library(spatialsample)
library(caret)
library(sf)
library(ggplot2)

# --- 1. Datos base ---
train_raw <- train_raw  # base de entrenamiento con variable price
test_raw <- test_raw    # base test SIN variable price (Kaggle)

# --- 2. Crear variable log-transformada para target ---
# Se recomienda modelar el logaritmo del precio para mejor desempeño y distribución
train_raw <- train_raw %>% mutate(logprice = log(price))
# test_raw no tiene price, no hacemos log

# --- 3. Variables predictoras ---
vars <- c("surface_total", "bedrooms", "bathrooms", "type_housing",
          "piscina", "garaje", "seguridad", "balcon", "gym", "year",
          "dist_parque", "AVALUO_COM", "dist_centComercial", "dist_cai",
          "dist_transmi", "num_paraderos_sitp", "num_colegios", "ESTRATO", 
          "num_restaurantes")

# --- 4. Crear clusters espaciales para validación cruzada espacial ---
# Usamos función spatial_clustering_cv para crear folds espaciales
set.seed(12345)
train_sf <- st_as_sf(train_raw, coords = c("longitude", "latitude"), crs = 4326) # Ajusta si tus columnas se llaman diferente

spatial_folds <- spatial_clustering_cv(
  data = train_sf,
  coords = c("longitude", "latitude"),
  cluster_function = kmeans,
  v = 5
)

# Obtener índices para validación cruzada espacial
clusters <- spatial_folds$folds
# clusters es una lista con índices de validación, invertimos para obtener índices de entrenamiento
folds_SL <- lapply(clusters, function(val_idx) setdiff(seq_len(nrow(train_raw)), val_idx))
V <- length(folds_SL)

# --- 5. Control de entrenamiento para tuning XGBoost con validación espacial ---
train_control_spatial <- trainControl(
  method = "cv",
  index = folds_SL,
  verboseIter = TRUE,
  summaryFunction = defaultSummary,
  selectionFunction = "best",
  savePredictions = "final",
  returnResamp = "all"
)

# --- 6. Definir grid de hiperparámetros para XGBoost ---
xgb_grid <- expand.grid(
  nrounds = c(100, 300),
  max_depth = c(4, 6),
  eta = c(0.1, 0.05),
  gamma = c(0, 1, 5),           # Ahora sí tuneamos gamma con varios valores
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# --- 7. Tunear XGBoost con caret y validación espacial ---
set.seed(123)
xgb_tune <- train(
  x = train_raw %>% select(all_of(vars)),
  y = train_raw$logprice,
  method = "xgbTree",
  trControl = train_control_spatial,
  tuneGrid = xgb_grid,
  metric = "MAE"
)

best_params <- xgb_tune$bestTune
print("Mejores hiperparámetros XGBoost:")
print(best_params)

# --- 8. Crear learner XGBoost personalizado con hiperparámetros óptimos ---
SL.xgboost.custom <- function(Y, X, newX, family, obsWeights, ...) {
  require(xgboost)
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample,
    eval_metric = "mae"
  )
  
  dtrain <- xgb.DMatrix(data = as.matrix(X), label = Y, weight = obsWeights)
  dtest <- xgb.DMatrix(data = as.matrix(newX))
  
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_params$nrounds,
    verbose = 0
  )
  
  pred <- predict(xgb_model, dtest)
  
  fit <- list(object = xgb_model)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.xgboost.custom")
  return(out)
}

predict.SL.xgboost.custom <- function(object, newdata, ...) {
  dnew <- xgb.DMatrix(as.matrix(newdata))
  predict(object$object, dnew)
}

# --- 9. Definir learners para SuperLearner ---
learners <- c("SL.lm", "SL.xgboost.custom")

# --- 10. Preparar datos para SuperLearner ---
X_train <- train_raw %>% select(all_of(vars))
Y_train <- train_raw$logprice

# --- 11. Entrenar Super Learner con validación cruzada espacial ---
set.seed(123)
fit_SL <- SuperLearner(
  Y = Y_train,
  X = X_train,
  SL.library = learners,
  method = "method.NNLS",
  family = gaussian(),
  cvControl = list(V = V, validRows = folds_SL),
  verbose = TRUE
)

print(fit_SL)

# --- 12. Gráficas para diagnóstico (entrenamiento) ---

# 12a. Gráfico Observado vs Predicho (log price) en entrenamiento usando validación interna
train_pred <- fit_SL$cvRisk
cv_preds <- fit_SL$SL.predict

df_pred <- data.frame(
  observed = Y_train,
  predicted = cv_preds
)

p1 <- ggplot(df_pred, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(title = "Observado vs Predicho (log precio) en entrenamiento",
       x = "Log Precio Observado", y = "Log Precio Predicho") +
  theme_minimal()

ggsave("observado_vs_predicho_entrenamiento.png", p1, width = 7, height = 5)

# 12b. Gráfico distribución de residuales
df_pred <- df_pred %>% mutate(residual = observed - predicted)

p2 <- ggplot(df_pred, aes(x = residual)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  labs(title = "Distribución de residuales en entrenamiento (log precio)",
       x = "Residual", y = "Frecuencia") +
  theme_minimal()

ggsave("residuales_entrenamiento.png", p2, width = 7, height = 5)

# 12c. Boxplot de residuales por folds (opcional)
folds_vec <- rep(NA, length(Y_train))
for (i in seq_along(folds_SL)) {
  val_idx <- setdiff(seq_len(nrow(train_raw)), folds_SL[[i]])
  folds_vec[val_idx] <- paste0("Fold_", i)
}

df_pred$fold <- folds_vec

p3 <- ggplot(df_pred, aes(x = fold, y = residual)) +
  geom_boxplot() +
  labs(title = "Boxplot de residuales por fold (entrenamiento)",
       x = "Fold", y = "Residual") +
  theme_minimal()

ggsave("boxplot_residuales_por_fold.png", p3, width = 7, height = 5)

# --- 13. Predicción en test y guardado (sin evaluación porque no hay price) ---
X_test <- test_raw %>% select(all_of(vars))
pred_test_log <- predict(fit_SL, newdata = X_test, onlySL = TRUE)$pred

test_raw <- test_raw %>% mutate(price_hat = exp(pred_test_log))

# Guardar archivo para Kaggle con ID y predicción
submission_path <- "ruta/donde/quieras/guardar"  # Cambia esta ruta a tu carpeta
file_name <- "submission_superlearner_spatial.csv"
write.csv(test_raw %>% select(ID, price_hat), file.path(submission_path, file_name), row.names = FALSE)
cat("Archivo de predicción guardado en:", file.path(submission_path, file_name), "\n")
