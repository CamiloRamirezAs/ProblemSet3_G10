#-----------------------------------------------------------------------------//
# Modelo Super learner: XGBoost & lineal regression
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 22 de mayo de 2025
#-----------------------------------------------------------------------------//


# 1. IMPORTAR DATOS ------------------------------------------------------------

# Bases iniciales
train_A <- readRDS(file.path(stores_path, "train_data.rds"))
test_A  <- readRDS(file.path(stores_path, "test_data.rds"))

train_A <- as.data.frame(train_A)
test_A <- as.data.frame(test_A)

# Eliminar la geometría innecesaria
train_A <- train_A %>% 
            dplyr::select(-geometry)


# Base espaciales
train_B <- readRDS(file.path(stores_path, "spatial_train.rds"))
test_B  <- readRDS(file.path(stores_path, "spatial_test.rds"))

# Join de las bases
train_raw <- train_A %>%
  left_join(train_B, by = "property_id")

test_raw <- test_A %>%
  left_join(test_B, by = "property_id")


# Seleccionar solo las variables que se usarán en la predicción
vars_model <- c("property_id", "price", "surface_total", "bedrooms", "bathrooms", "type_housing",
                "piscina", "garaje", "seguridad", "balcon", "gym", "year",
                "dist_parque", "AVALUO_COM", "dist_centComercial", "dist_cai",
                "dist_transmi", "num_paraderos_sitp", "num_colegios", "ESTRATO", 
                "num_restaurantes", "lon", "lat")

test_vars_model <- setdiff(vars_model, "price")  # test no tiene price

train_raw <- train_raw %>% select(all_of(vars_model))
test_raw  <- test_raw  %>% select(all_of(test_vars_model))


# 2. PREPROCESAMIENTO ----------------------------------------------------------

# Crear dummy para apartamento
train_raw <- train_raw %>%
            mutate(
              type_housing = factor(type_housing, levels = c("Casa", "Apartamento")),
              apto_casa = as.integer(type_housing == "Apartamento")
            ) %>%
            select(-type_housing)

test_raw <- test_raw %>%
            mutate(
              type_housing = factor(type_housing, levels = c("Casa", "Apartamento")),
              apto_casa = as.integer(type_housing == "Apartamento")
            ) %>%
            select(-type_housing)


# Crear objetos sf con CRS para lon/lat (WGS84 - EPSG:4326) sin eliminar columnas
train_sf <- st_as_sf(train_raw, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
test_sf  <- st_as_sf(test_raw, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Variable precio log-transformada
train_sf <- train_sf %>% 
          mutate(logprice = log(price))  # test_raw no tiene price, no hacemos log

# Fórmula del modelo
model_formula <- logprice ~ surface_total + bedrooms + bathrooms + apto_casa +
                piscina + garaje + seguridad + balcon + gym + year +
                dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
                dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
                num_restaurantes

# Guardar property_id antes de crear la matriz de predictores
property_ids_test <- test_raw$property_id

# Crear matriz de predictores solo con variables independientes
model_formula_rhs <- as.formula("~ surface_total + bedrooms + bathrooms + apto_casa +
  piscina + garaje + seguridad + balcon + gym + year +
  dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
  dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + num_restaurantes")

X_test <- model.matrix(model_formula_rhs, data = test_raw)[, -1, drop = FALSE]


# Variables predictoras y target
X_train <- model.matrix(model_formula, data = train_sf)[, -1]
Y_train <- train_sf$logprice

X_test <- model.matrix(model_formula_rhs, data = test_sf)[, -1]


# 3. CLUSTERS ESPACIALES PARA LA VALIDACIÓN CRUZADA ----------------------------

# crear folds espaciales
set.seed(123)

block_folds <- spatial_block_cv(train_sf, v = 5)
print(block_folds)

# Diagramar los folds
autoplot(block_folds)
p <- autoplot(block_folds)

# Guardar como PNG
ggsave(filename = file.path(view_path, "SuperLearner1_folds_plot.png"), plot = p, width = 8, height = 6, dpi = 300)


# Obtener índices de validación para caret y SuperLearner
folds_SL <- lapply(block_folds$splits, function(split) {
  analysis_indices <- analysis(split) %>% as.data.frame() %>% rownames() %>% as.integer()
  intersect(analysis_indices, 1:nrow(train_sf))
})

# Número de particiones (folds)
V <- length(folds_SL)


# 4. XGBOOST -------------------------------------------------------------------

# Función resumen personalizada con MAE
fiveStats <- function(data, lev = NULL, model = NULL) {
  require(Metrics)
  
  # Calcular métricas
  mae <- Metrics::mae(data$obs, data$pred)  # MAE
  rmse <- Metrics::rmse(data$obs, data$pred)  # RMSE
  r2 <- cor(data$obs, data$pred)^2  # R-squared
  med_error <- median(abs(data$obs - data$pred))  # Median error
  
  # Retornar un named vector con el MAE como métrica principal
  c(MAE = mae, RMSE = rmse, Rsquared = r2, MedianError = med_error)
}

# Definir índices para caret
index_caret <- lapply(folds_SL, function(idx) idx)

# Control de entrenamiento para tuning XGBoost con validación espacial

train_control_spatial <- trainControl(
  method = "cv",
  index = index_caret, # Índices personalizados
  verboseIter = TRUE,
  summaryFunction = fiveStats,
  selectionFunction = "best",
  savePredictions = "final",
  returnResamp = "all"
)

# Definir grid de hiperparámetros para XGBoost ---
xgb_grid <- expand.grid(
  nrounds = c(250, 500),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.05, 0.1),
  gamma = c(0, 0.1, 1),
  min_child_weight = c(1, 3, 5),  # Peso mínimo de un nodo hijo
  colsample_bytree = c(0.6, 0.8),
  subsample = 1
)

xgb_grid

# Entrenar el modelo XGBoost
set.seed(123)
xgb_tune <- train(
  x = X_train,
  y = Y_train,
  method = "xgbTree",
  trControl = train_control_spatial,
  tuneGrid = xgb_grid,
  metric = "MAE",
  verbosity = 0
)

# Mejor conjunto de hiperparámetros
best_params <- xgb_tune$bestTune
print(best_params)




# 5. SUPERLEARNER -------------------------------------------------------------

# Crear función personalizada para XGBoost en SuperLearner
SL.xgboost.custom <- function(Y, X, newX, family, obsWeights, ...) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.matrix(newX)) newX <- as.matrix(newX)
  
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
  
  dtrain <- xgb.DMatrix(data = X, label = Y, weight = obsWeights)
  dtest <- xgb.DMatrix(data = newX)
  
  xgb_model <- xgb.train(params = params, data = dtrain, nrounds = best_params$nrounds, verbose = 0)
  pred <- predict(xgb_model, dtest)
  
  fit <- list(object = xgb_model)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.xgboost.custom")
  return(out)
}

# Método de predicción para SuperLearner
predict.SL.xgboost.custom <- function(object, newdata, ...) {
  if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
  dnew <- xgb.DMatrix(data = newdata)
  predict(object$object, dnew)
}

# Crear folds usando caret 
set.seed(123)
folds <- createFolds(Y_train, k = 5, returnTrain = FALSE)
valid_folds <- folds  # Cada elemento es un vector de índices de validación

# Entrenar SuperLearner con tus modelos base
set.seed(123)
fit_SL <- SuperLearner(
  Y = Y_train,
  X = X_train,
  SL.library = c("SL.lm", "SL.xgboost.custom"),
  method = "method.NNLS",
  family = gaussian(),
  cvControl = list(V = length(valid_folds), validRows = valid_folds),
  verbose = TRUE
)

# Validación interna (MAE en escala original)
val_predictions <- predict(fit_SL, newdata = X_train, onlySL = TRUE)$pred
mae_train <- mean(abs(exp(val_predictions) - exp(Y_train)))
cat("MAE en entrenamiento:", mae_train, "\n")



# 6. PREDICCIÓN EN TEST PARA KAGGLE -------------------------------------------

# Obtener predicción en escala log
pred_test_log <- predict(fit_SL, newdata = X_test, onlySL = TRUE)$pred

# Volver a escala original (precio)
pred_test_price <- round(exp(pred_test_log))

# Crear data.frame con property_id y predicción
submission <- data.frame(
                property_id = property_ids_test,  # Guardado antes de eliminar columnas
                price = pred_test_price
              )

# Crear nombre dinámico para el archivo con los hiperparámetros usados
best_params_str <- paste0(
  "xgb_maxdepth", best_params$max_depth,
  "_eta", best_params$eta,
  "_gamma", best_params$gamma,
  "_nrounds", best_params$nrounds
)

# Nombre del archivo para guardar las predicciones
file_name <- paste0("SuperLearner_lm_xgboost_", best_params_str, ".csv")


# Guardar el archivo CSV
write.csv(submission, file = file.path(submission_path, file_name), row.names = FALSE)
cat("Archivo de predicción guardado en:", file.path(submission_path, file_name), "\n")





