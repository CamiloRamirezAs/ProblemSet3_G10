#-----------------------------------------------------------------------------//
# Modelo SupeLearner con validación cruzada espacial: 
# XGBoost, Random Forest, media y regresión lineal
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 23 de mayo de 2025
#-----------------------------------------------------------------------------//



# 1. IMPORTAR DATOS ------------------------------------------------------------

train_A <- readRDS(file.path(stores_path, "train_data.rds"))
test_A  <- readRDS(file.path(stores_path, "test_data.rds"))

train_A <- as.data.frame(train_A)
test_A  <- as.data.frame(test_A)

train_A <- train_A %>% dplyr::select(-geometry)

train_B <- readRDS(file.path(stores_path, "spatial_train.rds"))
test_B  <- readRDS(file.path(stores_path, "spatial_test.rds"))

train_raw <- train_A %>% left_join(train_B, by = "property_id")
test_raw  <- test_A %>% left_join(test_B, by = "property_id")

vars_model <- c("property_id", "price", "surface_total", "bedrooms", "bathrooms", "type_housing",
                "piscina", "garaje", "seguridad", "balcon", "gym", "year",
                "dist_parque", "AVALUO_COM", "dist_centComercial", "dist_cai",
                "dist_transmi", "num_paraderos_sitp", "num_colegios", "ESTRATO", 
                "num_restaurantes", "lon", "lat")

test_vars_model <- setdiff(vars_model, "price")

train_raw <- train_raw %>% select(all_of(vars_model))
test_raw  <- test_raw  %>% select(all_of(test_vars_model))

# 2. PREPROCESAMIENTO ----------------------------------------------------------

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

train_sf <- st_as_sf(train_raw, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
test_sf  <- st_as_sf(test_raw, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

model_formula <- price ~ surface_total + bedrooms + bathrooms + apto_casa +
  piscina + garaje + seguridad + balcon + gym + year +
  dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
  dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
  num_restaurantes

model_formula_rhs <- as.formula("~ surface_total + bedrooms + bathrooms + apto_casa +
  piscina + garaje + seguridad + balcon + gym + year +
  dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
  dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + num_restaurantes")

property_ids_test <- test_raw$property_id

X_train <- model.matrix(model_formula, data = train_sf)[, -1]
Y_train <- train_sf$price
X_test <- model.matrix(model_formula_rhs, data = test_sf)[, -1]

X_train_df <- as.data.frame(X_train)
X_test_df <- as.data.frame(X_test)

# 3. VALIDACIÓN CRUZADA ESPACIAL ----------------------------------------------

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

# Visualización de los folds
p <- autoplot(block_folds)
ggsave(filename = file.path(view_path, "SuperLearner_folds_plot.png"), plot = p, width = 8, height = 6, dpi = 300)

# Crear folds compatibles con SuperLearner
folds_SL <- lapply(block_folds$splits, function(split) {
  as.integer(rownames(analysis(split)))
})

# Validación: asegurar que todos los datos estén cubiertos
all_indices <- sort(unique(unlist(folds_SL)))
if (!all(all_indices == 1:nrow(train_sf))) {
  message("Fold incompletos, usando folds aleatorios de caret...")
  set.seed(123)
  folds_SL <- createFolds(Y_train, k = 5, returnTrain = TRUE)
}

V <- length(folds_SL)

# 4. FUNCIONES CUSTOM PARA SUPERLEARNER ---------------------------------------

SL.xgboost.robust <- function(Y, X, newX, family, obsWeights, 
                              max_depth = 7, eta = 0.05, gamma = 0, nrounds = 500, ...) {
  tryCatch({
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    params <- list(
      booster = "gbtree", objective = "reg:squarederror",
      max_depth = max_depth, eta = eta, gamma = gamma,
      colsample_bytree = 0.8, min_child_weight = 1, subsample = 1,
      eval_metric = "mae"
    )
    dtrain <- xgb.DMatrix(data = X, label = Y, weight = obsWeights)
    dtest <- xgb.DMatrix(data = newX)
    xgb_model <- xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
    pred <- predict(xgb_model, dtest)
    return(list(pred = pred, fit = list(object = xgb_model)))
  }, error = function(e) {
    warning("Error en XGBoost: ", e$message)
    pred <- rep(mean(Y), nrow(newX))
    return(list(pred = pred, fit = list(object = "mean_model")))
  })
}

SL.ranger.robust <- function(Y, X, newX, family, obsWeights,
                             num.trees = 1000, mtry = 8, min.node.size = 1, ...) {
  tryCatch({
    X <- as.data.frame(X)
    newX <- as.data.frame(newX)
    mtry <- min(mtry, ncol(X))
    data_train <- cbind(Y = Y, X)
    rf_model <- ranger::ranger(
      Y ~ ., data = data_train, num.trees = num.trees,
      mtry = mtry, min.node.size = min.node.size,
      case.weights = obsWeights, seed = 123, verbose = FALSE
    )
    pred <- predict(rf_model, data = newX)$predictions
    return(list(pred = pred, fit = list(object = rf_model)))
  }, error = function(e) {
    warning("Error en Random Forest: ", e$message)
    pred <- rep(median(Y), nrow(newX))
    return(list(pred = pred, fit = list(object = "median_model")))
  })
}

# Registrar funciones en entorno global
assign("SL.xgboost.robust", SL.xgboost.robust, envir = .GlobalEnv)
assign("SL.ranger.robust", SL.ranger.robust, envir = .GlobalEnv)

# 5. ENTRENAMIENTO SUPERLEARNER ------------------------------------------------

sl_library <- c("SL.mean", "SL.glm", "SL.xgboost.robust", "SL.ranger.robust")

set.seed(123)
fit_SL <- SuperLearner(
  Y = Y_train,
  X = X_train_df,
  SL.library = sl_library,
  method = "method.NNLS",
  family = gaussian(),
  cvControl = list(
    V = V,
    validRows = folds_SL,
    shuffle = FALSE
  ),
  verbose = TRUE
)


# 6. VALIDACIÓN SOBRE DATOS DE ENTRENAMIENTO (OUT-OF-FOLD) ---------------------

# Predicciones out-of-fold
oof_preds <- fit_SL$Z %*% fit_SL$coef

# DataFrame de predicciones vs observaciones reales
train_eval <- data.frame(
  property_id = train_raw$property_id,
  price_real = Y_train,
  price_pred = oof_preds
)

# Calcular métricas de desempeño
mae_train  <- mean(abs(train_eval$price_real - train_eval$price_pred))
rmse_train <- sqrt(mean((train_eval$price_real - train_eval$price_pred)^2))
r2_train   <- 1 - sum((train_eval$price_real - train_eval$price_pred)^2) /
  sum((train_eval$price_real - mean(train_eval$price_real))^2)

# Mostrar resultados
cat("Desempeño en datos de entrenamiento (out-of-fold):\n")
cat("MAE  : ", round(mae_train, 2), "\n")
cat("RMSE : ", round(rmse_train, 2), "\n")
cat("R²   : ", round(r2_train, 4), "\n")

# Guardar predicciones OOF para posterior análisis
write.csv(train_eval, "train_predictions_oof.csv", row.names = FALSE)



# 7. PREDICCIONES Y EXPORTACIÓN ------------------------------------------------

# Definir nombre base con modelos e hiperparámetros completos
sl_models <- "mean_glm_xgb_rf"

# Hiperparámetros XGBoost
xgb_params <- paste0(
  "md7_eta0.05_gamma0_col0.8_mcw1_sub1_nr500"
)

# Hiperparámetros Random Forest
rf_params  <- paste0("nt1000_mtry8_mns1")

# Nombre base del archivo
file_prefix <- paste0(sl_models, "_xgb", xgb_params, "_rf", rf_params)

# Predicciones out-of-fold para evaluar CV interna
oof_preds <- fit_SL$Z %*% fit_SL$coef
train_predictions <- data.frame(
  property_id = train_raw$property_id,
  price_pred = oof_preds
)

# Guardar predicciones sobre train (OOF)
write.csv(train_predictions,
          paste0("train_predictions_oof_", file_prefix, ".csv"),
          row.names = FALSE)

assign("SL.xgboost.robust", SL.xgboost.robust, envir = .GlobalEnv)
assign("SL.ranger.robust", SL.ranger.robust, envir = .GlobalEnv)

# Predicciones sobre test

predict_manual <- function(model, newdata) {
  # Obtener predicciones individuales
  preds <- list()
  
  for (algo in model$libraryNames) {
    model_obj <- model$fitLibrary[[algo]]$object
    
    # Manejar diferentes tipos de modelos
    if (is.numeric(model_obj) || is.character(model_obj)) {
      # Para SL.mean (devuelve la media) o modelos fallback
      preds[[algo]] <- rep(model_obj, nrow(newdata))
    } else if (inherits(model_obj, "xgb.Booster")) {
      # Para XGBoost
      dtest <- xgb.DMatrix(data = as.matrix(newdata))
      preds[[algo]] <- predict(model_obj, dtest)
    } else if (inherits(model_obj, "ranger")) {
      # Para Random Forest
      preds[[algo]] <- predict(model_obj, data = as.data.frame(newdata))$predictions
    } else if (inherits(model_obj, "glm")) {
      # Para GLM
      preds[[algo]] <- predict(model_obj, newdata = as.data.frame(newdata), type = "response")
    } else {
      # Para otros casos
      preds[[algo]] <- rep(NA, nrow(newdata))
      warning("No se pudo predecir para el algoritmo: ", algo)
    }
  }
  
  # Combinar según los coeficientes
  pred_matrix <- do.call(cbind, preds)
  final_pred <- pred_matrix %*% model$coef
  
  return(final_pred)
}

# Hacer la predicción
pred_test <- predict_manual(fit_SL, X_test_df)

# Crear submission para Kaggle con price redondeado a 0 decimales
submission <- data.frame(
  property_id = property_ids_test,
  price = round(pred_test, 0)
)

# Guardar submission con nombre detallado
write.csv(submission,
          paste0("SuperLearner_", file_prefix, ".csv"),
          row.names = FALSE)








