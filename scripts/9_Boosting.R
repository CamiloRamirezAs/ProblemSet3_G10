#-----------------------------------------------------------------------------//
# Modelo Boosting
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 23 de mayo de 2025
#-----------------------------------------------------------------------------//


# 1. IMPORTAR DATOS ------------------------------------------------------------

# Cargar datos de entrenamiento y prueba
train <- readRDS(file.path(stores_path, "train_data.rds"))
test <- readRDS(file.path(stores_path, "test_data.rds"))

# Separar variables predictoras y target
vars_quitar <- c("price", "property_id")
X_train <- train %>% select(-all_of(vars_quitar))
Y_train <- train$price
X_test <- test %>% select(-property_id)

# 2. COORDENADAS COMO OBJETO sf ------------------------------------------------

# Crear objeto sf con coordenadas geográficas
coords_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# Calcular zona UTM según longitud media
mean_lon <- mean(train$lon, na.rm = TRUE)
utm_zone <- floor((mean_lon + 180) / 6) + 1
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")

# Transformar coordenadas a UTM
coords_utm_sf <- st_transform(coords_sf, crs = utm_crs)
coords_utm_sf$price <- Y_train

# 3. ANALIZAR VARIOGRAMA -------------------------------------------------------

# Convertir a objeto Spatial para el variograma
coords_utm_sp <- as_Spatial(coords_utm_sf)

# Calcular variograma
variog <- variogram(price ~ 1, data = coords_utm_sp)
plot(variog)

# Estimar alcance del variograma (usaremos este valor para cellsize)
theRange_value <- 1500  # Ajustado según tu variograma

# 4. VALIDACIÓN CRUZADA ESPACIAL -----------------------------------------------

# Crear folds espaciales usando spatialsample
spatial_cv <- spatial_block_cv(
  coords_utm_sf,
  v = 5,                   # 5 folds
  cellsize = theRange_value # Usamos el rango del variograma
)

# Verificar folds
print(spatial_cv)

# Visualizar bloques (requiere ggplot2)
if(require(ggplot2)){
  autoplot(spatial_cv)
}

# 5. EXTRAER ÍNDICES PARA CARET ------------------------------------------------

# Obtener índices de entrenamiento (forma correcta para spatialsample)
index_caret <- map(spatial_cv$splits, ~ .x$in_id)  # in_id contiene los índices de entrenamiento
names(index_caret) <- paste0("Fold", seq_along(index_caret))

# Verificar distribución de datos por fold
fold_counts <- map_dbl(index_caret, length)
print(fold_counts)



# 6. Validación cruzada espacial ----------------------------------------------
spatial_blocks <- spatialBlock(
  speciesData = as.data.frame(coords_utm@coords),  # usa coords en UTM (metros)
  theRange = theRange_value,  
  k = 5,
  selection = "random",
  iteration = 100,
  showBlocks = TRUE,  # ahora sí para visualizar bloques
  progress = FALSE
)

# Crear índices para caret a partir de bloques espaciales
index_caret <- spatial_blocks$folds %>% purrr::map(~.$train)

# 7. Crear función de evaluación para regresión -------------------------------
regressionStats <- function(data, lev = NULL, model = NULL) {
  mae_val <- mae(data$obs, data$pred)
  rmse_val <- rmse(data$obs, data$pred)
  r2_val <- cor(data$obs, data$pred)^2
  c(MAE = mae_val, RMSE = rmse_val, Rsquared = r2_val)
}

# 8. Control de entrenamiento -------------------------------------------------
train_control_gbm <- trainControl(
  method = "cv",
  index = index_caret,
  summaryFunction = regressionStats,
  verboseIter = TRUE,
  savePredictions = "final",
  returnResamp = "all"
)

# 8. Grid de hiperparámetros --------------------------------------------------
gbm_grid <- expand.grid(
  n.trees = c(100, 300, 500),
  interaction.depth = c(1, 3, 5),
  shrinkage = c(0.01, 0.05, 0.1),
  n.minobsinnode = c(5, 10)
)

# 9. Entrenar modelo GBM ------------------------------------------------------
set.seed(123)
gbm_model <- train(
  x = X_train,
  y = Y_train,
  method = "gbm",
  trControl = train_control_gbm,
  tuneGrid = gbm_grid,
  metric = "MAE",
  verbose = FALSE
)

# 10. Evaluar resultados en validación cruzada -------------------------------
gbm_model$results %>%
  arrange(MAE) %>%
  head()

# 11. Validación interna: predecir sobre train y calcular MAE -----------------
pred_train <- predict(gbm_model, newdata = X_train)
mae_train <- Metrics::mae(train$price, pred_train)
cat("MAE en conjunto train: ", mae_train, "\n")

# 12. Predecir sobre conjunto test --------------------------------------------
pred_test <- predict(gbm_model, newdata = X_test)

# 13. Guardar archivo para Kaggle ---------------------------------------------
predicciones_kaggle <- data.frame(
  property_id = property_ids_test,
  pred_price = pred_test
)

write_csv(predicciones_kaggle, "kaggle_gbm_predictions.csv")



