
# 1. IMPORTAR DATOS ----------------------------------------------------------

# Semilla para reproducibilidad
set.seed(123)

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
train <- train_A %>%
  left_join(train_B, by = "property_id")

test <- test_A %>%
  left_join(test_B, by = "property_id")

# Seleccionar solo las variables que se usarán en la predicción
vars_model <- c("property_id", "price", "surface_total", "bedrooms", "bathrooms", "type_housing",
                "piscina", "garaje", "seguridad", "balcon", "gym", "year",
                "dist_parque", "AVALUO_COM", "dist_centComercial", "dist_cai",
                "dist_transmi", "num_paraderos_sitp", "num_colegios", "ESTRATO", 
                "num_restaurantes", "lon", "lat")

test_vars_model <- setdiff(vars_model, "price")  # test no tiene price

train <- train %>% select(all_of(vars_model))
test  <- test  %>% select(all_of(test_vars_model))


# Crear dummy para apartamento
train <- train %>%
  mutate(
    type_housing = factor(type_housing, levels = c("Casa", "Apartamento")),
    apto_casa = as.integer(type_housing == "Apartamento")
  ) %>%
  select(-type_housing)

test <- test %>%
  mutate(
    type_housing = factor(type_housing, levels = c("Casa", "Apartamento")),
    apto_casa = as.integer(type_housing == "Apartamento")
  ) %>%
  select(-type_housing)


# Verificar y convertir la variable price a numérico
train$price <- as.numeric(as.character(train$price))
if(any(is.na(train$price))) {
  warning("NAs detectados en price. Filtrando...")
  train <- train %>% filter(!is.na(price))
}

# Preparación de datos espaciales
coords_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
mean_lon <- mean(train$lon, na.rm = TRUE)
utm_zone <- floor((mean_lon + 180) / 6) + 1
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
coords_utm_sf <- st_transform(coords_sf, crs = utm_crs)

# 2. VALIDACIÓN CRUZADA ESPACIAL OPTIMIZADA ------------------------------------

# Análisis de variograma para determinar rango espacial
variog <- variogram(price ~ 1, as_Spatial(coords_utm_sf))
plot(variog, main = "Variograma Experimental")
theRange_value <- 1200  # Ajustar según punto de estabilización

# Crear folds espaciales con buffer
spatial_folds <- spatial_block_cv(
  coords_utm_sf,
  v = 5,
  cellsize = theRange_value,
  buffer = theRange_value/2,
  square = FALSE  # Bloques hexagonales para mejor cobertura
)

print(spatial_folds)
autoplot(spatial_folds)  # Si tienes ggplot2 instalado

# Visualización interactiva de los folds
#if(require(plotly)){
 # ggplotly(
  #  autoplot(spatial_folds) + 
   #   geom_sf(data = coords_utm_sf, aes(color = price), alpha = 0.6) +
    #  scale_color_viridis_c() +
     # ggtitle("Distribución de Precios y Folds Espaciales")
  #)
#}

# Preparar índices para caret
index_list <- map(spatial_folds$splits, ~ .x$in_id)

# 3. MODELO GBM CON TRANSFORMACIÓN LOGARÍTMICA ---------------------------------

# Configuración avanzada de entrenamiento
train_control <- trainControl(
  method = "cv",
  index = index_list,
  summaryFunction = function(data, lev = NULL, model = NULL) {
    c(MAE = Metrics::mae(data$obs, data$pred),
      MAE_log = Metrics::mae(log1p(data$obs), log1p(data$pred)),
      RMSE = Metrics::rmse(data$obs, data$pred),
      RMSLE = Metrics::rmse(log1p(data$obs), log1p(data$pred)))
  },
  verboseIter = TRUE,
  savePredictions = "final",
  allowParallel = TRUE
)

# Grid de parámetros optimizado
gbm_grid <- expand.grid(
  interaction.depth = c(5, 7, 9),
  n.trees = c(500, 750, 1000),
  shrinkage = c(0.01, 0.005),
  n.minobsinnode = c(10, 15)
)

# Entrenamiento paralelizado
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Modelo final con transformación logarítmica y distribución Laplace
gbm_model <- train(
  x = train %>% select(-c(property_id, price, lon, lat)),
  y = log1p(train$price),
  method = "gbm",
  distribution = "laplace",
  trControl = train_control,
  tuneGrid = gbm_grid,
  metric = "MAE",
  verbose = TRUE,
  bag.fraction = 0.8
)

stopCluster(cl)

# 4. EVALUACIÓN Y DIAGNÓSTICO DEL MODELO --------------------------------------

# Resultados de validación cruzada
cv_results <- gbm_model$resample %>% 
  group_by(Resample) %>% 
  summarise(MAE = mean(MAE), RMSLE = mean(RMSLE))

cv_results

# Gráfico de evolución del error
ggplot(gbm_model) + 
  geom_line(aes(y = MAE, color = as.factor(interaction.depth))) +
  facet_wrap(~ shrinkage, scales = "free_y") +
  labs(title = "Evolución del MAE por Hiperparámetros",
       color = "Profundidad") +
  theme_bw()

# Importancia de variables mejorada
var_imp <- varImp(gbm_model, scale = TRUE)
ggplot(var_imp, top = 15) + 
  ggtitle("Importancia de Variables (Escalada)") +
  theme(axis.text.y = element_text(size = 8))

# Diagnóstico espacial de residuales
train$pred <- expm1(predict(gbm_model, newdata = train))
train$residuals <- train$price - train$pred

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)  # CRS para WGS84

ggplot(train_sf) +
  geom_sf(aes(color = residuals), size = 1.2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, limits = c(-quantile(abs(train$residuals), 0.99), 
                                                 quantile(abs(train$residuals), 0.99))) +
  ggtitle("Mapa de Residuales (Escala Recortada al 99%)") +
  theme_minimal()


# 7. VALIDACIÓN CON LOS DATOS TRAI -------------------------------------------


# 2. Reconstruir las predicciones en escala original y calcular métricas
train$pred_scaled <- predict(gbm_model, newdata = train)  # Predicciones en escala logarítmica
train$pred_original <- expm1(train$pred_scaled)           # Convertir a escala original

# 3. Calcular métricas de error en escala original
mae_original <- mean(abs(train$price - train$pred_original))
rmse_original <- sqrt(mean((train$price - train$pred_original)^2))
mape <- mean(abs((train$price - train$pred_original)/train$price), na.rm = TRUE) * 100

# 4. Resultados de la validación
cat("\n=== MÉTRICAS DE VALIDACIÓN EN ESCALA ORIGINAL ===\n")
cat("MAE (escala original):", round(mae_original, 2), "\n")
cat("RMSE (escala original):", round(rmse_original, 2), "\n")
cat("MAPE (%):", round(mape, 2), "%\n")

# 5. Análisis de residuales en escala original
residual_stats <- summary(train$residuals)
cat("\n=== ESTADÍSTICAS DE RESIDUALES ===\n")
print(residual_stats)

# 6. Gráfico de residuales vs valores reales
ggplot(train, aes(x = price, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Residuales vs Valores Reales (Escala Original)",
       x = "Precio Real", y = "Residual") +
  theme_minimal()

# 7. Distribución de residuales
ggplot(train, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribución de Residuales", x = "Residual", y = "Densidad") +
  theme_minimal()

# 8. Gráfico QQ para normalidad de residuales
ggplot(train, aes(sample = residuals)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Gráfico Q-Q de Residuales") +
  theme_minimal()

# 9. Mapa de residuales espaciales (versión mejorada)
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# Definir límites simétricos para la escala de colores
residual_limit <- quantile(abs(train$residuals), 0.99)

ggplot() +
  geom_sf(data = train_sf, aes(color = residuals), size = 1.2, alpha = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0,
                        limits = c(-residual_limit, residual_limit),
                        name = "Residuales") +
  labs(title = "Distribución Espacial de Residuales",
       subtitle = paste("MAE:", round(mae_original, 2), 
                        "| MAPE:", round(mape, 2), "%")) +
  theme_minimal() +
  theme(legend.position = "bottom")


# 6. GENERACIÓN DE PREDICCIONES PARA KAGGLE ----------------------------------

# Obtener los mejores hiperparámetros del modelo
best_params <- gbm_model$bestTune

# Crear nombre descriptivo del archivo con hiperparámetros (formato solicitado)
best_params_str <- paste0(
  "gbm_trees", best_params$n.trees,
  "_depth", best_params$interaction.depth,
  "_shrink", best_params$shrinkage,
  "_minobs", best_params$n.minobsinnode
)

# Función para predecir y ajustar valores atípicos
predict_final_prices <- function(model, newdata) {
  # Predecir en escala logarítmica y convertir
  pred <- expm1(predict(model, newdata = newdata))
  
  # Ajustar outliers basado en los percentiles de entrenamiento
  q_low <- quantile(train$price, 0.01)
  q_high <- quantile(train$price, 0.99)
  pred <- pmin(pmax(pred, q_low), q_high)
  
  # Redondear a números enteros sin decimales
  return(round(pred, 0))
}

# Generar predicciones finales
test_pred <- predict_final_prices(gbm_model, test)

# Preparar datos de submission
submission <- data.frame(
  property_id = test$property_id,
  price = test_pred
)

# Generar nombre del archivo exactamente como se solicita
file_name <- paste0(
  "Boosting_GB_log(price)_",
  best_params_str,
  ".csv"
)

# Guardar el archivo CSV
write.csv(submission, file.path(submission_path, file_name), row.names = FALSE)

# Verificación final del archivo generado
cat("\n=== Archivo de Submission Generado ===\n")
cat("Nombre del archivo:", file_name, "\n")
cat("Ruta completa:", file.path(submission_path, file_name), "\n")
cat("Número de predicciones:", nrow(submission), "\n")
cat("Rango de precios predichos:\n")
print(summary(submission$price))

# Mostrar primeras líneas del archivo
cat("\nPrimeras 5 predicciones:\n")
print(head(submission, 5))










