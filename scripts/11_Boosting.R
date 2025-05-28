#-----------------------------------------------------------------------------//
# Modelo Boosting con validación cruzada espacial
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 23 de mayo de 2025
#-----------------------------------------------------------------------------//


# 1. IMPORTAR DATOS ----------------------------------------------------------
# (Esta sección permanece igual que en tu código original)
set.seed(123)
train_A <- readRDS(file.path(stores_path, "train_data.rds"))
test_A  <- readRDS(file.path(stores_path, "test_data.rds"))
train_A <- as.data.frame(train_A)
test_A <- as.data.frame(test_A)
train_A <- train_A %>% dplyr::select(-geometry)

train_B <- readRDS(file.path(stores_path, "spatial_train.rds"))
test_B  <- readRDS(file.path(stores_path, "spatial_test.rds"))

train <- train_A %>% left_join(train_B, by = "property_id")
test <- test_A %>% left_join(test_B, by = "property_id")

vars_model <- c("property_id", "price", "surface_total", "bedrooms", "bathrooms", "type_housing",
                "piscina", "garaje", "seguridad", "balcon", "gym", "year",
                "dist_parque", "AVALUO_COM", "dist_centComercial", "dist_cai",
                "dist_transmi", "num_paraderos_sitp", "num_colegios", "ESTRATO", 
                "num_restaurantes", "lon", "lat")

test_vars_model <- setdiff(vars_model, "price")
train <- train %>% select(all_of(vars_model))
test  <- test  %>% select(all_of(test_vars_model))

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

train$price <- as.numeric(as.character(train$price))
if(any(is.na(train$price))) {
  warning("NAs detectados en price. Filtrando...")
  train <- train %>% filter(!is.na(price))
}

# 2. VALIDACIÓN CRUZADA ESPACIAL --------------------------------------------
# (Esta sección permanece igual)
coords_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
mean_lon <- mean(train$lon, na.rm = TRUE)
utm_zone <- floor((mean_lon + 180) / 6) + 1
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
coords_utm_sf <- st_transform(coords_sf, crs = utm_crs)

variog <- variogram(price ~ 1, as_Spatial(coords_utm_sf))
plot(variog, main = "Variograma Experimental")
theRange_value <- 1200

spatial_folds <- spatial_block_cv(
  coords_utm_sf,
  v = 5,
  cellsize = theRange_value,
  buffer = theRange_value/2,
  square = FALSE # Bloques hexagonales para mejor cobertura
)

print(spatial_folds)
autoplot(spatial_folds)

# Preparar índices para caret
index_list <- map(spatial_folds$splits, ~ .x$in_id)

# 3. MODELO GBM SIN TRANSFORMACIÓN LOGARÍTMICA ------------------------------

# Configuración de entrenamiento MODIFICADA
train_control <- trainControl(
  method = "cv",
  index = index_list,
  summaryFunction = function(data, lev = NULL, model = NULL) {
    c(MAE = mean(abs(data$obs - data$pred)),
      RMSE = sqrt(mean((data$obs - data$pred)^2)),
      MAPE = mean(abs((data$obs - data$pred)/data$obs), na.rm = TRUE)*100)
  },
  verboseIter = TRUE,
  savePredictions = "final",
  allowParallel = TRUE
)

# Grid de parámetros (puedes ajustarlo)
gbm_grid <- expand.grid(
  interaction.depth = c(5, 7, 9),
  n.trees = c(500, 750, 1000),
  shrinkage = c(0.01, 0.005),
  n.minobsinnode = c(10, 15)
)

# Entrenamiento paralelizado
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Modelo final SIN transformación logarítmica
gbm_model <- train(
  x = train %>% select(-c(property_id, price, lon, lat)),
  y = train$price,  # Usamos price directamente
  method = "gbm",
  distribution = "laplace",  # Mantenemos Laplace para robustez
  trControl = train_control,
  tuneGrid = gbm_grid,
  metric = "MAE",  # Optimizamos por MAE
  verbose = TRUE,
  bag.fraction = 0.8
)

stopCluster(cl)

# 4. EVALUACIÓN Y DIAGNÓSTICO ----------------------------------------------

# Resultados de validación cruzada
cv_results <- gbm_model$resample %>% 
  group_by(Resample) %>% 
  summarise(MAE = mean(MAE), RMSE = mean(RMSE), MAPE = mean(MAPE))

print(cv_results)

# Gráficos de diagnóstico
ggplot(gbm_model) + 
  geom_line(aes(y = MAE, color = as.factor(interaction.depth))) +
  facet_wrap(~ shrinkage, scales = "free_y") +
  labs(title = "Evolución del MAE por Hiperparámetros",
       color = "Profundidad") +
  theme_bw()

# Residuales directos (sin transformación)
train$pred <- predict(gbm_model, newdata = train)
train$residuals <- train$price - train$pred

# Gráfico de residuales vs valores reales
ggplot(train, aes(x = price, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Residuales vs Valores Reales",
       x = "Precio Real", y = "Residual") +
  theme_minimal()

# Mapa espacial de residuales
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
residual_limit <- quantile(abs(train$residuals), 0.99)

ggplot(train_sf) +
  geom_sf(aes(color = residuals), size = 1.2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0, 
                        limits = c(-residual_limit, residual_limit)) +
  ggtitle("Distribución Espacial de Residuales") +
  theme_minimal()

# 5. GENERACIÓN DE PREDICCIONES PARA KAGGLE --------------------------------

# Obtener mejores parámetros
best_params <- gbm_model$bestTune

# Función de predicción ajustada
predict_final_prices <- function(model, newdata) {
  pred <- predict(model, newdata = newdata)
  # Ajustar outliers conservadoramente
  q_low <- quantile(train$price, 0.01)
  q_high <- quantile(train$price, 0.99)
  pred <- pmin(pmax(pred, q_low), q_high)
  return(round(pred, 0))
}

# Generar predicciones
test_pred <- predict_final_prices(gbm_model, test)

# Preparar submission
submission <- data.frame(
  property_id = test$property_id,
  price = test_pred
)

# Crear nombre descriptivo del archivo con hiperparámetros (formato solicitado)
best_params_str <- paste0(
  "gbm_trees", best_params$n.trees,
  "_depth", best_params$interaction.depth,
  "_shrink", best_params$shrinkage,
  "_minobs", best_params$n.minobsinnode
)


# Guardar archivo
file_name <- paste0("Boosting_GB_price_",
                    best_params_str,
                    ".csv")

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



# 6 VALIDACIÓN CON DATOS DE TRAIN -----------------------------------------

# Calcular métricas de evaluación en escala original
mae_train <- mean(abs(train$price - train$pred))
rmse_train <- sqrt(mean((train$price - train$pred)^2))
mape_train <- mean(abs((train$price - train$pred)/train$price), na.rm = TRUE) * 100

# Resultados detallados
cat("\n=== MÉTRICAS DE VALIDACIÓN EN TRAIN ===\n")
cat(sprintf("MAE: %.2f\n", mae_train))
cat(sprintf("RMSE: %.2f\n", rmse_train))
cat(sprintf("MAPE: %.2f%%\n", mape_train))

# Análisis de residuales
residual_stats <- summary(train$residuals)
cat("\n=== ESTADÍSTICAS DE RESIDUALES ===\n")
print(residual_stats)

# Gráfico de densidad de residuales mejorado
ggplot(train, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgreen") +
  labs(title = "Distribución de Residuales en Train",
       subtitle = paste("MAE:", round(mae_train, 2), "| MAPE:", round(mape_train, 2), "%"),
       x = "Residuales (Precio Real - Predicción)", 
       y = "Densidad") +
  theme_minimal()

# Análisis por percentiles de precio
# Primero creamos la variable price_percentile
train <- train %>%
  mutate(price_percentile = cut(price, 
                                breaks = quantile(price, probs = seq(0, 1, 0.1)),
                                include.lowest = TRUE))

# Luego calculamos las métricas por percentil
error_by_percentile <- train %>%
  group_by(price_percentile) %>%
  summarise(
    n = n(),
    mean_price = mean(price),
    mean_pred = mean(pred),
    mae = mean(abs(residuals)),
    mape = mean(abs(residuals/price), na.rm = TRUE)*100,
    .groups = 'drop'
  )

# Gráfico de error por percentiles
percentile_plot <- ggplot(error_by_percentile, aes(x = price_percentile, y = mape)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", mape)), vjust = -0.5, size = 3) +
  labs(title = "Error Porcentual Absoluto Medio por Percentil de Precio",
       x = "Percentil de Precio", 
       y = "MAPE (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(percentile_plot)

# Gráfico Q-Q de residuales
qq_plot <- ggplot(train, aes(sample = residuals)) +
  stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Gráfico Q-Q de Residuales",
       subtitle = "Evaluación de normalidad") +
  theme_minimal()

print(qq_plot)

# Mapa de residuales mejorado
residual_breaks <- quantile(train$residuals, probs = seq(0, 1, 0.1))
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

residual_map <- ggplot(train_sf) +
  geom_sf(aes(color = residuals), size = 1, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0,
                        breaks = residual_breaks,
                        labels = scales::comma) +
  labs(title = "Distribución Espacial de Residuales",
       subtitle = paste("MAE:", round(mae_train, 2), "| MAPE:", round(mape_train, 2), "%"),
       color = "Residuales") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(residual_map)