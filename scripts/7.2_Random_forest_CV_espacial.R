#-----------------------------------------------------------------------------//
# Modelo Random Forest
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 21 de mayo de 2025
#-----------------------------------------------------------------------------//



# 1. IMPORTAR Y PREPARAR DATOS ------------------------------------------------

# Cargar y preparar datos
train_A <- readRDS(file.path(stores_path, "train_data.rds")) %>% 
  as.data.frame() %>% 
  select(-geometry)

test_A <- readRDS(file.path(stores_path, "test_data.rds")) %>% 
  as.data.frame()

train_B <- readRDS(file.path(stores_path, "spatial_train.rds"))
test_B <- readRDS(file.path(stores_path, "spatial_test.rds"))

# Unir datasets y preprocesar
train_raw <- train_A %>%
  left_join(train_B, by = "property_id") %>%
  mutate(
    type_housing = factor(type_housing, levels = c("Casa", "Apartamento")),
    price = as.numeric(as.character(price)),
    across(c(piscina, garaje, seguridad, balcon, gym), as.factor)
  ) %>% 
  filter(!is.na(price))

test_raw <- test_A %>%
  left_join(test_B, by = "property_id") %>%
  mutate(
    type_housing = factor(type_housing, levels = c("Casa", "Apartamento")),
    across(c(piscina, garaje, seguridad, balcon, gym), as.factor)
  )

# 2. PREPARACIÓN ESPACIAL -----------------------------------------------------
train_sf <- st_as_sf(train_raw, coords = c("lon", "lat"), crs = 4326)
mean_lon <- mean(st_coordinates(train_sf)[,1])
utm_zone <- floor((mean_lon + 180)/6) + 1
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
train_utm <- st_transform(train_sf, crs = utm_crs)

# 3. ANÁLISIS ESPACIAL Y CREACIÓN DE FOLDS ------------------------------------
# Crear categorías de precio para estratificación
train_utm$price_cat <- cut(train_utm$price, 
                           breaks = quantile(train_utm$price, probs = seq(0, 1, 0.2)),
                           include.lowest = TRUE)

# Calcular variograma
variog <- variogram(price ~ 1, as_Spatial(train_utm))
plot(variog, main = "Variograma Experimental")
theRange_value <- 1200 # Ajustar según punto de estabilización

# Crear folds espaciales estratificados
set.seed(123)
spatial_folds <- cv_spatial(
  x = train_utm,
  column = "price_cat", # Estratificar por categorías de precio
  k = 5,
  size = theRange_value,
  selection = "random",
  iteration = 100,
  progress = TRUE
)

# Visualización interactiva
plot(spatial_folds)

# 4. CONFIGURACIÓN DEL MODELO -------------------------------------------------
tuneGrid_spatial <- expand.grid(
  mtry = c(4, 6, 8),          # Número de variables a considerar
  splitrule = "variance",     # Para regresión
  min.node.size = c(5, 10, 15) # Tamaño mínimo de nodos
)

ctrl_spatial <- trainControl(
  method = "cv",
  index = spatial_folds$folds_ids$train,
  indexOut = spatial_folds$folds_ids$test,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE,
  summaryFunction = function(data, lev = NULL, model = NULL) {
    c(MAE = mean(abs(data$obs - data$pred)),
      RMSE = sqrt(mean((data$obs - data$pred)^2)),
      MAPE = mean(abs((data$obs - data$pred)/data$obs), na.rm = TRUE)*100)
  }
)

# 5. ENTRENAMIENTO PARALELIZADO -----------------------------------------------
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123)
rf_spatial <- train(
  price ~ surface_total + bedrooms + bathrooms + type_housing +
    piscina + garaje + seguridad + balcon + gym + year +
    dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
    dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
    num_restaurantes,
  data = as.data.frame(train_raw), # Asegurar que es dataframe
  method = "ranger",
  trControl = ctrl_spatial,
  tuneGrid = tuneGrid_spatial,
  metric = "MAE",
  num.trees = 1000,
  importance = "permutation"
)

stopCluster(cl)

# 6. EVALUACIÓN Y DIAGNÓSTICO -------------------------------------------------
# Resultados del modelo
print(rf_spatial)
plot(rf_spatial)

# Importancia de variables
var_imp <- vip(rf_spatial, num_features = 20, geom = "point") + 
  theme_minimal()

# Predicciones y residuales
train_raw$pred <- predict(rf_spatial, train_raw)
train_raw$residuals <- train_raw$price - train_raw$pred

# Gráficos de diagnóstico
p1 <- ggplot(train_raw, aes(x = price, y = pred)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicciones vs Valores Reales", x = "Precio Real", y = "Predicción")

p2 <- ggplot(train_raw, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribución de Residuales")

p3 <- ggplot(st_as_sf(train_raw), aes(color = residuals)) +
  geom_sf(size = 0.8, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Residuales Espaciales")

# Combinar gráficos
(p1 + p2) / p3 + plot_layout(heights = c(1, 2))

# 7. VALIDACIÓN Y PREDICCIÓN FINAL --------------------------------------------
# Dividir datos para validación (si no hay conjunto de validación separado)
set.seed(123)
val_index <- createDataPartition(train_raw$price, p = 0.2, list = FALSE)
validation <- train_raw[val_index, ]

# Métricas de validación
val_pred <- predict(rf_spatial, validation)
val_metrics <- data.frame(
  MAE = mean(abs(validation$price - val_pred)),
  RMSE = sqrt(mean((validation$price - val_pred)^2)),
  MAPE = mean(abs((validation$price - val_pred)/validation$price), na.rm = TRUE)*100
)

print("Métricas de Validación:")
print(val_metrics)

# Predicción final para Kaggle
test_pred <- predict(rf_spatial, test_raw)

submission <- data.frame(
  property_id = test_raw$property_id,
  price = test_pred
)

# Nombre del archivo con metadatos del modelo
file_name <- sprintf("RF_SpatialCV_mae%.0f_mtry%d_nodesize%d.csv",
                     min(rf_spatial$results$MAE),
                     rf_spatial$bestTune$mtry,
                     rf_spatial$bestTune$min.node.size)

write.csv(submission, file.path(submission_path, file_name), row.names = FALSE)

