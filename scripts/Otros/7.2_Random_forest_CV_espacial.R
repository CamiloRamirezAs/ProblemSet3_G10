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


# 1. PREPARACIÓN DE DATOS ESPACIALES -------------------------------------------
# Convertir a objeto espacial
train_sf <- st_as_sf(train_raw, coords = c("lon", "lat"), crs = 4326)

# Calcular UTM para trabajo en metros
mean_lon <- mean(st_coordinates(train_sf)[,1])
utm_zone <- floor((mean_lon + 180)/6) + 1
utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
train_utm <- st_transform(train_sf, crs = utm_crs)

# 2. ANÁLISIS ESPACIAL --------------------------------------------------------
# Calcular variograma
variog <- variogram(price ~ 1, as_Spatial(train_utm))
plot(variog, main = "Variograma Experimental")
theRange_value <- 1200  # Ajustar según punto de estabilización

# 3. CREACIÓN DE FOLDS ESPACIALES ---------------------------------------------
set.seed(123)
spatial_folds <- cv_spatial(
  x = train_utm,
  k = 5,
  size = theRange_value,
  selection = "random",
  iteration = 100,
  progress = TRUE
)

# Verificar estructura de los folds
print(names(spatial_folds))
str(spatial_folds$folds_ids)

# 4. CONFIGURACIÓN DEL ENTRENAMIENTO ------------------------------------------
index_list <- list()
indexOut_list <- list()

for (i in seq_along(spatial_folds$folds)) {
  test_indices <- spatial_folds$folds[[i]]          # Índices de test
  train_indices <- setdiff(seq_len(nrow(train_raw)), test_indices)  # Índices de train
  index_list[[paste0("Fold", i)]] <- train_indices
  indexOut_list[[paste0("Fold", i)]] <- test_indices
}

ctrl_spatial <- trainControl(
  method = "cv",
  index = index_list,
  indexOut = indexOut_list,
  savePredictions = "final",
  verboseIter = TRUE,
  allowParallel = TRUE,
  summaryFunction = function(data, lev = NULL, model = NULL) {
    c(
      MAE = mean(abs(data$obs - data$pred)),
      RMSE = sqrt(mean((data$obs - data$pred)^2)),
      MAPE = mean(abs((data$obs - data$pred)/data$obs), na.rm = TRUE)*100
    )
  }
)

# 5. ENTRENAMIENTO DEL MODELO -------------------------------------------------
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123)
rf_spatial <- train(
  price ~ surface_total + bedrooms + bathrooms + type_housing +
    piscina + garaje + seguridad + balcon + gym + year +
    dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
    dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
    num_restaurantes,
  data = train_raw,  # Usando train_raw directamente
  method = "ranger",
  trControl = ctrl_spatial,
  tuneGrid = expand.grid(
    mtry = c(4, 6, 8),
    splitrule = "variance",
    min.node.size = c(5, 10, 15)
  ),
  metric = "MAE",
  num.trees = 1000,
  importance = "permutation"
)

stopCluster(cl)

# 6. EVALUACIÓN DEL MODELO ----------------------------------------------------
# Resultados
print(rf_spatial)
plot(rf_spatial)

# Importancia de variables
var_imp <- vip(rf_spatial, num_features = 15)
print(var_imp)

# Predicciones en train_raw
train_raw$pred <- predict(rf_spatial, train_raw)
train_raw$residuals <- train_raw$price - train_raw$pred

# 7. PREDICCIÓN FINAL EN TEST_RAW ---------------------------------------------
test_raw$pred_price <- predict(rf_spatial, test_raw)

# Preparar submission
submission <- data.frame(
  property_id = test_raw$property_id,
  price = test_raw$pred_price
)

# Guardar resultados
write.csv(submission, "predicciones_finales.csv", row.names = FALSE)

# 8. VISUALIZACIONES FINALES --------------------------------------------------
# Mapa de residuales
ggplot(st_as_sf(train_raw, crs = 4326)) +
  geom_sf(aes(color = residuals), size = 1, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0) +
  labs(title = "Distribución Espacial de Residuales")

# Gráfico de valores reales vs predichos
ggplot(train_raw, aes(x = price, y = pred)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Precio Real", y = "Precio Predicho")