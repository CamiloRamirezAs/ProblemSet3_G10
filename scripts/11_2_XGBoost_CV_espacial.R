#-----------------------------------------------------------------------------//
# Modelo XGBoost con validación cruzada espacial
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 23 de mayo de 2025
#-----------------------------------------------------------------------------//


# 1. IMPORTAR DATOS ------------------------------------------------------------

# Bases iniciales
train_A <- readRDS(file.path(stores_path, "train_data.rds"))
test_A  <- readRDS(file.path(stores_path, "test_data.rds"))

train_A <- as.data.frame(train_A)
test_A  <- as.data.frame(test_A)

# Eliminar geometría
train_A <- train_A %>% select(-geometry)

# Bases espaciales
train_B <- readRDS(file.path(stores_path, "spatial_train.rds"))
test_B  <- readRDS(file.path(stores_path, "spatial_test.rds"))

# Join
train_raw <- train_A %>% left_join(train_B, by = "property_id")
test_raw  <- test_A  %>% left_join(test_B, by = "property_id")

# Variables seleccionadas
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

# Crear objetos sf
train_sf <- st_as_sf(train_raw, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
test_sf  <- st_as_sf(test_raw, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Fórmula del modelo
model_formula <- price ~ surface_total + bedrooms + bathrooms + apto_casa +
  piscina + garaje + seguridad + balcon + gym + year +
  dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
  dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
  num_restaurantes

model_formula_rhs <- as.formula("~ surface_total + bedrooms + bathrooms + apto_casa +
  piscina + garaje + seguridad + balcon + gym + year +
  dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
  dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
  num_restaurantes")

property_ids_test <- test_raw$property_id

X_train <- model.matrix(model_formula, data = train_sf)[, -1]
Y_train <- train_sf$price

X_test <- model.matrix(model_formula_rhs, data = test_sf)[, -1]


# 3. VALIDACIÓN CRUZADA ESPACIAL -----------------------------------------------

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)
folds_SL <- lapply(block_folds$splits, function(split) {
  analysis_indices <- analysis(split) %>% as.data.frame() %>% rownames() %>% as.integer()
  intersect(analysis_indices, 1:nrow(train_sf))
})

autoplot(block_folds)

index_caret <- lapply(folds_SL, function(idx) idx)

fiveStats <- function(data, lev = NULL, model = NULL) {
  mae <- Metrics::mae(data$obs, data$pred)
  rmse <- Metrics::rmse(data$obs, data$pred)
  r2 <- cor(data$obs, data$pred)^2
  med_error <- median(abs(data$obs - data$pred))
  c(MAE = mae, RMSE = rmse, Rsquared = r2, MedianError = med_error)
}

train_control_spatial <- trainControl(
  method = "cv",
  index = index_caret,
  verboseIter = TRUE,
  summaryFunction = fiveStats,
  selectionFunction = "best",
  savePredictions = "final",
  returnResamp = "all"
)

# 4. ENTRENAMIENTO CON XGBOOST -------------------------------------------------

xgb_grid <- expand.grid(
  nrounds = 500,
  max_depth = 7,
  eta = 0.05,
  gamma = 0,
  min_child_weight = 3,
  colsample_bytree = 0.8,
  subsample = 1
)

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

best_params <- xgb_tune$bestTune
print(best_params)


# 5. EVALUACIÓN EN TRAIN -------------------------------------------------------

train_pred <- predict(xgb_tune, newdata = X_train)
train_metrics <- data.frame(
  MAE = Metrics::mae(Y_train, train_pred),
  RMSE = Metrics::rmse(Y_train, train_pred),
  R2 = cor(Y_train, train_pred)^2,
  MedianError = median(abs(Y_train - train_pred))
)

print("Performance en datos de entrenamiento:")
print(train_metrics)


# 6. PREDICCIÓN Y SUBMISIÓN ----------------------------------------------------

test_pred <- predict(xgb_tune, newdata = X_test)

submission <- tibble(
  property_id = property_ids_test,
  price = round(test_pred)
)

# Crear nombre con hiperparámetros
submission_name <- paste0(
  "XGBoost_CV_",
  "nrounds", best_params$nrounds, "_",
  "depth", best_params$max_depth, "_",
  "eta", gsub("\\.", "", as.character(best_params$eta)), "_",
  "gamma", gsub("\\.", "", as.character(best_params$gamma)), "_",
  "minchild", best_params$min_child_weight, "_",
  "colsample", gsub("\\.", "", as.character(best_params$colsample_bytree)),
  ".csv"
)

write_csv(submission, file.path(submission_path, submission_name))
cat("Archivo de submission guardado en:", file.path(submission_path, submission_name), "\n")


