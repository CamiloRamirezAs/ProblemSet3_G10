#-----------------------------------------------------------------------------//
# Modelo XGBoost sin validación cruzada espacial
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

# Fórmula completa
model_formula <- price ~ surface_total + bedrooms + bathrooms + apto_casa +
  piscina + garaje + seguridad + balcon + gym + year +
  dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
  dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
  num_restaurantes

# Matrices
X_train <- model.matrix(model_formula, data = train_raw)[, -1]
Y_train <- train_raw$price

X_test <- model.matrix(~ surface_total + bedrooms + bathrooms + apto_casa +
                         piscina + garaje + seguridad + balcon + gym + year +
                         dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
                         dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
                         num_restaurantes, data = test_raw)[, -1]

# 2. ENTRENAMIENTO INICIAL -----------------------------------------------------

set.seed(123)
ctrl <- trainControl(
  method = "cv", 
  number = 5, 
  summaryFunction = defaultSummary, 
  classProbs = TRUE,
  savePredictions = "all", 
  verboseIter = TRUE
)

xgb_base <- train(
  x = X_train,
  y = Y_train,
  method = "xgbTree",
  trControl = ctrl,
  metric = "MAE",
  tuneLength = 3 # búsqueda rápida inicial
)

# 3. IMPORTANCIA DE VARIABLES --------------------------------------------------

vip <- varImp(xgb_base, scale = TRUE)
vip_df <- vip$importance %>% 
  rownames_to_column("variable") %>%
  arrange(desc(Overall))

top_vars <- vip_df %>% slice_head(n = 12) %>% pull(variable)  # seleccionamos 12 más importantes

cat("Variables seleccionadas:\n")
print(top_vars)

# 4. ENTRENAR MODELO FINAL CON VARIABLES SELECCIONADAS -------------------------

X_train_sel <- X_train[, top_vars]
X_test_sel  <- X_test[, top_vars]

# Nueva validación cruzada más precisa
xgb_grid <- expand.grid(
  nrounds = c(250, 500),
  max_depth = c(3, 5),
  eta = c(0.01, 0.05),
  gamma = c(0, 0.1),
  min_child_weight = 1,
  colsample_bytree = 0.8,
  subsample = 1
)

xgb_grid

set.seed(456)
xgb_final <- train(
  x = X_train_sel,
  y = Y_train,
  method = "xgbTree",
  trControl = ctrl,
  metric = "MAE",
  tuneGrid = xgb_grid
)

best_params <- xgb_final$bestTune
print(best_params)

# 5. EVALUACIÓN EN TRAIN -------------------------------------------------------

train_pred <- predict(xgb_final, newdata = X_train_sel)
train_metrics <- data.frame(
  MAE = Metrics::mae(Y_train, train_pred),
  RMSE = Metrics::rmse(Y_train, train_pred),
  R2 = cor(Y_train, train_pred)^2,
  MedianError = median(abs(Y_train - train_pred))
)

cat("Performance en datos de entrenamiento:\n")
print(train_metrics)

# 6. PREDICCIÓN Y SUBMISIÓN ----------------------------------------------------

test_pred <- predict(xgb_final, newdata = X_test_sel)

submission <- tibble(
  property_id = test_raw$property_id,
  price = round(test_pred)
)

submission_name <- paste0(
  "XGBoost_", 
  paste(top_vars[1:3], collapse = "_"), "_",  # 3 variables más importantes
  "nrounds", best_params$nrounds, "_",
  "depth", best_params$max_depth, "_",
  "eta", gsub("\\.", "", as.character(best_params$eta)), 
  "gamma", gsub("\\.", "", as.character(best_params$gamma)), "_",
  "minchild", best_params$min_child_weight, "_",
  "colsample", gsub("\\.", "", as.character(best_params$colsample_bytree)),".csv"
)

write_csv(submission, file.path(submission_path, submission_name))
cat("Archivo de submission guardado en:", file.path(submission_path, submission_name), "\n")
