
# Bases iniciales
train_A <- readRDS(file.path(stores_path, "train_data.rds"))
test_A  <- readRDS(file.path(stores_path, "test_data.rds"))

train_A <- as.data.frame(train_A)
test_A <- as.data.frame(test_A)

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

nrow(test_raw)/nrow(train_raw) ##Entrenamos con el 40% de los datos

#Entrenar Elastic Net
elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Crear la grilla
lambda <- 10^seq(2, -6, length.out = 100)
alpha <- seq(0, 1, by = 0.15)

grid_values <- expand_grid(
  penalty = lambda,
  mixture = alpha
)

# Primera receta
rec_1 <- recipe(
  price ~ surface_total + bedrooms + bathrooms + type_housing +
    piscina + garaje + seguridad + balcon + gym +
    dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
    dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
    num_restaurantes,
  data = train_raw
) %>%
  step_interact(terms = ~  dist_transmi:ESTRATO + AVALUO_COM:ESTRATO) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Segunda receta 
rec_2 <- recipe(
  price ~ surface_total + bedrooms + bathrooms + type_housing +
    piscina + garaje + seguridad + balcon + gym +
    dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
    dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
    num_restaurantes,
  data = train_raw
) %>%
  step_interact(terms = ~ dist_parque:ESTRATO) %>% 
  step_interact(terms = ~ AVALUO_COM:ESTRATO) %>% 
  step_interact(terms = ~  dist_transmi:ESTRATO) %>%
  step_poly(surface_total, AVALUO_COM, dist_parque, dist_centComercial, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

#Flujo de trabajo
workflow_1 <- workflow() %>% 
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)

workflow_2 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(elastic_net_spec)

#Validacion espocial para tratar autocorrelación espacial
train_sf <- st_as_sf(
  train_raw,
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(1234)
block_folds <- spatial_block_cv(train_sf, v = 5)
block_folds
autoplot(block_folds)

#Hiperparametros
set.seed(1234)

tune_res1 <- tune_grid(
  workflow_1,        
  resamples = block_folds,  
  grid = grid_values,        
  metrics = metric_set(mae) 
)

workflowsets::collect_metrics(tune_res1)

# Parametros escogidos por rec1
best_tune_res1 <- select_best(tune_res1, metric = "mae")
best_tune_res1

set.seed(1234)

tune_res2 <- tune_grid(
  workflow_2,         
  resamples = block_folds,  
  grid = grid_values,        
  metrics = metric_set(mae) 
)

workflowsets::collect_metrics(tune_res2)

# Parametros escogidos por rec2
best_tune_res2<- select_best(tune_res2, metric = "mae")
best_tune_res2

# Finalizar el flujo de trabajo con el mejor valor de parametros
res1_final <- finalize_workflow(workflow_1, best_tune_res1)
res2_final <- finalize_workflow(workflow_2, best_tune_res2)

# Ajustar el modelo  utilizando los datos de entrenamiento
EN_final1 <- fit(res1_final, data = train_raw)
EN_final2 <- fit(res2_final, data = train_raw)

# Predicciones sobre los datos de test 
predictiones_1 <- test_raw %>% 
  select(property_id) %>% 
  bind_cols(predict(EN_final1, new_data = test_raw)) %>%
  rename(price = .pred)

predictiones_2 <- test_raw %>% 
  select(property_id) %>% 
  bind_cols(predict(EN_final2, new_data = test_raw)) %>%
  rename(price = .pred)

#Revision MAE sobre train
predictiones_train <- predict(EN_final1, new_data = train_raw)
mae_1 <- train_raw %>%
  bind_cols(predictiones_train ) %>%
  yardstick::mae(truth = price, estimate = .pred)
mae_1[[".estimate"]]


predictiones_train <- predict(EN_final2, new_data = train_raw)
mae_1 <- train_raw %>%
  bind_cols(predictiones_train ) %>%
  yardstick::mae(truth = price, estimate = .pred)
mae_1[[".estimate"]]

#Guardar CSV
write.csv(predictiones_2, file.path(submission_path, "EN_lambda_0_000001_alpha_0_75.csv"), row.names = FALSE)


