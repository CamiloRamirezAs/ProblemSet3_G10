#-----------------------------------------------------------------------------//
# Red neuronal 
# Problem Set 4 G10 - BDML 202501
# Fecha: 3 de mayo 2025
#-----------------------------------------------------------------------------//

train_data =  readRDS(file.path(stores_path, "train_data.rds"))
train_spatial =  readRDS(file.path(stores_path, "spatial_train.rds"))
train = merge(train_data, train_spatial, by = c("property_id"), all = T)
train = as.data.frame(train) 

#-----------------------------------------------------------------------------//

test_data =  readRDS(file.path(stores_path, "test_data.rds"))
test_spatial =  readRDS(file.path(stores_path, "spatial_test.rds"))
test = merge(test_data, test_spatial, by = c("property_id"), all = T)
test = as.data.frame(test)

rownames(test) <- test$property_id
colnames(test)

test = test %>% select(-c(city, description_adj, month, year)) 

#-----------------------------------------------------------------------------//

x_test <- scale(model.matrix( ~ . - property_id - 1, data = test))
x_test <- model.matrix( ~ . - property_id - 1, data = test) %>% scale()

#-----------------------------------------------------------------------------//

rownames(train) <- train$property_id
colnames(train)
#train = train %>% select(-c(lon, lat, city, geometry, description_adj, month, year)) 
x <- scale(model.matrix(price ~ . - property_id - 1, data = train))
x <- model.matrix(price ~ . - property_id - 1, data = train) %>% scale()
y = train$price



#-----------------------------------------------------------------------------//
library(sf)
library(dplyr)
library(blockCV)
install.packages("blockCV")


# Suponiendo que tienes `train` como un `data.frame` con coordenadas lon y lat
# Convierte a objeto espacial
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# Usa blockCV para crear folds espaciales
set.seed(123)
spatial_folds <- spatialBlock(
  speciesData = train_sf,
  theRange = 5000,
  k = 5,
  selection = "random",
  iteration = 100,
  showBlocks = TRUE
)

fold_ids <- spatial_folds$foldID

results <- c()

for (i in 1:5) {
  test_idx <- which(fold_ids == i)
  train_idx <- setdiff(1:nrow(train), test_idx)
  
  x_train <- x[train_idx, ]
  y_train <- y[train_idx]
  x_test <- x[test_idx, ]
  y_test <- y[test_idx]
  
  # Modelo
  modnn <- keras_model_sequential() %>%
    layer_dense(units = 10, activation = "relu", input_shape = ncol(x)) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 10, activation = "relu") %>%
    layer_dense(units = 1)
  
  modnn %>% compile(
    loss = "mean_absolute_error",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = "mean_absolute_error"
  )
  
  early_stop <- callback_early_stopping(
    monitor = "val_mean_absolute_error",
    patience = 20,
    restore_best_weights = TRUE
  )
  
  history <- modnn %>% fit(
    x_train, y_train,
    epochs = 400,
    batch_size = 32,
    validation_split = 0.2,
    callbacks = list(early_stop),
    verbose = 0
  )
  
  pred <- predict(modnn, x_test) %>% as.vector()
  mae <- mean(abs(y_test - pred))
  results <- c(results, mae)
  
  print(paste0("fold", i))
}

mean(results)

