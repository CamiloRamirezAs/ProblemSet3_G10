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
nrow(test)

test = test %>% select(-c(lon, lat, city, description_adj, month, year, piscina)) 

x_test <- scale(model.matrix( ~ . - property_id - 1, data = test))
x_test <- model.matrix( ~ . - property_id - 1, data = test) %>% scale()

nrow(x_test)

#-----------------------------------------------------------------------------//
rownames(train) <- train$property_id
colnames(train)
train = train %>% select(-c(lon, lat, city, geometry, description_adj, month, year, piscina)) 
x <- scale(model.matrix(price ~ . - property_id - 1, data = train))
x <- model.matrix(price ~ . - property_id - 1, data = train) %>% scale()
y = train$price
#-----------------------------------------------//

modnn = keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 1) 

colnames(x)

modnn %>% compile(
  loss = "mean_absolute_error", 
  optimizer = optimizer_adam(learning_rate = 0.01), # una tasa para que vaya más despacio en los saltos de la funcion
  metrics = c("mean_absolute_error")
)

early_stop <- callback_early_stopping(
  monitor = "val_mean_absolute_error", 
  patience = 20,         # número de épocas sin mejora antes de detener
  restore_best_weights = TRUE 
)

history <- modnn %>% fit(
  x, y,
  epochs = 400, 
  batch_size = 32, 
  validation_split = 0.2,
  callbacks = list(early_stop))

npred <- predict(modnn, x)
npred_vec <- as.vector(npred)
mae <- mean(abs(y - npred_vec))
print(mae)

options(scipen = 999)  # Desactiva notación científica

hist(train$price)
summary(train$price)
summary(npred)


prueba = data.frame(property_id = train$property_id,
                    predicho = predict(modnn, x), 
                    observado = train$price)


ggplot(prueba, aes(x = observado)) +
  geom_density(aes(fill = "Observado"), alpha = 0.5, color = NA) +
  geom_density(aes(x = predicho, fill = "Predicho"), alpha = 0.2, color = NA) +
  
  labs(title = "Densidad de valores observados vs. predichos",
       x = "Precio",
       y = "Densidad",
       fill = "Valores") +
  scale_fill_manual(values = c("Observado" = "gray", "Predicho" = "blue")) +
  theme_classic()


#-----------------------------------------------------------------------------//
# Guardar submit
#-----------------------------------------------------------------------------//


npred_test <- predict(modnn, x_test)

submit = data.frame(property_id = test$property_id, 
                    price = predict(modnn, x_test) )


head(rownames(x_test)) == head(as.character(test$property_id))
head(submit)
dim(submit)
any(is.na(submit$price))  # Debe ser FALSE

# rownames(submit) <- submit$property_id

write.csv(submit, file.path(stores_path, "submit3_NN.csv"), row.names = FALSE)


