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
test_spatial =  readRDS(file.path(stores_path, "test_spatial.rds"))
test = merge(test_data, test_spatial, by = c("property_id"), all = T)
test = as.data.frame(test)

rownames(test) <- test$property_id
colnames(test)

test = test %>% select(-c(lon, lat, city, description_adj, month, year)) 

x_test <- scale(model.matrix( ~ . - property_id - 1, data = test))
x_test <- model.matrix( ~ . - property_id - 1, data = test) %>% scale()


#-----------------------------------------------------------------------------//
rownames(train) <- train$property_id
colnames(train)
train = train %>% select(-c(lon, lat, city, geometry, description_adj, month, year)) 
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
  optimizer = optimizer_adam(learning_rate = 0.001), # una tasa para que vaya más despacio en los saltos de la funcion
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

write.csv(submit, file.path(stores_path, "submit2_NN.csv"), row.names = FALSE)










#-----------------------------------------------------------------------------//





















































#-----------------------------------------------------------------------------//
# 
#-----------------------------------------------------------------------------//
# sapply(train, function(x) if (is.character(x) || is.factor(x)) length(unique(x)))

train = train %>% 
        #select(-c(geometry, city)) %>% 
        mutate(apto = ifelse(type_housing == "Apartamento", 1, 0), 
               casa = ifelse(type_housing == "Casa", 1, 0))

table(train$piscina)

formula = as.formula(paste("price ~ apto + seguridad + surface_total + 
                       bathrooms + gym + balcon + piscina"))

recipe_nnet = recipe(formula,
                     data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

recipe_nnet$ptype

#-----------------------------------------------------------------------------//
# Red neuronal con Keras
#-----------------------------------------------------------------------------//

snn = keras_model_sequential() %>%
      layer_dense(units = 6, 
                  activation = "relu",
                  input_shape = ncol(X)) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 1, name = "capa_salida")
summary(snn)

X = as.matrix(train %>% select(apto, seguridad, surface_total, bathrooms,
                               gym, balcon, piscina))
y = as.matrix(train  %>% select(price))

snn %>% compile(
  loss = "mse",
  optimizer = 'sgd',
  metrics = list("mean_squared_error", "mean_absolute_error")
)

history <- snn %>% fit(
  X, y, 
  epochs =50,
  batch_size = 2^(ncol(X)) # 
  #validation_split = 0.2   # Reserva el 20% de los datos de entrenamiento para validación
)


npred <- predict(snn, X)


#-----------------------------------------------------------------------------//



snn %>% compile(
        loss = "mse",
        optimizer = 'sgd',
        metrics = list("mean_squared_error", "mean_absolute_error")
      )





head(X)

set.seed(10101)
history <- snn %>% fit(
  X, y, 
  epochs = 50,
  batch_size = 2^(ncol(X)*2) # 
  #validation_split = 0.2   # Reserva el 20% de los datos de entrenamiento para validación
)


prob_predictions <- snn %>% 
  predict(as.matrix(X), batch_size = 2^(ncol(X)))
head(prob_predictions, 5 )


ncol(X)


# Predicciones sobre los datos de entrenamiento
predicciones_train <- as.data.frame(predict(snn, X))

# Ver las primeras 10 predicciones
head(predicciones_train, 10)





#-----------------------------------------------------------------------------//

workflow_base <- workflows::workflow() %>% 
  workflows::add_recipe(recipe_nnet) %>%
  workflows::add_model(snn) 



### definir validación cruzada tradicional 

# Creamos folds de validación cruzada tradicional
set.seed(86936)
cv_folds <- vfold_cv(train, v = 5)

# Visualizamos la estructura de los folds
cv_folds
















nn <- nnet::nnet(formula, 
                 data = train,
                 size = 3, 
                 linout=TRUE
) 

yhat_nn <- predict(nn, newdata = train)

# Especificar el modelo 
nnet_base = 
          parsnip::mlp(
            hidden_units = 10, # neuronas
            epochs = 100,     # número de veces que se pasa la data por las neuronas
            engine = 'nnet'   # valor por defecto
            ) %>% 
          parsnip::set_mode("regression") %>% 
          parsnip::set_engine("nnet")


workflow_base = workflows::workflow() %>% 
                workflows::add_recipe(recipe_nnet) %>%
                workflows::add_model(nnet_base) 

# Entrenamiento del modelo
base_final_fit = fit(workflow_base, data = train)

resultados = data.frame(
  Obsr = seq_along(train$property_id),
  Observado = train$price,  
  Predicho = predict(base_final_fit, train))


predicciones <- predict(base_final_fit, new_data = train) %>% 
  bind_cols(train %>% select(price))  # Combina con valores reales

head(predicciones)



# Desempeño en la muestra
broom::augment(base_final_fit, new_data = train) %>%
  mae(truth = price, estimate = .pred)


# Obtener predicciones
preds <- broom::augment(base_final_fit, new_data = train)

submit = preds %>% select(property_id, .pred) %>% rename(price = .pred)



# Gráfico
ggplot(preds, aes(x = price)) +
  geom_density(aes(fill = "Observado"), alpha = 0.5, color = NA) +
  geom_density(aes(x = .pred, fill = "Predicho"), alpha = 0.2, color = NA) +

  labs(title = "Densidad de valores observados vs. predichos",
       x = "Precio",
       y = "Densidad",
       fill = "Valores") +
  scale_fill_manual(values = c("Observado" = "gray", "Predicho" = "blue")) +
  theme_classic()

