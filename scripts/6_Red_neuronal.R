#-----------------------------------------------------------------------------//
# Red neuronal 
# Problem Set 4 G10 - BDML 202501
# Fecha: 3 de mayo 2025
#-----------------------------------------------------------------------------//

train =  readRDS(file.path(stores_path, "train_data.rds"))
train = as.data.frame(train) 
test =  readRDS(file.path(stores_path, "test_data.rds"))
test = as.data.frame(test)

#-----------------------------------------------------------------------------//
# 
#-----------------------------------------------------------------------------//

recipe_nnet = recipe(price ~ type_housing + seguridad + surface_total + 
                       bathrooms + gym + balcon, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

recipe_nnet$ptype


#-----------------------------------------------------------------------------//





























skim(train)
table(train$garaje)



reg_lin = lm(price ~ type_housing + seguridad + surface_total + 
               bathrooms + gym + balcon, data = train)
summary(reg_lin)


submit = data.frame(
  property_id = test$property_id,
  price = predict(reg_lin, newdata = test)
)
head(submit)
write.csv(submit, file.path(stores_path, "submit_lm.csv"), row.names = FALSE)

pred_test <- predict(reg_lin, newdata = test)


submit = pred_df %>% select(property_id, precio_predicho) %>% rename(price = precio_predicho)




# Si quieres ver los primeros valores:
head(pred_test)


broom::augment(reg_lin, new_data = train) %>%
  mae(truth = price, estimate = .fitted)

broom::augment(reg_lin, new_data = test) 
head(test)
pred_df = data.frame(
           property_id = train$property_id,
           precio_observado = train$price,
           precio_predicho = reg_lin$fitted.values,
           precio_predicho_test = predict(reg_lin, newdata = test)
          )

head(pred_df)
broom::augment(reg_lin, new_data = train)



submit = pred_df %>% select(property_id, precio_predicho) %>% rename(price = precio_predicho)
head(submit)













nn <- nnet::nnet(price ~ type_housing + seguridad + surface_total + 
                   bathrooms + gym + balcon, 
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

