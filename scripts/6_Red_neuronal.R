#-----------------------------------------------------------------------------//
# Red neuronal 
# Problem Set 4 G10 - BDML 202501
# Fecha: 3 de mayo 2025
#-----------------------------------------------------------------------------//

train =  readRDS(file.path(stores_path, "train_data.rds"))
train = as.data.frame(train)
test =  readRDS(file.path(stores_path, "test_data.rds"))
test = as.data.frame(test)

# Ecuación de regresión
formula <- stats::as.formula(
            paste("price ~ type_housing + seguridad + surface_total + bathrooms +
                  gym + balcon + piscina"))

# Preprocesamiento 
recipe_nnet = recipes::recipe(formula, train) %>%
              recipes::step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
              recipes::step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
              recipes::step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
              recipes::step_normalize(all_predictors())  # normaliza los predictores. 

# Especificar el modelo 
nnet_base = 
          parsnip::mlp(
            hidden_units = 6, # neuronas
            epochs = 100,     # número de veces que se pasa la data por las neuronas
            engine = 'nnet'   # valor por defecto
            ) %>% 
          parsnip::set_mode("regression") %>% 
          parsnip::set_engine("nnet")
nnet_base

workflow_base = workflows::workflow() %>% 
                workflows::add_recipe(recipe_nnet) %>%
                workflows::add_model(nnet_base) 

# Entrenamiento del modelo
base_final_fit = fit(workflow_base, data = train)


# Desempeño en la muestra
broom::augment(base_final_fit, new_data = train) %>%
  mae(truth = price, estimate = .pred)


# Obtener predicciones
preds <- broom::augment(base_final_fit, new_data = train)

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

