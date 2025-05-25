#-----------------------------------------------------------------------------//
# Regresión lineal
# Problem Set 4 G10 - BDML 202501
# Fecha: 3 de mayo 2025
#-----------------------------------------------------------------------------//


train =  readRDS(file.path(stores_path, "train_data.rds"))
train = as.data.frame(train) 
test =  readRDS(file.path(stores_path, "test_data.rds"))
test = as.data.frame(test)


#-----------------------------------------------------------------------------//
# Estimar regresión 
#-----------------------------------------------------------------------------//

reg_lin = lm(price ~ type_housing + seguridad + surface_total + 
               bathrooms + gym + balcon , data = train)

summary(reg_lin)

lpred <- predict(reg_lin, train)

#MAE
with(train, mean(abs(lpred - price)))

resultados = data.frame(
  Obsr = (train$property_id),
  Observado = train$price,  
  Predicho = predict(reg_lin, data = train))

ggplot(resultados, aes(x = Observado)) +
  geom_density(aes(fill = "Observado"), alpha = 0.5, color = NA) +
  geom_density(aes(x = Predicho, fill = "Predicho"), alpha = 0.2, color = NA) +
  
  labs(title = "Densidad de valores observados vs. predichos",
       x = "Precio",
       y = "Densidad",
       fill = "Valores") +
  scale_fill_manual(values = c("Observado" = "gray", "Predicho" = "blue")) +
  theme_classic()

#-----------------------------------------------------------------------------//
# Guardar submit
#-----------------------------------------------------------------------------//

submit = data.frame(
          property_id = test$property_id,
          price = predict(reg_lin, newdata = test)
        )

head(submit)

write.csv(submit, file.path(stores_path, "submit1_lm.csv"), row.names = FALSE)




