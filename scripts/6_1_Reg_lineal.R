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
               bathrooms + gym + balcon, data = train)

summary(reg_lin)

#-----------------------------------------------------------------------------//
# Guardar submit
#-----------------------------------------------------------------------------//

submit = data.frame(
  property_id = test$property_id,
  price = predict(reg_lin, newdata = test)
)
head(submit)
write.csv(submit, file.path(stores_path, "submit_lm.csv"), row.names = FALSE)


