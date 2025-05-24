#-----------------------------------------------------------------------------//
# Modelo Random Forest
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 21 de mayo de 2025
#-----------------------------------------------------------------------------//

# 1. IMPORTAR DATOS ------------------------------------------------------------

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


# 2. PREPROCESAMIENTO ----------------------------------------------------------

# Asegurar que tipo de vivienda es un factor
train_raw$type_housing <- factor(train_raw$type_housing, levels = c("Casa", "Apartamento"))
test_raw$type_housing <- factor(test_raw$type_housing, levels = c("Casa", "Apartamento"))


# 3. DIVISION DE LA MUESTRA ----------------------------------------------------

# Establecer semilla
set.seed(91519) 

# Dividir datos en entrenamiento y validación

inTrain <- createDataPartition(
          y = train_raw$price, ## La variable dependiente u objetivo 
          p = .9, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
          list = FALSE)

train <- train_raw[inTrain, ]
validation <- train_raw[-inTrain, ]

# Verificar la distribucion 
table(train$price)
table(validation$price)


# 4. CONSTRUIR EL MODELO RANDOM FOREST -----------------------------------

# Crear el arbol complejo

rf <- ranger::ranger(
  price ~ surface_total + bedrooms + bathrooms + type_housing +
          piscina + garaje + seguridad + balcon + gym + year +
          dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
          dist_transmi,
  data = train,
  num.trees = 500,        # Incrementar número de árboles
  mtry = 5, # Elegir sqrt del número de variables
  min.node.size = 5,        # Nodo mínimo ajustado
  importance = "impurity"
)


# Mejor modelo
print(rf)

# Graficar modelo
vip(rf)


# 5. PREDICCIONES EN VALIDACION ------------------

# Realizar predicciones en el conjunto de validación
predictions_validation <- predict(rf, data = validation)

# Extraer las predicciones continuas
phat_rf_val <- predictions_validation$predictions

# Revisar las primeras predicciones
print("Primeras predicciones:")
head(phat_rf_val)

# Evaluar el modelo con MAE
mae <- mean(abs(validation$price - phat_rf_val))
print(paste("Mean Absolute Error (MAE):", mae))

# Guardar las predicciones en el conjunto de validación
validation_with_predictions <- validation %>%
  mutate(predicted_price = phat_rf_val)


# 7. PREDICCIONES EN TEST PARA KAGGLE ------------------------------------------

# Realizar predicciones en el conjunto de prueba
predictions_test <- predict(rf, data = test_raw)

# Extraer las predicciones continuas
phat_rf_test <- predictions_test$predictions

# Crear un dataframe con property_id y las predicciones
kaggle_submission <- data.frame(
                      property_id = test_raw$property_id,
                      price = phat_rf_test
                    )

# Revisar las primeras filas del archivo de salida
head(kaggle_submission)

# Crear el nombre del archivo dinámicamente con los parámetros del modelo
file_name <- paste0(
            "RandomForest_trees", rf$num.trees, 
            "_mtry", rf$mtry, 
            "_nodesize", rf$min.node.size, 
            ".csv"
          )

# Exportar el archivo en formato CSV para Kaggle
write.csv(kaggle_submission, file.path(submission_path, file_name), row.names = FALSE)

# Confirmar el nombre del archivo generado
print(paste("Archivo generado:", file_name))

