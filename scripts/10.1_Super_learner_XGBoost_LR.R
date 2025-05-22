#-----------------------------------------------------------------------------//
# Modelo Super learner: XGBoost & lineal regression
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 22 de mayo de 2025
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

# Crear objetos sf con CRS para lon/lat (WGS84 - EPSG:4326)
train_sf <- st_as_sf(train_raw, coords = c("lon", "lat"), crs = 4326)
test_sf <- st_as_sf(test_raw, coords = c("lon", "lat"), crs = 4326)


# 3. CREAR BLOQUES ESPACIALES --------------------------------------------------

# Crear bloques espaciales

set.seed(12345)
spatial_folds <- spatial_clustering_cv(
                data = train_sf,
                coords = c("longitude", "latitude"),  # Nombres de las columnas de coordenadas
                cluster_function = kmeans,           # Algoritmo para crear clusters espaciales
                v = 5                                # Número de bloques
                )


# 3. DIVISION DE LA MUESTRA ----------------------------------------------------

# Establecer semilla
set.seed(91519) 

# Dividir datos en entrenamiento y validación

inTrain <- createDataPartition(
          y = train_raw$price, ## La variable dependiente u objetivo 
          p = .95, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
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
          dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
          num_restaurantes,
  data = train,
  num.trees = 1000,        # Incrementar número de árboles
  mtry = 5, # Elegir sqrt del número de variables
  min.node.size = 5,        # Nodo mínimo ajustado
  importance = "permutation"
)


# Mejor modelo
print(rf)

# Graficar modelo
vip(rf)


# 7. TUNING AUTOMÁTICO CON CARET ----------------------------------------------

set.seed(91519)

# Definir control para validación cruzada
ctrl <- trainControl(
  method = "cv",          # Validación cruzada
  number = 5,             # 5-fold CV
  verboseIter = TRUE      # Para seguir el proceso
)

# Definir grid de búsqueda para los hiperparámetros
tuneGrid <- expand.grid(
  mtry = c(2, 4, 6, 8),          # Número de variables a considerar en cada split
  splitrule = c("variance"),     # Regresión usa 'variance' como criterio
  min.node.size = c(1, 5, 10)    # Tamaño mínimo de nodos
)

# Entrenar el model

rf_tuned <- train(
            price ~ surface_total + bedrooms + bathrooms + type_housing +
                    piscina + garaje + seguridad + balcon + gym + year +
                    dist_parque + AVALUO_COM + dist_centComercial + dist_cai +
                    dist_transmi + num_paraderos_sitp + num_colegios + ESTRATO + 
                    num_restaurantes,
            data = train,
            method = "ranger",            # Método: Random Forest con ranger
            trControl = ctrl,             # Configuración de validación cruzada
            tuneGrid = tuneGrid,          # Grid de búsqueda de hiperparámetros
            metric = "MAE",               # Usamos MAE como métrica de evaluación
            num.trees = 1000,             # Número de árboles
            importance = "permutation"    # Importancia de variables
          )


# Graficar los resultados del tuning
plot(rf_tuned)

# Mostrar el mejor modelo
print(rf_tuned)

# Crear la gráfica de importancia de variables
vip_plot <- vip(rf_tuned)
vip_plot

# Guardar la gráfica como archivo PNG
vip_file <- file.path(submission_path, "RF_CV_variable_importance.png")
ggplot2::ggsave(
  filename = vip_file,
  plot = vip_plot,
  width = 8,   # Ancho en pulgadas
  height = 6,  # Alto en pulgadas
  dpi = 300    # Resolución en puntos por pulgada
)

cat("La gráfica de importancia de variables se guardó como:", vip_file, "\n")

# Guardar el mejor modelo en un archivo de texto
best_model_file <- "best_model_rf.txt"
write(
  paste(capture.output(print(rf_tuned)), collapse = "\n"),
  file = file.path(submission_path, best_model_file)
)
cat("El mejor modelo se guardó en:", file.path(submission_path, best_model_file), "\n")


# Guardar la gráfica del tuning en un archivo PNG
tuning_results_file <- "tuning_results_rf.png"
png(filename = file.path(submission_path, tuning_results_file), width = 800, height = 600)
plot(rf_tuned, main = "Resultados del Tuning para Random Forest")
dev.off()
cat("La gráfica de tuning se guardó como:", file.path(submission_path, tuning_results_file), "\n")



# 5. PREDICCIONES EN VALIDACION ------------------

# Realizar predicciones en el conjunto de validación
predictions_validation <- predict(rf_tuned, validation)

# Evaluar el modelo usando MAE
mae <- mean(abs(validation$price - predictions_validation))
print(paste("Mean Absolute Error (MAE):", mae))


# 7. PREDICCIONES EN TEST PARA KAGGLE ------------------------------------------

# Realizar predicciones en el conjunto de prueba
predictions_test <- predict(rf_tuned, test_raw)

# Crear el archivo para Kaggle
kaggle_submission <- data.frame(
                      property_id = test_raw$property_id,
                      price = predictions_test
                    )


# Asumiendo que num.trees es fijo, por ejemplo 500
num_trees_val <- 1000

# Guardar archivo con los hiperparámetros en el nombre
file_name <- paste0(
  "RandomForest_CV_trees", num_trees_val,
  "_mtry", rf_tuned$bestTune$mtry,
  "_nodesize", rf_tuned$bestTune$min.node.size,
  ".csv"
)


# Exportar el archivo en formato CSV para Kaggle
write.csv(kaggle_submission, file.path(submission_path, file_name), row.names = FALSE)

print(paste("Archivo guardado como:", filename))




