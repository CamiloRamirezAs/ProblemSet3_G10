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

rf_tuned

rf_tuned$bestTune


# Graficar los resultados del tuning
plot(rf_tuned)

png(file.path(view_path, "RF_cv_tunning.png"))
print(plot(rf_tuned))
dev.off()

# Mostrar el mejor modelo
print(rf_tuned)

# Crear la gráfica de importancia de variables
vip_plot <- vip(rf_tuned)
vip_plot

# Guardar la gráfica como archivo PNG
vip_file <- file.path(view_path, "RF_CV_variable_importance.png")
ggplot2::ggsave(
  filename = vip_file,
  plot = vip_plot,
  width = 8,   # Ancho en pulgadas
  height = 6,  # Alto en pulgadas
  dpi = 300    # Resolución en puntos por pulgada
)
vip_file

cat("La gráfica de importancia de variables se guardó como:", vip_file, "\n")

# Guardar el mejor modelo en un archivo de texto
best_model_file <- "best_model_rf.txt"
write(
  paste(capture.output(print(rf_tuned)), collapse = "\n"),
  file = file.path(submission_path, best_model_file)
)
cat("El mejor modelo se guardó en:", file.path(view_path, best_model_file), "\n")


# Guardar la gráfica del tuning en un archivo PNG
tuning_results_file <- "tuning_results_rf.png"
png(filename = file.path(submission_path, tuning_results_file), width = 800, height = 600)
plot(rf_tuned, main = "Resultados del Tuning para Random Forest")
dev.off()
cat("La gráfica de tuning se guardó como:", file.path(view_path, tuning_results_file), "\n")



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
  "RandomForest_CV_revisar_trees_", num_trees_val,
  "_mtry", rf_tuned$bestTune$mtry,
  "_nodesize", rf_tuned$bestTune$min.node.size,
  ".csv"
)



# 7. GRÁFICAS ------------------------------------------

# MAE según hierparámetros
ggplot(tuning_results, aes(x = mtry, y = min.node.size, size = MAE, color = MAE)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red", name = "MAE") +
  scale_size_continuous(name = "MAE") +  # Mismo nombre para unificar leyendas
  labs(
    title = "",
    x = "mtry", 
    y = "min.node.size"
  ) +
  theme_bw() +
  guides(color = guide_legend(), size = guide_legend())  # Fuerza leyendas unificadas




# Exportar el archivo en formato CSV para Kaggle
write.csv(kaggle_submission, file.path(submission_path, file_name), row.names = FALSE)

print(paste("Archivo guardado como:", file_name))




# Versión avanzada del gráfico de importancia

vip_plot <- vip(rf_tuned, num_features = 15, geom = "col", fill = "#1E8449") +
  labs(#title = "Variables más influyentes en el precio (Chapinero)",
       #subtitle = "Importancia calculada por permutación",
       y = "Reducción en MAE al incluir la variable") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

vip_plot

ggsave(file.path(view_path, "RF_cv_mportancia_variables.png"), vip_plot, width = 10, height = 6, dpi = 300)



# Convertir el plot de caret a interactivo
tuning_data <- rf_tuned$results
tuning_plot <- plot_ly(tuning_data, x = ~mtry, y = ~MAE, 
                       color = ~as.factor(min.node.size), 
                       type = "scatter", mode = "lines+markers",
                       text = ~paste("Node size:", min.node.size)) %>%
  layout(title = "Optimización de Hiperparámetros (MAE)",
         xaxis = list(title = "Número de Variables por Split (mtry)"),
         yaxis = list(title = "MAE en Validación Cruzada"),
         hoverlabel = list(bgcolor = "white"))

tuning_plot

htmlwidgets::saveWidget(tuning_plot, "RF_tuning_interactivo.html")




# Predicciones vs. valores reales

validation$predicted <- predict(rf_tuned, validation)

scatter_plot <- ggplot(validation, aes(x = price, y = predicted)) +
  geom_point(alpha = 0.6, color = "#2874A6") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Predicciones vs. Precios reales (validación)",
       subtitle = paste("MAE:", round(mae, 2)),
       x = "Precio real (COP)", y = "Precio predicho (COP)") +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar)

scatter_plot

ggsave(file.path(view_path, "RF_pred_vs_real.png"), scatter_plot, width = 8, height = 6)




tree_data <- rpart(price ~ ., data = train[, c("price", "surface_total", "bedrooms", "type_housing")])
rpart.plot(tree_data, 
           main = "Ejemplo de Árbol de Decisión (Simplificado)",
           box.palette = "Greens",
           shadow.col = "gray")







validation_sf <- st_as_sf(validation, coords = c("lon", "lat"), crs = 4326)
validation_sf$error <- abs(validation_sf$price - validation_sf$predicted)

leaflet(validation_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~log(error)/2,
    color = ~ifelse(error > median(error), "red", "blue"),
    popup = ~paste("Error:", round(error), "<br>Precio Real:", price)
  ) %>%
  addLegend("bottomright", colors = c("red", "blue"), 
            labels = c("Error alto", "Error bajo"))





# 1. Añadir predicciones y errores al conjunto de validación
validation$predicted <- predict(rf_tuned, validation)
validation$error <- abs(validation$price - validation$predicted)

# 2. Gráfico de dispersión con facetas por tipo de vivienda
error_plot <- ggplot(validation, aes(x = price, y = predicted, color = type_housing)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_wrap(~type_housing, scales = "free") +  # Separar por tipo de vivienda
  labs(
    #title = "Error de Predicción por Tipo de Vivienda (Chapinero)",
    subtitle = paste("MAE Global:", round(mae, 2), "COP | Línea roja: Predicción perfecta"),
    x = "Precio real (COP)",
    y = "Precio predicho (COP)",
    color = "Tipo de vivienda"
  ) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    strip.background = element_rect(fill = "#2874A6"),  # Color de la banda de facetas
    strip.text = element_text(color = "white", face = "bold")
  ) +
  scale_color_manual(values = c("Casa" = "#1E8449", "Apartamento" = "#3498DB"))



error_plot

# 3. Guardar el gráfico
ggsave("RF_error_por_tipo_vivienda.png", error_plot, width = 12, height = 6, dpi = 300)






validation %>%
  group_by(type_housing) %>%
  summarise(MAE = mean(error)) %>%
  ggplot(aes(x = type_housing, y = MAE, fill = type_housing)) +
  geom_col() +
  labs(title = "MAE por tipo de vivienda")


