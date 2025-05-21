#-----------------------------------------------------------------------------//
# Variables espacialaes
# Problem Set 3 G10 - BDML 202501
# Fecha actualización: 21 de abril de 2025
#-----------------------------------------------------------------------------//

# 1. IMPORTAR DATOS ------------------------------------------------------------

train <- readRDS(file.path(stores_path, "train_data.rds"))
test <- readRDS(file.path(stores_path, "test_data.rds"))

# 2. CARGAR LIBRERIAS

# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       gridExtra, # Para graficar en grid
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels, #para modelos de ML
       webshot2, # Programa para exportar como imagen el mapa inicial
       htmlwidgets,
       leaflet
       ) 

# 2. PREPROCESAMIENTO ----------------------------------------------------------

# Eliminamos las observaciones que no tienen información de latitud o longitud
train_B <- train %>%
          filter(!is.na(lat) & !is.na(lon))


# Primera visualización de los datos

mapa_inicial <- leaflet() %>%
                addTiles() %>%
                addCircles(lng = train_B$lon, 
                           lat = train_B$lat)

# Guardar como HTML
saveWidget(mapa_inicial, "mapa_leaflet.html", selfcontained = TRUE)

# Guardar como PNG
webshot("mapa_leaflet.html",
        file = file.path(view_path, "mapa_leaflet.png"),
        vwidth = 1000,
        vheight = 800)


# Obtener limites para Bogotá
limites <- getbb("Bogota Colombia")
limites

# Limitar los datos solo para Bogotá
train_B <- train_B %>%
          filter(
            between(lon, limites[1, "min"], limites[1, "max"]) & 
              between(lat, limites[2, "min"], limites[2, "max"])
          )








#--------------------------------------






# Eliminar algunas variables que no entran en el modelo
train_raw <- train_raw %>% select(-ends_with("_z"))
test_raw <- test_raw %>% select(-ends_with("_z"))

# Crear algunas interacciones
train_raw <- train_raw %>% 
              mutate(inter_haci_tam = N_personas * hacinamiento)

test_raw <- test_raw %>% 
            mutate(inter_haci_tam = N_personas * hacinamiento)

# Asegurar que Pobre es un factor con niveles correctos
train_raw$Pobre <- factor(train_raw$Pobre, levels = c("Pobre", "No_pobre"))

# Configuracion inicial: utilizar como referencia "Pobre" para la variable Pobre
train_raw <- train_raw  %>% mutate(Pobre = relevel(Pobre, ref="Pobre"))

# Crear variable numerica en train_raw
train_raw$pobre_num <- ifelse(train_raw$Pobre == "Pobre", 1, 0)


# 3. DIVISION DE LA MUESTRA ----------------------------------------------------

# Establecer semilla
set.seed(91519) 

# Dividir datos en entrenamiento y validación

inTrain <- createDataPartition(
          y = train_raw$Pobre, ## La variable dependiente u objetivo 
          p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
          list = FALSE)

train <- train_raw[inTrain, ]
validation <- train_raw[-inTrain, ]

# Verificar la distribucion 
table(train$Pobre)
table(validation$Pobre)


# 4. CONSTRUIR EL XGBOOST -----------------------------------------------------

# Calcular proporción pobres y no pobres
num_neg <- sum(train$Pobre == "No_pobre")
num_pos <- sum(train$Pobre == "Pobre")
scale_pos_weight <- num_neg / num_pos
scale_pos_weight

# Grilla de hiperparámetros con ajustes adicionales
grid_xgboost <- expand.grid(
                nrounds = 500,              # Número de rondas (iteraciones)
                max_depth = 7,             # Profundidad máxima del árbol
                eta = 0.05,           # Tasa de aprendizaje
                gamma = 0,               # Penalización por complejidad
                min_child_weight = 5,      # Peso mínimo de un nodo hijo
                colsample_bytree = 0.6,     # Fracción de características por árbol
                subsample = 0.7                 # Fracción de muestras para cada árbol
              )  

grid_xgboost

# Definir el cross validation

# Función resumen personalizada con F1
fiveStats <- function(data, lev = NULL, model = NULL) {
  c(
    F1 = MLmetrics::F1_Score(y_pred = data$pred, y_true = data$obs, positive = "Pobre"),
    caret::twoClassSummary(data, lev, model)
  )
}


# Configuración de muestreo (sobremuestreo para balancear clases)
fitControl <- trainControl(
              method = "cv", 
              number = 5, 
              summaryFunction = fiveStats, 
              classProbs = TRUE,
              savePredictions = "all", 
              verboseIter = TRUE,
              sampling = "smote"   # Sobremuestreo de la clase minoritaria (Pobre)
            )

# Entrenamiento de XGBoost
set.seed(91519) # Semilla para reproducibilidad

Xgboost_tree <- train(
                Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 + jefe_salud_sub +
                  N_personas + hacinamiento + 
                  max_nivel_educ + Clase + pago_arriendo +
                  viv_noPropia + Lp + prop_fuentes_ing + prop_ina_pet + Dominio + 
                  prop_ocu_pet + jefe_pension + prop_menores12_pob + prop_mayores_pob +
                  prop_anios_educ + tipo_trabajo + prop_des_pet + jefe_ocu + N_mujer,
                data = train_raw, 
                method = "xgbTree", 
                trControl = fitControl, 
                tuneGrid = grid_xgboost, 
                metric = "F1", 
                verbosity = 0
                #scale_pos_weight = scale_pos_weight # Balanceo de clases
              )

Xgboost_tree


# 5. PREDICCIONES EN VALIDACION (XGBoost) --------------------------------------


# Probabilidades estimadas de ser "Pobre"
phat_xgb_val <- predict(Xgboost_tree,
                        newdata = validation,
                        type = "prob")[, "Pobre"]

# Clasificación con umbral 0.5
pred_class_val <- ifelse(phat_xgb_val >= 0.36, 1, 0)

# Vector real binario
actual_val <- ifelse(validation$Pobre == "Pobre", 1, 0)

# Verificar que todos tienen misma longitud
stopifnot(length(actual_val) == length(phat_xgb_val))
stopifnot(length(actual_val) == length(pred_class_val))

# AUC
aucval_xgb <- Metrics::auc(actual = actual_val, predicted = phat_xgb_val)

# F1 Score
f1_val_xgb <- MLmetrics::F1_Score(y_true = actual_val, y_pred = pred_class_val, positive = "1")

# Matriz de confusión
cm_xgb <- caret::confusionMatrix(as.factor(pred_class_val), as.factor(actual_val), positive = "1")

# Imprimir métricas
cat("AUC en validación (XGBoost):", round(aucval_xgb, 5), "\n")
cat("F1 Score en validación (umbral 0.35):", round(f1_val_xgb, 4), "\n")
print(cm_xgb)


# 6. PREDICCIONES EN TEST PARA KAGGLE (XGBoost) --------------------------------

# 1. Estimar probabilidad de ser "Pobre"
phat_xgb_test <- predict(Xgboost_tree, newdata = test_raw, type = "prob")[, "Pobre"]

# 2. Clasificar con umbral 0.5
test_raw$pobre <- ifelse(phat_xgb_test >= 0.36, "Pobre", "No_pobre")

# 3. Crear base de predicción para Kaggle
predictSample <- test_raw %>%
  select(id, pobre) %>%
  mutate(pobre = ifelse(pobre == "Pobre", 1, 0))

# 4. Crear nombre del archivo dinámicamente con mejores parámetros
best_params <- Xgboost_tree$bestTune

# (Opcional: redondear algunos para nombre más corto)
name <- sprintf("XGB_F2_cv_%dfolds_n%d_d%d_eta%.2f_g%.1f_cs%.2f_mc%d_ss%.2f.csv",
                fitControl$number,
                best_params$nrounds,
                best_params$max_depth,
                best_params$eta,
                best_params$gamma,
                best_params$colsample_bytree,
                best_params$min_child_weight,
                best_params$subsample)

# 5. Guardar CSV para envío a Kaggle
write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)

# 6. Mostrar resumen y confirmar guardado
cat("Predicciones guardadas correctamente\n")
print(table(predictSample$pobre))
cat("Archivo de predicciones:", name, "\n")

# 7. Guardar parámetros en un archivo de texto como bitácora
param_log_name <- gsub(".csv", "_params.txt", name)

sink(file.path(stores_path, param_log_name))
cat("Mejores parámetros del modelo XGBoost\n\n")
print(best_params)
sink()

cat("Bitacora de parámetros guardada en:", param_log_name, "\n")


# 7. IMPORTANCIA DE LAS VARIABLES (XGBoost) --------------------------------

# Obtener importancia de variables
var_imp <- varImp(Xgboost_tree)

# Convertir a data frame para ggplot
imp_df <- data.frame(
  Variable = rownames(var_imp$importance),
  Importance = var_imp$importance$Overall
)

# Ordenar por importancia
imp_df <- imp_df %>% arrange(desc(Importance))

# Crear el gráfico y guardarlo en una variable
plot_importance <- ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de las variables en XGBoost",
    x = "Variable",
    y = "Importancia (relativa)"
  ) +
  theme_minimal()

plot_importance

# Guardar el gráfico
ggsave(
  filename = file.path(stores_path, "importancia_variables_xgboostF2.png"),
  plot = plot_importance,
  width = 8,
  height = 6,
  dpi = 300
)



# 8. SELECCIONAR UMBRAL ÓPTIMO (XGBoost) --------------------------------

# Vector de probabilidades
phat_xgb_val <- predict(Xgboost_tree, newdata = validation, type = "prob")[, "Pobre"]
actual_val <- ifelse(validation$Pobre == "Pobre", 1, 0)

# Buscar el mejor umbral
thresholds <- seq(0.1, 0.9, by = 0.01)
f1_scores <- sapply(thresholds, function(thresh) {
  pred_class <- ifelse(phat_xgb_val >= thresh, 1, 0)
  F1_Score(y_true = actual_val, y_pred = pred_class, positive = "1")
})

best_thresh <- thresholds[which.max(f1_scores)]
cat("Mejor umbral para F1 Score:", round(best_thresh, 2), "\n")
cat("Mejor F1 Score obtenido:", round(max(f1_scores), 4), "\n")

# Graficar F1 Score vs Umbral
plot(thresholds, f1_scores, type = "l", col = "blue", lwd = 2,
     xlab = "Umbral", ylab = "F1 Score", main = "Optimización del umbral")
abline(v = best_thresh, col = "red", lty = 2)

# Guardar el gráfico base en un archivo PNG
png(filename = file.path(stores_path, "umbral_f1_xgboostF2.png"),
    width = 800, height = 600)

# Crear gráfico base
plot(thresholds, f1_scores, type = "l", col = "blue", lwd = 2,
     xlab = "Umbral", ylab = "F1 Score", main = "Optimización del umbral")
abline(v = best_thresh, col = "red", lty = 2)

# Cerrar dispositivo gráfico
dev.off()


