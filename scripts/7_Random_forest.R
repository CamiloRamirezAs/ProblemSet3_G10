#-----------------------------------------------------------------------------//
# Modelo Random Forest
# Problem Set 2 G10 - BDML 202501
# Fecha actualización: 05 de abril de 2025
#-----------------------------------------------------------------------------//

# 1. IMPORTAR DATOS ------------------------------------------------------------

#up_train_raw <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
train_raw <- readRDS(file.path(stores_path, "train_data.rds"))
test_raw  <- readRDS(file.path(stores_path, "test_data.rds"))


# 2. PREPROCESAMIENTO ----------------------------------------------------------

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


# 4. CONSTRUIR EL MODELO RANDOM FOREST ______-----------------------------------

# Crear el arbol complejo
rf<- ranger::ranger(
      Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 + jefe_salud_sub +
        N_personas + hacinamiento + N_ocupados + N_inactivos +
        N_menores + N_mayor_dependiente + max_nivel_educ + Clase, 
      data = train,
      num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
      mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición
      min.node.size  = 1, ## Numero minimo de observaciones en un nodo
      importance="impurity") 
rf

# Mejor modelo
print(rf)

# Graficar modelo
vip(rf)


# 5. PREDICCIONES EN VALIDACION (usando votos de los arboles) ------------------

# Predecimos con todos los arboles individualmente
pred_raw <- predict(rf, data = validation, predict.all = TRUE)$predictions

# Convertimos a data frame para operar con votos
pred.rf <- as.data.frame(pred_raw)

# Contamos votos por clase "Pobre" (asumimos codificada como 1)
ntrees <- ncol(pred.rf)
phat_rf_val <- rowSums(pred.rf == 1) / ntrees

# Clasificamos según umbral 0.5
pred_class_val <- ifelse(phat_rf_val >= 0.5, 1, 0)


# 6. CALCULAR MÉTRICAS ---------------------------------------------------------

# Crear vector numérico para la variable dependiente real
actual_val <- ifelse(validation$Pobre == "Pobre", 1, 0)

# AUC
aucval_rf <- Metrics::auc(actual = actual_val, predicted = phat_rf_val)

# F1 Score
f1_val <- F1_Score(y_true = actual_val, y_pred = pred_class_val, positive = "1")

# Matriz de confusión
cm <- confusionMatrix(as.factor(pred_class_val), as.factor(actual_val), positive = "1")

# Imprimir métricas
cat("AUC en validación (votos RF):", round(aucval_rf, 5), "\n")
cat("F1 Score en validación (umbral 0.5):", round(f1_val, 4), "\n")
print(cm)

# 7. PREDICCIONES EN TEST PARA KAGGLE ------------------------------------------

# Predecimos usando votos de los árboles
pred_test_raw <- predict(rf, data = test_raw, predict.all = TRUE)$predictions
pred_test_df <- as.data.frame(pred_test_raw)

# Probabilidad estimada de ser "Pobre"
phat_rf_test <- rowSums(pred_test_df == 1) / ntrees

# Clasificación con umbral 0.5
test_raw$pobre <- ifelse(phat_rf_test >= 0.5, "Pobre", "No_pobre")

# Crear base para envío
predictSample <- test_raw %>% 
  select(id, pobre) %>%
  mutate(pobre = ifelse(pobre == "Pobre", 1, 0))

# Nombre del archivo según parámetros usados
name <- paste0("RF_ntrees_", rf$num.trees,
               "_mtry_", rf$mtry,
               "_minNode_", rf$min.node.size,
               ".csv")

# Guardar archivo
write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)

# Verificar distribución y nombre
print(table(predictSample$pobre))
print(paste("Archivo guardado:", name))

