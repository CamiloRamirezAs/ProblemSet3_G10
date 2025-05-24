#------------------------------------------------------------------------------#
# Modelos: bagging y boosting ----
#------------------------------------------------------------------------------#

train = readRDS(file.path(stores_path, "upsampled_train_data.rds"))
#train = readRDS(file.path(stores_path, "train_data.rds"))
test  = readRDS(file.path(stores_path, "test_data.rds"))

intersect(colnames(train), colnames(test))

#---------------------------
# Ajuste de variables y cols
#---------------------------

train = train[, !duplicated(colnames(train))]
train = train %>% dplyr::mutate(Pobre_d = ifelse(Pobre == "Pobre", 1, 0))

#Pobre_num = ifelse(train$Pobre_d == "Si", 1, 0)

#---------------------------
# Ajuste de variables factor
#---------------------------
X_1 = c("hacinamiento", 
        "Clase",
        "jefe_mujer", 
        "prop_ocu_pet", 
        "jefe_edad",
        "jefe_nivel_educ",
        "N_mayor_dependiente", 
        "prop_ina_pet",
        "jefe_pension", 
        "N_menores")

summary(train$hacinamiento)
summary(train$jefe_edad)
summary(train$N_menores)
table(train$jefe_mujer)
summary(train$prop_ocu_pet)
summary(train$N_mayor_dependiente)
table(train$jefe_nivel_educ)
table(train$jefe_pension)
table(train$jefe_salud_sub)
table(train$Pobre)

train = train %>% mutate(
        Pobre_d = ifelse(Pobre == "Pobre", 1, 0),
        hacinamiento_f = factor(ifelse(hacinamiento > 3, 1, 0), labels = c("Si", "No")), # porque es el a lo que se aproxima el 3 cuantil
        jefe_mayor_f   = factor(ifelse(jefe_edad > 49, 1, 0), labels = c("Si", "No")), # puede ser indicador de que la persona que sostiene el hogar tenga más o menos dinámica laboral
        N_menores_f    = factor(ifelse(N_menores > 1.6, 1, 0), labels = c("Si", "No")), # número promedio hijas/os por mujer en Colombia (podrían ser más grandes pero por practicidad)
        jefe_mujer_f   = factor(ifelse(jefe_mujer == "Jefe_mujer", 1, 0), labels = c("Si", "No")), 
        prop_ocu_pet_f = factor(ifelse(prop_ocu_pet > 0.6, 1, 0), labels = c("Si", "No")), # hogares usualmente 3,3 personas, que trabaje al menos el 60 % (cuantil 3) 
        Mayor_dependiente_f = factor(ifelse(N_mayor_dependiente >= 1, 1, 0), labels = c("Si", "No")),
        jefe_cot_pens  = factor(ifelse(jefe_pension == "Jefe_af_pension", 1, 0), labels = c("Si", "No")),
        jefe_cont_salud  = factor(ifelse(jefe_salud_sub == "Jefe_salud_contributivo", 1, 0), labels = c("Si", "No")), 
        N_desocupados_f = factor(ifelse(N_desocupados >= 1, 1, 0), labels = c("Si", "No")),
        )

test = test %>% mutate(
        hacinamiento_f = factor(ifelse(hacinamiento > 3, 1, 0), labels = c("Si", "No")), # porque es el a lo que se aproxima el 3 cuantil
        jefe_mayor_f   = factor(ifelse(jefe_edad > 49, 1, 0), labels = c("Si", "No")), # puede ser indicador de que la persona que sostiene el hogar tenga más o menos dinámica laboral
        N_menores_f    = factor(ifelse(N_menores > 1.6, 1, 0), labels = c("Si", "No")), # número promedio hijas/os por mujer en Colombia (podrían ser más grandes pero por practicidad)
        jefe_mujer_f   = factor(ifelse(jefe_mujer == "Jefe_mujer", 1, 0), labels = c("Si", "No")), 
        prop_ocu_pet_f = factor(ifelse(prop_ocu_pet > 0.6, 1, 0), labels = c("Si", "No")), # hogares usualmente 3,3 personas, que trabaje al menos el 60 % (cuantil 3) 
        Mayor_dependiente_f = factor(ifelse(N_mayor_dependiente >= 1, 1, 0), labels = c("Si", "No")),
        jefe_cot_pens  = factor(ifelse(jefe_pension == "Jefe_af_pension", 1, 0), labels = c("Si", "No")),
        jefe_cont_salud  = factor(ifelse(jefe_salud_sub == "Jefe_salud_contributivo", 1, 0), labels = c("Si", "No")), 
        N_desocupados_f = factor(ifelse(N_desocupados >= 1, 1, 0), labels = c("Si", "No"))
) 

sapply(train, class)

table(train$jefe_salud_sub)
table(test$jefe_salud_sub)


X_2 = c("jefe_edad", 
        "jefe_mujer",
        "jefe_edad2",
        "jefe_salud_sub", 
        "N_personas",
        "hacinamiento",
        "max_nivel_educ", 
        "Clase",
        "viv_noPropia",
        "prop_fuentes_ing", 
        "prop_ina_pet",
        "prop_ocu_pet",
        "jefe_pension",
        "prop_menores_pob",
        "prop_mayores_pob",
        "promedio_anios_educ",
        "tipo_trabajo")

#table(train$N_desocupados)

#------------------------------------------------------------------------------#
# 1. Modelos ----
#------------------------------------------------------------------------------#

#-----------------------
# 1.1. Bagging 
#-----------------------

bagged_tree = ranger::ranger(
              formula(paste0("Pobre ~", paste0(X_2, collapse = " + "))),
              data = train,
              num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
              mtry = 8,
              min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
              probability = TRUE  # clave para obtener probabilidades
            ) 
bagged_tree

table(train$Pobre)
table(train$Pobre_d)

# Calcular predicciones

# Guardamos la predicción de cada árbol en forma de dataframe
pred.bag_ranger <- as.data.frame( bagged_tree$predictions )

# Visualizemoslo
head(tibble(pred.bag_ranger))


# Calcular las probabilidades (promedio de todas las predicciones de los árboles)
phat.bag <- rowMeans(pred.bag_ranger[, 1, drop = FALSE]) # Probabilidades de ser "Pobre"


# Calcular y guardar AUC de bagging
aucval_bag = Metrics::auc(
              actual = train$Pobre_d,
              predicted = phat.bag)

aucval_bag

# Umbral de 0.5 para obtener las predicciones binarias
yhat.bag <- ifelse(phat.bag >= 0.5, 1, 0)

# Calcular el F1
F1_Score(
  y_pred = factor(yhat.bag, levels = c(1, 0)),
  y_true = factor(train$Pobre_d, levels = c(1, 0)),
  positive = "1"
)


#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

# Calcular las probabilidades para test
preds_test <- predict(bagged_tree, data = test)$predictions

# Asumimos que preds_test tiene columnas: Pobre, No_pobre
pred_pobre <- preds_test[, 1]

# Convertir las probabilidades en etiquetas binarias (0 o 1)
predictSample <- test %>%
  mutate(pobre_lab = ifelse(pred_pobre >= 0.5, 1, 0)) %>%
  select(id, pobre_lab) %>%
  rename(pobre = pobre_lab) %>%
  arrange(id)

head(predictSample)

# Guardar CSV
name = paste0(
  "Bagging_3",
  ".csv"
)

write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)



#-----------------------
# 1.2. Boosting 
#-----------------------

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}


ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE, 
                    verboseIter = TRUE,   # muestra el progreso
                    savePredictions = TRUE)

adagrid = expand.grid(
          mfinal = c(50, 300 ,500),
          maxdepth = c(2,3,5),
          coeflearn = c('Breiman','Freund'))


set.seed(91519) # important set seed. 

adaboost_tree <- train(
                       formula(paste0("Pobre ~", paste0(X_2, collapse = " + "))),
                       data = train, 
                       method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
                       trControl = ctrl,
                       metric = "ROC",
                       tuneGrid = adagrid
)

adaboost_tree


# Cálculo del AUC sobre el conjunto de entrenamiento ---------------------------

# Obtener las probabilidades predichas para el conjunto de entrenamiento
pred_probs_train <- predict(adaboost_tree, newdata = train, type = "prob")

# Extraer la probabilidad de la clase "Pobre"
prob_pobre_train <- pred_probs_train$Pobre

# Cálculo del AUC utilizando la librería pROC
roc_curve_train <- roc(train$Pobre, prob_pobre_train)

# Mostrar AUC
auc(roc_curve_train)


# Cálculo del AUC sobre el conjunto de entrenamiento ---------------------------

# Obtener las clases predichas para el conjunto de entrenamiento (con un umbral de 0.5)
yhat_train <- ifelse(prob_pobre_train >= 0.5, 1, 0)

# Calcular el F1 Score
F1_Score_train <- F1_Score(
  y_pred = factor(yhat_train, levels = c(1, 0)),
  y_true = factor(train$Pobre_d, levels = c(1, 0)),
  positive = "1"
)

# Mostrar el F1 Score
F1_Score_train


#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

# Realizar predicciones sobre el conjunto de test (con probabilidades)
pred_probs_test <- predict(adaboost_tree, newdata = test, type = "prob")

# Ver las primeras predicciones
head(pred_probs_test)

# Crear una predicción binarizada (0 o 1) usando un umbral de 0.5
pred_class_test <- ifelse(pred_probs_test$Pobre >= 0.5, 1, 0)

# Crear un dataframe para Kaggle con las predicciones
predictSample <- test %>%
  mutate(pobre_pred = pred_class_test) %>%
  select(id, pobre_pred) %>%
  arrange(id)  # Si necesitas ordenar por 'id'

# Ver las primeras filas del dataframe
head(predictSample)

# Extraer los parámetros óptimos
best_params <- adaboost_tree$bestTune

# Crear el nombre del archivo con los parámetros
file_name <- paste0("Boosting_AdaBoost", 
                    "mfinal", best_params$mfinal, 
                    "_maxdepth", best_params$maxdepth, 
                    "_coeflearn", best_params$coeflearn, 
                    ".csv")

# Guardar el archivo CSV
write.csv(predictSample, file.path(stores_path, file_name), row.names = FALSE)