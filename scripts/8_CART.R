# 1. CARGAR DATOS + LIMPIEZA ---------------------------------------------------

train_data =  readRDS(file.path(stores_path, "train_data.rds"))
train_spatial =  readRDS(file.path(stores_path, "spatial_train.rds"))
train = merge(train_data, train_spatial, by = c("property_id"), all = T)
train <- train %>% select(-c(city, description_adj, month, year))
train <- train %>%st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Lo mismo para test si lo necesitas en análisis espacial


test_data =  readRDS(file.path(stores_path, "test_data.rds"))
test_spatial =  readRDS(file.path(stores_path, "spatial_test.rds"))
test = merge(test_data, test_spatial, by = c("property_id"), all = T)
test <- test %>% select(-c(city, description_adj, month, year))
test <- test %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
colnames(test)

rm(train_data, test_data, train_spatial, test_spatial)


# 2. MODELO CART -------------------------------------------------------------

# Fórmula usando todas las variables excepto "property_id" y "price"
train_st_drop <- st_drop_geometry(train)
vars_predictoras <- setdiff(names(train_st_drop), c("property_id", "price"))
fmla <- as.formula(paste("price ~", paste(vars_predictoras, collapse = " + ")))
colnames(train_st_drop)

# Árbol complejo inicial
complex_tree <- rpart(fmla,
                      data = train_st_drop,
                      method = "anova",
                      cp = 0,
                      minbucket = 15)

# Visualizar el árbol
prp(complex_tree,
    under = TRUE,
    branch.lty = 2,
    faclen = 0,
    varlen = 10,
    box.palette = "-RdYlGn")

# 3. VALIDACIÓN CRUZADA Y PODA ----------------------------------------------

ctrl <- trainControl(method = "cv", number = 5)

grid <- expand.grid(cp = seq(0, 0.03, 0.001))

cv_tree <- train(fmla,
                 data = train_st_drop,
                 method = "rpart",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric = "MAE",
                 control = rpart.control(minbucket = 20))

best_cp <- cv_tree$bestTune$cp
print(paste("Mejor cp:", best_cp))

# Visualizar el árbol final podado
prp(cv_tree$finalModel,
    under = TRUE,
    branch.lty = 2,
    faclen = 0,
    varlen = 10,
    box.palette = "-RdYlGn")

# 3. EVALUACIÓN EN TRAIN sf_drop ----------------------------------------------------

pred_train0 <- predict(complex_tree, newdata = train_st_drop)
mae_cart0 <- mean(abs(train_st_drop$price - pred_train0))
print(paste("MAE en train (CART sf drop):", round(mae_cart0, 2)))
train_st_drop$price_pred <- pred_train0
ver0 = train_st_drop %>% select(property_id, price, price_pred)
View(ver0)

prueba0 = data.frame(property_id = train_st_drop$property_id,
                    predicho = predict(cv_tree, newdata = train_st_drop), 
                    observado = train_st_drop$price)

dev.off()  # Cierra el dispositivo gráfico actual

CART_CV = ggplot(prueba0, aes(x = observado)) +
  geom_density(aes(fill = "Observado"), alpha = 0.5, color = NA) +
  geom_density(aes(x = predicho, fill = "Predicho"), alpha = 0.2, color = NA) +
  
  labs(title = "",
       x = "Precio",
       y = "Densidad",
       fill = "Valores") +
  scale_fill_manual(values = c("Observado" = "gray", "Predicho" = "#63B8FF")) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.justification = "center"
  )
CART_CV
ggsave(file.path(paste0(view_path, "/CART_CV.png")), 
       plot = CART_CV, width = 10, height = 6, dpi = 300)

# ---------------------------------------------------- ----------------------------------------------------
# ---------------------------------------------------- ----------------------------------------------------
# ---------------------------------------------------- ----------------------------------------------------




# 4. VALIDACIÓN CRUZADA ESPACIAL Y PODA ----------------------------------------------

#graficar con ggplot
ggplot() +
  geom_sf(data = train)+
  theme_bw()
p_load("leaflet")

set.seed(123)
block_folds <- spatial_block_cv(train, v = 5)
train_df <- st_drop_geometry(train)
vars_predictoras <- setdiff(names(train_df), c("property_id", "price", "lon", "lat")); vars_predictoras
fmla <- as.formula(paste("price ~", paste(vars_predictoras, collapse = " + ")))

# 5. Crear índice para trainControl a partir de block_folds
index <- lapply(block_folds$splits, function(x) x$in_id)  # in_id es el vector con índices de entrenamiento

CART_ECV_FOLSD = walk(block_folds$splits, function(x) print(autoplot(x)))
CART_ECV_FOLSD <- map(block_folds$splits, ~ autoplot(.x))
ggsave(file.path(paste0(view_path, "/CART_ECV_FOLSD_1.png")), 
       plot = CART_ECV_FOLSD[[1]], width = 10, height = 6, dpi = 300)

ctrl <- trainControl(method = "cv", index = index)

grid <- expand.grid(cp = seq(0, 0.03, 0.001))

cv_tree <- train(fmla,
                 data = train,
                 method = "rpart",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric = "MAE",
                 control = rpart.control(minbucket = 20))

best_cp <- cv_tree$bestTune$cp
print(paste("Mejor cp:", best_cp))

# Visualizar el árbol final podado
prp(cv_tree$finalModel,
    under = TRUE,
    branch.lty = 2,
    faclen = 0,
    varlen = 10,
    box.palette = "-RdYlGn")

# 5. EVALUACIÓN EN TRAIN ----------------------------------------------------

pred_train <- predict(cv_tree, newdata = train)
mae_cart <- mean(abs(train$price - pred_train))
print(paste("MAE en train (CART):", round(mae_cart, 2)))
train$price_pred <- pred_train
ver = train %>% select(property_id, price, price_pred)
View(ver)

prueba = data.frame(property_id = train$property_id,
                    predicho = predict(cv_tree, newdata = train), 
                    observado = train$price)


CART_ECV = ggplot(prueba, aes(x = observado)) +
  geom_density(aes(fill = "Observado"), alpha = 0.5, color = NA) +
  geom_density(aes(x = predicho, fill = "Predicho"), alpha = 0.2, color = NA) +
  
  labs(title = "",
       x = "Precio",
       y = "Densidad",
       fill = "Valores") +
  scale_fill_manual(values = c("Observado" = "gray", "Predicho" = "#63B8FF")) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.justification = "center"
  )
CART_ECV
ggsave(file.path(paste0(view_path, "/CART_ECV.png")), 
       plot = CART_ECV, width = 10, height = 6, dpi = 300)


# 6. PREDICCIÓN EN TEST ------------------------------------------------------

pred_test <- predict(cv_tree, newdata = test)
test$price <- pred_test

# Exportar
submit <- paste0("CART_alfa_", best_cp, ".csv")
# Verificar
head(test %>% select(property_id, price))
test_dta = st_drop_geometry(test)
head(test_dta %>% select(property_id, price))

write.csv(test_dta %>% select(property_id, price),
          file.path(stores_path, submit),
          row.names = FALSE)

