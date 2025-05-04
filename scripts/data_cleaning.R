
#-----------------------------------------------------------------------------//
# Importar datos
# Problem Set 3 
# Fecha: 3 de mayo de 2025
#-----------------------------------------------------------------------------//

# 2. IMPORTAR DATOS ------------------------------------------------------------

# Ruta con las bases de datos en ZIP
zip_path <- file.path(raw_path, "uniandes-bdml-2025-10-ps-3.zip")

# Listar los archivos dentro del ZIP para verificar la estructura
zip_files <- zip::zip_list(zip_path)
print(zip_files$file)  # Para ver cómo están organizados dentro del ZIP

# Definir los nombres correctos de los archivos dentro del ZIP
test <- read_csv(unz(zip_path, "test.csv"))
train <- read_csv(unz(zip_path, "train.csv"))

sapply(train, class)
sapply(test, class)

#Revisar missings
vis_dat(train)
vis_dat(test)

#Stats
skim(train)
skim(test)

#Imputación para superficie, número de cuartos y de baños
#Con la moda para número de cuartos y número de baños

# Calcular la moda  de cuartos e imputar
moda_rooms <- as.numeric(names(sort(table(train$rooms), decreasing = TRUE))[1])
train$rooms[is.na(train$rooms)] <- moda_rooms

# Calcular la moda  de baños e imputar
moda_bath <-  as.numeric(names(sort(table(train$bathrooms), decreasing = TRUE))[1])
train$bathrooms[is.na(train$bathrooms)] <- moda_bath

#Calcular la mediana para superficie total e imputar
median_surftotal <- as.numeric(median(train$surface_total, na.rm = TRUE))
train$surface_total[is.na(train$surface_total)] <- median_surftotal

#Calcular la mediana para superficie cubierta e imputar
median_surfcov <- as.numeric(median(train$surface_covered, na.rm = TRUE))
train$surface_covered[is.na(train$surface_covered)] <- median_surfcov

#Para evitar data leakage imputamos en test los mismos valores para los missings
test$rooms[is.na(test$rooms)] <- moda_rooms
test$bathrooms[is.na(test$bathrooms)] <- moda_bath
test$surface_total[is.na(test$surface_total)] <- median_surftotal
test$surface_covered[is.na(test$surface_covered)] <- median_surfcov

#Precio por m2
train <- train %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0)) 
summary(train$precio_por_mt2) #promedio coherente con lo reportado por constructoras pero con valores atipicos
hist(train$precio_por_mt2)

train %>%
  ggplot(aes(y = precio_por_mt2)) +
  geom_boxplot(fill = "darkblue", alpha = 0.4) +
  labs(
    y = "Precio por metro cuadrado", x = "") +
  theme_bw()

#Eliminar obs por encima y por debajo de 2SD??
low <- round(mean(train$precio_por_mt2) - 2*sd(train$precio_por_mt2),2)
up <- round(mean(train$precio_por_mt2) + 2*sd(train$precio_por_mt2))

#Identificar duplicados en precio y descripcion
train <- train %>%
  group_by(description, price) %>%
  mutate(dup = if_else(n() > 1, 1, 0)) %>%
  ungroup()
table(train$dup)

train_nodup <- train %>%
  group_by(description, price) %>%
  filter(row_number() == 1) %>%
  ungroup()




