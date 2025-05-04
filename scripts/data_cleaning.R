
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

#Identificar propiedades duplicadas y dejar solo una
train <- train %>%
  group_by(year, month, surface_total, surface_covered, rooms, bedrooms, bathrooms, price, lat, lon) %>%
  mutate(dup = if_else(n() > 1, 1, 0)) %>%
  ungroup()
table(train$dup)

train_nodups <- train %>%
  group_by(year, month, surface_total, surface_covered, rooms, bedrooms, bathrooms, price, lat, lon) %>%
  slice_head(n = 1) %>%  
  ungroup()

#Identificar propiedades que se publican varias veces en distintos momentos y dejar la más reciente
train_nodups <- train %>%
  group_by(surface_total, surface_covered, rooms, bedrooms, bathrooms, lat, lon) %>%
  arrange(desc(year), desc(month)) %>%  # Ordena por año y mes (de más reciente a más antiguo)
  mutate(dup = case_when(
    n() > 1 ~ row_number(),  # Si hay duplicados, asigna un número secuencial (1 será la más reciente)
    TRUE ~ 0                 # Si no hay duplicados, asigna 0
  )) %>%
  ungroup() %>%
  filter(dup %in% c(0, 1))  # Filtra solo las observaciones con dup == 0 o dup == 1
table(train_nodups$dup)

#Precio por m2
train_nodups  <-train_nodups  %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0)) 
summary(train_nodups $precio_por_mt2) #promedio coherente con lo reportado por constructoras pero con valores atipicos
hist(train_nodups $precio_por_mt2)

train_nodups  %>%
  ggplot(aes(y = precio_por_mt2)) +
  geom_boxplot(fill = "darkblue", alpha = 0.4) +
  labs(
    y = "Precio por metro cuadrado", x = "") +
  theme_bw()

#Eliminar obs por encima y por debajo de 2SD
low <- round(mean(train_nodups$precio_por_mt2) - 2*sd(train_nodups$precio_por_mt2),2)
up <- round(mean(train_nodups$precio_por_mt2) + 2*sd(train_nodups$precio_por_mt2))






