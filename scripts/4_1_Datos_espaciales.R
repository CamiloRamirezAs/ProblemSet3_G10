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
       leaflet,
       nngeo
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


# 3. VISUALIZACIÓN PRECIO ------------------------------------------------------

# Importar los poligonos de las localidades
localidades <- st_read(file.path(raw_path, "Datos_Bogota", "15_localidades", "localidades.shp"))

# Eliminar localidad de Sumapaz
localidades <- localidades %>%
              filter(NOMBRE != "SUMAPAZ")

# Mapa de localidades
localidades <- st_transform(localidades,4686)

ggplot()+
  geom_sf(data=localidades, color = "blue3")

# Transformar base train a sf para graficar
sf_train<- st_as_sf(train_B, coords = c("lon", "lat"),  crs = 4686) # cambiar a test 

# Graficar apartamentos
mapa_precio_aptos <- ggplot()+
                  geom_sf(data=localidades, color = "#8B8989") + 
                  geom_sf(data=sf_train%>% filter(type_housing== "Apartamento"),aes(color = price) ,shape=15, size=0.3)+
                  labs(color = "Precio apartamentos") +  # <- título de la leyenda
                  theme_bw()

# Guardar mapa precios apartamentos
ggsave(
  filename = file.path(view_path, "mapa_precio_aptos.png"),   # nombre del archivo
  plot = mapa_precio_aptos,             # objeto del gráfico
  width = 10, height = 8,         # tamaño en pulgadas
  dpi = 300                       # resolución
)


# Graficar casas
mapa_precio_casa <- ggplot()+
                    geom_sf(data=localidades, color = "#8B8989") + 
                    geom_sf(data=sf_train%>% filter(type_housing== "Casa"),aes(color = price) ,shape=15, size=0.3)+
                    labs(color = "Precio casas") +  # <- título de la leyenda
                    theme_bw()

# Guardar mapa precios apartamentos
ggsave(
  filename = file.path(view_path, "mapa_precio_casa.png"),   # nombre del archivo
  plot = mapa_precio_casa,             # objeto del gráfico
  width = 10, height = 8,         # tamaño en pulgadas
  dpi = 300                       # resolución
)



# 4.1. CERCANÍA A PARQUES ------------------------------------------------------

# Importar los datos de parques
parques <- st_read(file.path(raw_path, "Datos_Bogota", "4_parques_esc_deportivos", "parques.shp"))

# Eliminar parques de bolsillo
parques <- parques[!(parques$TIPOPARQUE %in% c("PARQUE DE BOLSILLO", "PARQUE ZONAL PROPUESTO", "PARQUE METROPOLITANO PROPUESTO")), ]

# Calcular el centroide del parque
centroides_parques <- st_centroid(parques, byid = T)

centroides_parques <- centroides_parques %>%
                      mutate(x=st_coordinates(centroides_parques)[, "X"]) %>%
                      mutate(y=st_coordinates(centroides_parques)[, "Y"]) 


# Graficar parques
mapa_parques <- ggplot() +
                geom_sf(data = localidades, color = "#8B8989") + 
                geom_sf(data = centroides_parques, color = "green4", shape = 16, size = 0.5) +  # centroide con cruz
                labs(color = "Parques") +
                theme_bw()

# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)
centroides_sf_parque <- st_as_sf(centroides_parques, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_train_proj <- st_transform(sf_train, crs = 3116)
centroides_sf_parque_proj <- st_transform(centroides_sf_parque, crs = 3116)

# Finalmente calcular distancias
dist_matrix_parque <- st_distance(sf_train_proj, centroides_sf_parque_proj)
dim(dist_matrix_parque)

# Distancia minima a cualquier parque
dist_min_parque <- apply(dist_matrix_parque, 1, min)

# La agregamos como variable a nuestra base de datos original 
train_B <- train_B %>% mutate(dist_parque = dist_min_parque)

# Distribución de la nueva variable
parques_hist <- ggplot(train_B, aes(x = dist_parque)) +
                geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
                labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
                     title = "Distribución de la distancia a los parques") +
                theme_bw()
ggplotly(parques_hist)


# Relación con nuestra variable de interés
parques_cor <- ggplot(train_B%>%sample_n(1000), aes(x = dist_parque, y = price)) +
          geom_point(col = "darkblue", alpha = 0.4) +
          labs(x = "Distancia mínima a un parque en metros (log-scale)", 
               y = "Valor de venta  (log-scale)",
               title = "Relación entre la proximidad a un parque y el precio del immueble") +
          scale_x_log10() +
          scale_y_log10(labels = scales::dollar) +
          theme_bw()
ggplotly(parques_cor)



# 4.2. AVALUO CATASTRAL 2019 ---------------------------------------------------

# Importar los datos de avaluo catastral
catastral <- st_read(file.path(raw_path, "Datos_Bogota", "3_avaluo_catastral_manzana_2019", "Avaluo_catastral_Manzana.shp"))
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)

# Transformar bases al mismo CRS (EPSG:3857)
catastral <- st_transform(catastral, crs = 3116)
sf_train <- st_transform(sf_train, crs = 3116)

# Unir los predios con las manzanas catastrales
predios_avaluo <- st_join(sf_train, catastral["AVALUO_COM"])

# Identificar predios con valores faltantes de AVALUO_COM
predios_na <- predios_avaluo %>% 
  filter(is.na(AVALUO_COM))  # Predios con AVALUO_COM faltante

# Identificar predios con valores conocidos de AVALUO_COM
predios_conocidos <- predios_avaluo %>% 
  filter(!is.na(AVALUO_COM)) %>% 
  st_as_sf()  # Convertir en objeto sf si no lo es

# Calcular las distancias entre los predios faltantes y los conocidos
dist_matrix <- st_distance(st_as_sf(predios_na), predios_conocidos)

# Encontrar el índice del predio más cercano para cada predio faltante
indice_cercano <- apply(dist_matrix, 1, which.min)

# Asignar el valor de AVALUO_COM más cercano a los predios faltantes
predios_na <- predios_na %>%
            mutate(AVALUO_COM = predios_conocidos$AVALUO_COM[indice_cercano])

# Combinar los predios con valores completados con el resto
predios_avaluo <- predios_avaluo %>% 
                  filter(!is.na(AVALUO_COM)) %>%  # Mantener los predios con valores conocidos
                  bind_rows(predios_na)  # Agregar los predios con valores completados

# Convertir predios_avaluo a un data.frame para usar left_join
predios_avaluo_df <- predios_avaluo %>%
                    st_drop_geometry() %>%  # Eliminar la geometría
                    select(property_id, AVALUO_COM)  # Asegurarse de que solo están las columnas necesarias

# Unir predios_avaluo con train_B usando property_id como llave
train_B <- train_B %>%
          left_join(predios_avaluo_df, by = "property_id")

# Distribución de la nueva variable
avaluo_hist <- ggplot(train_B, aes(x = AVALUO_COM)) +
              geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
              labs(x = "Avaluo comercial", y = "Frecuencia",
                   title = "Avaluo comercial del metro cuadrado") +
              theme_bw()
ggplotly(avaluo_hist)

# Relación con nuestra variable de interés
avaluo_cor <- ggplot(train_B%>% sample_n(1000), aes(x = AVALUO_COM, y = price)) +
              geom_point(col = "darkblue", alpha = 0.4) +
              labs(x = "Avaluo comercial", 
                   y = "Valor de venta",
                   title = "Relación entre el precio del immueble y el avaluo comercial") +
              scale_x_log10() +
              scale_y_log10(labels = scales::dollar) +
              theme_bw()
ggplotly(avaluo_cor)



# 4.3. CERCANÍA A GRANDES CENTROS COMERCIALES ----------------------------------

# Importar los datos de centros comerciales
cc <- st_read(file.path(raw_path, "Datos_Bogota", "6_gran_centro_comercial", "Gran_centro_comercial_20211229.shp"))

# Calcular el centroide del centro comercial
centroides_cc <- st_centroid(cc, byid = T)

centroides_cc <- centroides_cc %>%
                mutate(x=st_coordinates(centroides_cc)[, "X"]) %>%
                mutate(y=st_coordinates(centroides_cc)[, "Y"]) 


# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)
centroides_sf_cc <- st_as_sf(centroides_cc, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_train_proj <- st_transform(sf_train, crs = 3116)
centroides_sf_cc_proj <- st_transform(centroides_sf_cc, crs = 3116)

# Finalmente calcular distancias
dist_matrix_cc <- st_distance(sf_train_proj, centroides_sf_cc_proj)
dim(dist_matrix_cc)

# Distancia midata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCCnima a cualquier parque
dist_min_cc <- apply(dist_matrix_cc, 1, min)

# La agregamos como variable a nuestra base de datos original 
train_B <- train_B %>% mutate(dist_centComercial = dist_min_cc)

# Distribución de la nueva variable
parques_cc <- ggplot(train_B, aes(x = dist_centComercial)) +
            geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
            labs(x = "Distancia mínima a un centro comercial", y = "Frecuencia",
                 title = "Distribución de la distancia a los centros comerciales") +
            theme_bw()
ggplotly(parques_cc)


# Relación con nuestra variable de interés
cc_cor <- ggplot(train_B%>%sample_n(1000), aes(x = dist_centComercial, y = price)) +
          geom_point(col = "darkblue", alpha = 0.4) +
          labs(x = "Distancia mínima a un Centro comercial en metros (log-scale)", 
               y = "Valor de venta  (log-scale)",
               title = "Relación entre la proximidad a un centro comercial y el precio del immueble") +
          scale_x_log10() +
          scale_y_log10(labels = scales::dollar) +
          theme_bw()
ggplotly(cc_cor)



# 4.4. CERCANÍA A LOS CAI ------------------------------------------------------

# Importar los datos de CAI
cai <- st_read(file.path(raw_path, "Datos_Bogota", "12_estaciones_policia_CAI", "ComandoAtencionInmediata.shp"))

# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)
sf_cai <- st_as_sf(cai, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_train_proj <- st_transform(sf_train, crs = 3116)
sf_cai_proj <- st_transform(sf_cai, crs = 3116)

# Finalmente calcular distancias
dist_matrix_cai <- st_distance(sf_train_proj, sf_cai_proj)
dim(dist_matrix_cai)

# Distancia minima a cualquier parque
dist_min_cai <- apply(dist_matrix_cai, 1, min)

# La agregamos como variable a nuestra base de datos original 
train_B <- train_B %>% mutate(dist_cai = dist_min_cai)

# Distribución de la nueva variable
cai_hist <- ggplot(train_B, aes(x = dist_cai)) +
              geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
              labs(x = "Distancia mínima a un CAI", y = "Frecuencia",
                   title = "Distribución de la distancia a los CAI") +
              theme_bw()
ggplotly(cai_hist)


# Relación con nuestra variable de interés
cai_cor <- ggplot(train_B%>%sample_n(1000), aes(x = dist_cai, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un CAI en metros", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a un CAI y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(cai_cor)



# 4.5. CERCANÍA A ESTACIÓN DE TRANSMILENIO -------------------------------------

# Importar los datos de Transmilenio
transmi <- st_read(file.path(raw_path, "Datos_Bogota", "10_estaciones_transmilenio", "Estaciones_Troncales_de_TRANSMILENIO.shp"))


# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)
sf_transmi <- st_as_sf(transmi, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_train_proj <- st_transform(sf_train, crs = 3116)
sf_transmi_proj <- st_transform(sf_transmi, crs = 3116)

# Finalmente calcular distancias
dist_matrix_transmi <- st_distance(sf_train_proj, sf_transmi_proj)
dim(dist_matrix_transmi)

# Distancia minima a cualquier parque
dist_min_transmi <- apply(dist_matrix_transmi, 1, min)

# La agregamos como variable a nuestra base de datos original 
train_B <- train_B %>% mutate(dist_transmi = dist_min_transmi)

# Distribución de la nueva variable
transmi_hist <- ggplot(train_B, aes(x = dist_transmi)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio", y = "Frecuencia",
       title = "Distribución de la distancia a las estaciones de Transmilenio") +
  theme_bw()
ggplotly(transmi_hist)


# Relación con nuestra variable de interés
transmi_cor <- ggplot(train_B%>%sample_n(1000), aes(x = dist_transmi, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a una estación de Transmilenio y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(transmi_cor)



# 4.6. NÚMERO DE ESTACIONES SITP -----------------------------------------------

# Importar los datos de estaciones del SITP
sitp <- st_read(file.path(raw_path, "Datos_Bogota", "11_paraderos_SITP", "Paraderos_Zonales_del_SITP.shp"))


# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)
sf_sitp <- st_as_sf(sitp, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_train_proj <- st_transform(sf_train, crs = 3116)
sf_sitp_proj <- st_transform(sf_sitp, crs = 3116)

# Crear un buffer de 300 metros alrededor de cada predio
buffer_300m <- st_buffer(sf_train_proj, dist = 300)

# Contar el número de paraderos dentro de cada buffer
num_paraderos <- sapply(1:nrow(buffer_300m), function(i) {
                sum(st_intersects(buffer_300m[i, ], sf_sitp_proj, sparse = FALSE))
                })

# Agregar el conteo de paraderos como una nueva variable al dataset train
train_B <- train_B %>% mutate(num_paraderos_sitp = num_paraderos)


# Distribución de la nueva variable
sitp_hist <- ggplot(train_B, aes(x = num_paraderos_sitp)) +
            geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
            labs(x = "Número de estaciones cercanas del SITP", y = "Frecuencia",
                 title = "Distribución del número de estaciones cercanas del SITP") +
            theme_bw()
ggplotly(sitp_hist)


# Relación con nuestra variable de interés
sitp_cor <- ggplot(train_B%>%sample_n(1000), aes(x = num_paraderos_sitp, y = price)) +
            geom_point(col = "darkblue", alpha = 0.4) +
            labs(x = "Número de estaciones cercanas del SITP", 
                 y = "Valor de venta  (log-scale)",
                 title = "Relación entre el número de estaciones del SITP y el precio del immueble") +
            scale_x_log10() +
            scale_y_log10(labels = scales::dollar) +
            theme_bw()
ggplotly(sitp_cor)


# 4.7. NÚMERO DE COLEGIOS CERCANOS ---------------------------------------------

# Importar los datos de colegios cercano
colegios <- st_read(file.path(raw_path, "Datos_Bogota", "13_colegios", "Colegios03_2024.shp"))

# Graficar las estaciones de Transmilenio
mapa_colegios <- ggplot() +
                geom_sf(data = localidades, color = "#8B8989") + 
                geom_sf(data = colegios, color = "turquoise4", shape = 16, size = 0.5) +  
                labs(color = "Colegios") +
                theme_bw()
ggplotly(mapa_colegios)

# Primero crear objetos sf con CRS MAGNA SIRGAS (lon/lat)
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)
sf_colegio <- st_as_sf(colegios, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3116 para distancias en metros
sf_train_proj <- st_transform(sf_train, crs = 3116)
sf_colegio_proj <- st_transform(sf_colegio, crs = 3116)

# Crear un buffer de 300 metros alrededor de cada predio
buffer_500m <- st_buffer(sf_train_proj, dist = 500)

# Contar el número de paraderos dentro de cada buffer
num_colegio <- sapply(1:nrow(buffer_500m), function(i) {
                  sum(st_intersects(buffer_500m[i, ], sf_colegio_proj, sparse = FALSE))
                })

# Agregar el conteo de paraderos como una nueva variable al dataset train
train_B <- train_B %>% mutate(num_colegios = num_colegio)


# Distribución de la nueva variable
colegio_hist <- ggplot(train_B, aes(x = num_colegios)) +
                geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
                labs(x = "Número de colegios cercanos", y = "Frecuencia",
                     title = "Distribución del número de colegios cercanos") +
                theme_bw()
ggplotly(colegio_hist)


# Relación con nuestra variable de interés
colegio_cor <- ggplot(train_B%>%sample_n(1000), aes(x = num_colegios, y = price)) +
              geom_point(col = "darkblue", alpha = 0.4) +
              labs(x = "Número de colegios cercanos", 
                   y = "Valor de venta  (log-scale)",
                   title = "Relación entre el número colegios cercanos y el precio del immueble") +
              scale_x_log10() +
              scale_y_log10(labels = scales::dollar) +
              theme_bw()
ggplotly(colegio_cor)


# 4.8. ESTRATIFICACIÓN MANZANA -------------------------------------------------

# Importar los datos de estratificación de manzanas
estrato <- st_read(file.path(raw_path, "Datos_Bogota", "1_manzana_estratificacion", "ManzanaEstratificacion.shp"))
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)

# Corregir geometría invalidas
#estrato <- st_make_valid(estrato)
#sf_train <- st_make_valid(sf_train)

# Transformar bases al mismo CRS (EPSG:3116) que fue hecho para Bogotá
estrato <- st_transform(estrato, crs = 3116)
sf_train <- st_transform(train_B, crs = 3116)

# Unir los predios con las manzanas catastrales
predios_estrato <- st_join(sf_train, estrato["ESTRATO"])

# Identificar predios con valores faltantes de Estrato
predios_na_estrato <- predios_estrato %>% filter(is.na(ESTRATO))
predios_con_estrato <- predios_estrato %>% filter(!is.na(ESTRATO))

# Crear un buffer de 500 metros alrededor de los predios con NA
buffer_global <- st_union(st_buffer(predios_na_estrato, dist = 400))

# Filtrar las manzanas dentro del buffer
manzanas_cercanas <- estrato[st_intersects(estrato, buffer_global, sparse = FALSE), ] %>%
                    filter(ESTRATO >= 1)

# Usar `nngeo::st_nn` para encontrar el vecino más cercano
vecinos <- st_nn(predios_na_estrato, manzanas_cercanas, k = 1, returnDist = TRUE)

# Asignar el estrato del vecino más cercano a los predios con NA
estrato_asignado <- sapply(vecinos$nn, function(indice) {
  if (length(indice) > 0) {
    return(manzanas_cercanas$ESTRATO[indice])
  } else {
    return(NA)
  }
})

# Actualizar los valores de estrato en los predios con NA
predios_na_estrato$ESTRATO <- estrato_asignado

# Combinar nuevamente los predios con y sin NA
predios_estrato_final <- bind_rows(predios_con_estrato, predios_na_estrato)

# Hacer el pegue a la base de train
predios_estrato_final <- predios_estrato_final %>% select(property_id, ESTRATO)
predios_estrato_final_df <- as.data.frame(predios_estrato_final)

train_B <- train_B %>%
          left_join(predios_estrato_final_df, by = "property_id")

# Distribución de la nueva variable
estrato_hist <- ggplot(train_B, aes(x = ESTRATO)) +
              geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
              labs(x = "Estrato", y = "Frecuencia",
                   title = "Estrato del predio") +
              theme_bw()
ggplotly(estrato_hist)

# Relación con nuestra variable de interés
estrato_cor <- ggplot(train_B%>% sample_n(1000), aes(x = ESTRATO, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Estrato", 
       y = "Valor de venta",
       title = "Relación entre el precio del immueble y estrato") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(estrato_cor)


mapa_predios <- ggplot() +
  geom_sf(data = train_B, aes(color = as.factor(ESTRATO)), size = 0.5) +  # Colorear según el estrato
  scale_color_viridis_d(name = "Estrato") +  # Escala de colores discreta
  theme_minimal() +  # Tema minimalista
  labs(
    title = "Mapa de Predios con Estrato",
    subtitle = "Visualización de estratos asignados a predios",
    x = "Longitud",
    y = "Latitud"
  )

# Mostrar el mapa
print(mapa_predios)


# 4.9. RESTAURANTES Y BARES CERCANOS -------------------------------------------

# Importar los datos de colegios cercano
restaurantes <- st_read(file.path(raw_path, "Datos_Bogota", "8_establecimiento_gastronomia_bar", "EGBa.shp"))

# Graficar las estaciones de Transmilenio
mapa_restaurantes <- ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = restaurantes, color = "orchid3", shape = 16, size = 0.5) +  
  labs(color = "Restaurantes") +
  theme_bw()
ggplotly(mapa_restaurantes)

# Primero crear objetos sf con CRS MAGNA SIRGAS (lon/lat)
sf_train <- st_as_sf(train_B, coords = c("lon", "lat"), crs = 4686)
sf_restaurantes <- st_as_sf(restaurantes, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3116 para distancias en metros
sf_train_proj <- st_transform(sf_train, crs = 3116)
sf_restaurantes_proj <- st_transform(sf_restaurantes, crs = 3116)

# Crear un buffer de 300 metros alrededor de cada predio
buffer_500m <- st_buffer(sf_train_proj, dist = 500)

# Contar el número de restaurantes y bares dentro de cada buffer
num_restaurante <- sapply(1:nrow(buffer_500m), function(i) {
                sum(st_intersects(buffer_500m[i, ], sf_restaurantes_proj, sparse = FALSE))
              })

# Agregar el conteo de paraderos como una nueva variable al dataset train
train_B <- train_B %>% mutate(num_restaurantes = num_restaurante)


# Distribución de la nueva variable
restaurantes_hist <- ggplot(train_B, aes(x = num_restaurantes)) +
                    geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
                    labs(x = "Número de restaurantes y bares cercanos", y = "Frecuencia",
                         title = "Distribución del número de restaurantes y bares cercanos") +
                    theme_bw()
ggplotly(restaurantes_hist)


# Relación con nuestra variable de interés
restaurantes_cor <- ggplot(train_B%>%sample_n(1000), aes(x = num_restaurantes, y = price)) +
                    geom_point(col = "darkblue", alpha = 0.4) +
                    labs(x = "Número de restaurantes y bares cercanos", 
                         y = "Valor de venta  (log-scale)",
                         title = "Relación entre el precio del immueble y el número de restaurantes y bares cercanos") +
                    scale_x_log10() +
                    scale_y_log10(labels = scales::dollar) +
                    theme_bw()
ggplotly(restaurantes_cor)



###############################################################################
# DATOS BASE TEST
###############################################################################

# 1. PREPROCESAMIENTO ----------------------------------------------------------

# Eliminamos las observaciones que no tienen información de latitud o longitud
test_B <- test %>%
  filter(!is.na(lat) & !is.na(lon))


# Primera visualización de los datos

mapa_inicial <- leaflet() %>%
  addTiles() %>%
  addCircles(lng = test_B$lon, 
             lat = test_B$lat)

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
test_B <- test_B %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )


# 2.1. CERCANÍA A PARQUES ------------------------------------------------------

# Importar los datos de parques
parques <- st_read(file.path(raw_path, "Datos_Bogota", "4_parques_esc_deportivos", "parques.shp"))

# Eliminar parques de bolsillo
parques <- parques[!(parques$TIPOPARQUE %in% c("PARQUE DE BOLSILLO", "PARQUE ZONAL PROPUESTO", "PARQUE METROPOLITANO PROPUESTO")), ]

# Calcular el centroide del parque
centroides_parques <- st_centroid(parques, byid = T)

centroides_parques <- centroides_parques %>%
  mutate(x=st_coordinates(centroides_parques)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_parques)[, "Y"]) 


# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
centroides_sf_parque <- st_as_sf(centroides_parques, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_test_proj <- st_transform(sf_test, crs = 3116)
centroides_sf_parque_proj <- st_transform(centroides_sf_parque, crs = 3116)

# Finalmente calcular distancias
dist_matrix_parque <- st_distance(sf_test_proj, centroides_sf_parque_proj)
dim(dist_matrix_parque)

# Distancia minima a cualquier parque
dist_min_parque <- apply(dist_matrix_parque, 1, min)

# La agregamos como variable a nuestra base de datos original 
test_B <- test_B %>% mutate(dist_parque = dist_min_parque)

# Distribución de la nueva variable
parques_hist <- ggplot(test_B, aes(x = dist_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(parques_hist)



# 4.2. AVALUO CATASTRAL 2019 ---------------------------------------------------

# Importar los datos de avaluo catastral
catastral <- st_read(file.path(raw_path, "Datos_Bogota", "3_avaluo_catastral_manzana_2019", "Avaluo_catastral_Manzana.shp"))
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)

# Transformar bases al mismo CRS (EPSG:3857)
catastral <- st_transform(catastral, crs = 3116)
sf_test <- st_transform(sf_test, crs = 3116)

# Unir los predios con las manzanas catastrales
predios_avaluo <- st_join(sf_test, catastral["AVALUO_COM"])

# Identificar predios con valores faltantes de AVALUO_COM
predios_na <- predios_avaluo %>% 
  filter(is.na(AVALUO_COM))  # Predios con AVALUO_COM faltante

# Identificar predios con valores conocidos de AVALUO_COM
predios_conocidos <- predios_avaluo %>% 
  filter(!is.na(AVALUO_COM)) %>% 
  st_as_sf()  # Convertir en objeto sf si no lo es

# Calcular las distancias entre los predios faltantes y los conocidos
dist_matrix <- st_distance(st_as_sf(predios_na), predios_conocidos)

# Encontrar el índice del predio más cercano para cada predio faltante
indice_cercano <- apply(dist_matrix, 1, which.min)

# Asignar el valor de AVALUO_COM más cercano a los predios faltantes
predios_na <- predios_na %>%
  mutate(AVALUO_COM = predios_conocidos$AVALUO_COM[indice_cercano])

# Combinar los predios con valores completados con el resto
predios_avaluo <- predios_avaluo %>% 
  filter(!is.na(AVALUO_COM)) %>%  # Mantener los predios con valores conocidos
  bind_rows(predios_na)  # Agregar los predios con valores completados

# Convertir predios_avaluo a un data.frame para usar left_join
predios_avaluo_df <- predios_avaluo %>%
  st_drop_geometry() %>%  # Eliminar la geometría
  select(property_id, AVALUO_COM)  # Asegurarse de que solo están las columnas necesarias

# Unir predios_avaluo con test_B usando property_id como llave
test_B <- test_B %>%
  left_join(predios_avaluo_df, by = "property_id")

# Distribución de la nueva variable
avaluo_hist <- ggplot(test_B, aes(x = AVALUO_COM)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Avaluo comercial", y = "Frecuencia",
       title = "Avaluo comercial del metro cuadrado") +
  theme_bw()
ggplotly(avaluo_hist)



# 4.3. CERCANÍA A GRANDES CENTROS COMERCIALES ----------------------------------

# Importar los datos de centros comerciales
cc <- st_read(file.path(raw_path, "Datos_Bogota", "6_gran_centro_comercial", "Gran_centro_comercial_20211229.shp"))

# Calcular el centroide del centro comercial
centroides_cc <- st_centroid(cc, byid = T)

centroides_cc <- centroides_cc %>%
  mutate(x=st_coordinates(centroides_cc)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_cc)[, "Y"]) 


# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
centroides_sf_cc <- st_as_sf(centroides_cc, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_test_proj <- st_transform(sf_test, crs = 3116)
centroides_sf_cc_proj <- st_transform(centroides_sf_cc, crs = 3116)

# Finalmente calcular distancias
dist_matrix_cc <- st_distance(sf_test_proj, centroides_sf_cc_proj)
dim(dist_matrix_cc)

# Distancia minima a cualquier parque
dist_min_cc <- apply(dist_matrix_cc, 1, min)

# La agregamos como variable a nuestra base de datos original 
test_B <- test_B %>% mutate(dist_centComercial = dist_min_cc)

# Distribución de la nueva variable
parques_cc <- ggplot(test_B, aes(x = dist_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un centro comercial", y = "Frecuencia",
       title = "Distribución de la distancia a los centros comerciales") +
  theme_bw()
ggplotly(parques_cc)



# 4.4. CERCANÍA A LOS CAI ------------------------------------------------------

# Importar los datos de CAI
cai <- st_read(file.path(raw_path, "Datos_Bogota", "12_estaciones_policia_CAI", "ComandoAtencionInmediata.shp"))

# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
sf_cai <- st_as_sf(cai, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_test_proj <- st_transform(sf_test, crs = 3116)
sf_cai_proj <- st_transform(sf_cai, crs = 3116)

# Finalmente calcular distancias
dist_matrix_cai <- st_distance(sf_test_proj, sf_cai_proj)
dim(dist_matrix_cai)

# Distancia minima a cualquier parque
dist_min_cai <- apply(dist_matrix_cai, 1, min)

# La agregamos como variable a nuestra base de datos original 
test_B <- test_B %>% mutate(dist_cai = dist_min_cai)

# Distribución de la nueva variable
cai_hist <- ggplot(test_B, aes(x = dist_cai)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un CAI", y = "Frecuencia",
       title = "Distribución de la distancia a los CAI") +
  theme_bw()
ggplotly(cai_hist)


# 4.5. CERCANÍA A ESTACIÓN DE TRANSMILENIO -------------------------------------

# Importar los datos de Transmilenio
transmi <- st_read(file.path(raw_path, "Datos_Bogota", "10_estaciones_transmilenio", "Estaciones_Troncales_de_TRANSMILENIO.shp"))

# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
sf_transmi <- st_as_sf(transmi, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_test_proj <- st_transform(sf_test, crs = 3116)
sf_transmi_proj <- st_transform(sf_transmi, crs = 3116)

# Finalmente calcular distancias
dist_matrix_transmi <- st_distance(sf_test_proj, sf_transmi_proj)
dim(dist_matrix_transmi)

# Distancia minima a cualquier parque
dist_min_transmi <- apply(dist_matrix_transmi, 1, min)

# La agregamos como variable a nuestra base de datos original 
test_B <- test_B %>% mutate(dist_transmi = dist_min_transmi)

# Distribución de la nueva variable
transmi_hist <- ggplot(test_B, aes(x = dist_transmi)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio", y = "Frecuencia",
       title = "Distribución de la distancia a las estaciones de Transmilenio") +
  theme_bw()
ggplotly(transmi_hist)


# 4.6. NÚMERO DE ESTACIONES SITP -----------------------------------------------

# Importar los datos de estaciones del SITP
sitp <- st_read(file.path(raw_path, "Datos_Bogota", "11_paraderos_SITP", "Paraderos_Zonales_del_SITP.shp"))

# Primero crear objetos sf con CRS WGS84 (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
sf_sitp <- st_as_sf(sitp, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3857 para distancias en metros
sf_test_proj <- st_transform(sf_test, crs = 3116)
sf_sitp_proj <- st_transform(sf_sitp, crs = 3116)

# Crear un buffer de 300 metros alrededor de cada predio
buffer_300m <- st_buffer(sf_test_proj, dist = 300)

# Contar el número de paraderos dentro de cada buffer
num_paraderos <- sapply(1:nrow(buffer_300m), function(i) {
  sum(st_intersects(buffer_300m[i, ], sf_sitp_proj, sparse = FALSE))
})

# Agregar el conteo de paraderos como una nueva variable al dataset test
test_B <- test_B %>% mutate(num_paraderos_sitp = num_paraderos)


# Distribución de la nueva variable
sitp_hist <- ggplot(test_B, aes(x = num_paraderos_sitp)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Número de estaciones cercanas del SITP", y = "Frecuencia",
       title = "Distribución del número de estaciones cercanas del SITP") +
  theme_bw()
ggplotly(sitp_hist)



# 4.7. NÚMERO DE COLEGIOS CERCANOS ---------------------------------------------

# Importar los datos de colegios cercano
colegios <- st_read(file.path(raw_path, "Datos_Bogota", "13_colegios", "Colegios03_2024.shp"))

# Primero crear objetos sf con CRS MAGNA SIRGAS (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
sf_colegio <- st_as_sf(colegios, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3116 para distancias en metros
sf_test_proj <- st_transform(sf_test, crs = 3116)
sf_colegio_proj <- st_transform(sf_colegio, crs = 3116)

# Crear un buffer de 300 metros alrededor de cada predio
buffer_500m <- st_buffer(sf_test_proj, dist = 500)

# Contar el número de paraderos dentro de cada buffer
num_colegio <- sapply(1:nrow(buffer_500m), function(i) {
  sum(st_intersects(buffer_500m[i, ], sf_colegio_proj, sparse = FALSE))
})

# Agregar el conteo de paraderos como una nueva variable al dataset test
test_B <- test_B %>% mutate(num_colegios = num_colegio)


# Distribución de la nueva variable
colegio_hist <- ggplot(test_B, aes(x = num_colegios)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Número de colegios cercanos", y = "Frecuencia",
       title = "Distribución del número de colegios cercanos") +
  theme_bw()
ggplotly(colegio_hist)


# 4.8. ESTRATIFICACIÓN MANZANA -------------------------------------------------

# Importar los datos de estratificación de manzanas
estrato <- st_read(file.path(raw_path, "Datos_Bogota", "1_manzana_estratificacion", "ManzanaEstratificacion.shp"))
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)

# Corregir geometría invalidas
#estrato <- st_make_valid(estrato)
#sf_test <- st_make_valid(sf_test)

# Primero crear objetos sf con CRS MAGNA SIRGAS (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
sf_estrato <- st_as_sf(estrato, coords = c("x", "y"), crs = 4686)

# Transformar bases al mismo CRS (EPSG:3116) que fue hecho para Bogotá
estrato <- st_transform(sf_estrato, crs = 3116)
sf_test <- st_transform(sf_test, crs = 3116)

# Unir los predios con las manzanas catastrales
predios_estrato <- st_join(sf_test, estrato["ESTRATO"])

# Identificar predios con valores faltantes de Estrato
predios_na_estrato <- predios_estrato %>% filter(is.na(ESTRATO))
predios_con_estrato <- predios_estrato %>% filter(!is.na(ESTRATO))

# Crear un buffer de 500 metros alrededor de los predios con NA
buffer_global <- st_union(st_buffer(predios_na_estrato, dist = 400))

# Filtrar las manzanas dentro del buffer
manzanas_cercanas <- estrato[st_intersects(estrato, buffer_global, sparse = FALSE), ] %>%
  filter(ESTRATO >= 1)

# Usar `nngeo::st_nn` para encontrar el vecino más cercano
vecinos <- st_nn(predios_na_estrato, manzanas_cercanas, k = 1, returnDist = TRUE)

# Asignar el estrato del vecino más cercano a los predios con NA
estrato_asignado <- sapply(vecinos$nn, function(indice) {
  if (length(indice) > 0) {
    return(manzanas_cercanas$ESTRATO[indice])
  } else {
    return(NA)
  }
})

# Actualizar los valores de estrato en los predios con NA
predios_na_estrato$ESTRATO <- estrato_asignado

# Combinar nuevamente los predios con y sin NA
predios_estrato_final <- bind_rows(predios_con_estrato, predios_na_estrato)

# Hacer el pegue a la base de test
predios_estrato_final <- predios_estrato_final %>% select(property_id, ESTRATO)
predios_estrato_final_df <- as.data.frame(predios_estrato_final)

test_B <- test_B %>%
  left_join(predios_estrato_final_df, by = "property_id")

# Distribución de la nueva variable
estrato_hist <- ggplot(test_B, aes(x = ESTRATO)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Estrato", y = "Frecuencia",
       title = "Estrato del predio") +
  theme_bw()
ggplotly(estrato_hist)



# 4.9. RESTAURANTES Y BARES CERCANOS -------------------------------------------

# Importar los datos de colegios cercano
restaurantes <- st_read(file.path(raw_path, "Datos_Bogota", "8_establecimiento_gastronomia_bar", "EGBa.shp"))


# Primero crear objetos sf con CRS MAGNA SIRGAS (lon/lat)
sf_test <- st_as_sf(test_B, coords = c("lon", "lat"), crs = 4686)
sf_restaurantes <- st_as_sf(restaurantes, coords = c("x", "y"), crs = 4686)

# Luego transformar ambos a EPSG:3116 para distancias en metros
sf_test_proj <- st_transform(sf_test, crs = 3116)
sf_restaurantes_proj <- st_transform(sf_restaurantes, crs = 3116)

# Crear un buffer de 300 metros alrededor de cada predio
buffer_500m <- st_buffer(sf_test_proj, dist = 500)

# Contar el número de restaurantes y bares dentro de cada buffer
num_restaurante <- sapply(1:nrow(buffer_500m), function(i) {
  sum(st_intersects(buffer_500m[i, ], sf_restaurantes_proj, sparse = FALSE))
})

# Agregar el conteo de paraderos como una nueva variable al dataset test
test_B <- test_B %>% mutate(num_restaurantes = num_restaurante)


# Distribución de la nueva variable
restaurantes_hist <- ggplot(test_B, aes(x = num_restaurantes)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Número de restaurantes y bares cercanos", y = "Frecuencia",
       title = "Distribución del número de restaurantes y bares cercanos") +
  theme_bw()
ggplotly(restaurantes_hist)


#######################################
# EXPORTAR BASES
#######################################

# Dejar solo las variables espaciales en train
train_final <- as.data.frame(train_B)

train_final <- train_B %>% 
              select(-lon, -lat, -geometry.x, -geometry.y, -description_adj, -gym, -balcon, -seguridad, -garaje, -piscina, -description_adj, -type_housing, -bathrooms, -bedrooms, -surface_total, -year, -month, -city, -price)


# Dejar solo las variables espaciales en train
test_final <- as.data.frame(test_B)

test_final <- test_final %>%
               select(-lon, -lat, -geometry,  -gym, -balcon, -seguridad, -garaje, -piscina, -description_adj, -type_housing, -bathrooms, -bedrooms, -surface_total, -year, -month, -city)


# Guardar los archivos en formato .rds en la carpeta stores
saveRDS(train_final, file.path(stores_path, "spatial_train.rds"))
saveRDS(test_final, file.path(stores_path, "spatial_test.rds"))

# Mensaje de proceso realizado
message(("✅ Bases guardadas en "), stores_path)



