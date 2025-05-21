
#------------------------------------------------------------------------------#
#Analisis Espacial
#------------------------------------------------------------------------------#

#Cargar datos
train <- readRDS(file.path(stores_path, "train_data.rds"))
lim_chapinero <- st_read(file.path(raw_path, "Datos_Bogota", "15_poligonos-localidades", "poligonos-localidades.shp"))

#------------------------------------------------------------------------------#
#Train
#------------------------------------------------------------------------------#

#Revisar distribución de propiedades en train
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

#Excluir localidades fuera del poligno de chapinero
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = st_crs(lim_chapinero))
train <- train_sf[!st_within(train_sf, lim_chapinero, sparse = FALSE), ]

#Revisar si hay propiedades fuera de bogota
limites_bogota <- getbb("Bogota Colombia")

# Extraer coordenadas desde la geometría
train <- train %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

#No hay propiedades fuera de Bogota
train <- train %>%
  filter(
    between(lon, limites_bogota[1, "min"], limites_bogota[1, "max"]) & 
      between(lat, limites_bogota[2, "min"], limites_bogota[2, "max"])
  )

saveRDS(train, file.path(stores_path, "train_data.rds"))

