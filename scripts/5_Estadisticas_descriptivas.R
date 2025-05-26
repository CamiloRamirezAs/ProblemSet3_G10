#-----------------------------------------------------------------------------//
# Estadísticas descriptivas
# Problem Set 3 G10 - BDML 202501
# Fecha: 3 de mayo 2025
#-----------------------------------------------------------------------------//

#---------
# Train 
#---------

train_data =  readRDS(file.path(stores_path, "train_data.rds"))
train_spatial =  readRDS(file.path(stores_path, "spatial_train.rds"))
train = merge(train_data, train_spatial, by = c("property_id"), all = T)
train = train %>% st_as_sf(coords = c("lon", "lat"), crs = 4686)

#---------
# Test 
#---------

test_data =  readRDS(file.path(stores_path, "test_data.rds"))
test_spatial =  readRDS(file.path(stores_path, "spatial_test.rds"))
test = merge(test_data, test_spatial, by = c("property_id"), all = T)
test = test %>% st_as_sf(coords = c("lon", "lat"), crs = 4686)

#-----------------------------------------------------------------------------//
# Ubicación en Bogotá - Precios
#-----------------------------------------------------------------------------//
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

mapa_ubicacion_train <- ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = train, color = "black", shape = 15, size = 0.3) +
  theme_bw()

mapa_ubicacion_test <- ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = test, color = "black", shape = 15, size = 0.3) +
  theme_bw()

library(cowplot)

plot_grid(mapa_ubicacion_train, mapa_ubicacion_test, 
          labels = c("Train", "Test"), 
          ncol = 2)


