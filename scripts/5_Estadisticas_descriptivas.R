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
# 1. Ubicación en Bogotá ----
#-----------------------------------------------------------------------------//

#----------------------------------------------
# 1.1 Importar los poligonos de las localidades ----
#----------------------------------------------

localidades <- st_read(file.path(raw_path, "Datos_Bogota", "15_localidades", "localidades.shp"))
localidades <- localidades %>% filter(NOMBRE != "SUMAPAZ")
localidades <- st_transform(localidades,4686)

ggplot() + geom_sf(data=localidades, color = "blue3")

#----------------------------------------------
# 1.2 Lugares clave ----
#----------------------------------------------

Centro_internacional = geocode_OSM("Centro internacional", as.sf = TRUE)
Unicentro = geocode_OSM("Unicentro, Avenida Calle 127", as.sf = TRUE)

lugares_clave <- list(
  geom_sf(data = Unicentro,   color = "red", size = 1), 
  geom_sf(data = Centro_internacional,   color = "red", size = 1))

#----------------------------------------------
# 1.2 Mapa por estrato ----
#----------------------------------------------

mapa_estrato_train = ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = train, aes(color = as.factor(ESTRATO)), shape = 15, size = 0.3) +
  lugares_clave + 
  labs(color = "Estrato") +  
  guides(color = guide_legend(override.aes = list(shape = 15, size = 3))) +
  scale_color_brewer(palette = "Blues", direction = 1) +  # Escala azul
  theme_classic()

mapa_estrato_test = ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = test, aes(color = as.factor(ESTRATO)), shape = 15, size = 0.3) +
  lugares_clave + 
  scale_color_brewer(palette = "Blues", direction = 1) +  # Escala azul
  labs(color = "Estrato") +  
  guides(color = guide_legend(override.aes = list(shape = 15, size = 3))) +
  theme_classic()

mapa_estrato = plot_grid(mapa_estrato_train, mapa_estrato_test, 
               labels = c("Train", "Test"), 
               ncol = 2)
mapa_estrato
# Guardar gráfico 

ggsave(file.path(paste0(view_path, "/mapa_estrato.png")), 
       plot = mapa_estrato, width = 10, height = 6, dpi = 300)

#----------------------------------------------
# 1.2 Mapa por precio (test) ----
#----------------------------------------------

mapa_casa_precio = ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = train %>% filter(type_housing== "Casa"), aes(color = price/1000000),
          shape = 15, size = 0.3) +
  lugares_clave + 
  labs(color = "Precio (millones)") +  
  #guides(color = guide_legend(override.aes = list(shape = 15, size = 3))) +
  theme_classic()

mapa_apto_precio = ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = train %>% filter(type_housing== "Apartamento"), aes(color = price/1000000),
          shape = 15, size = 0.3) +
  lugares_clave + 
  labs(color = "Precio (millones)") +  
  #guides(color = guide_legend(override.aes = list(shape = 15, size = 3))) +
  theme_classic()

mapa_precio = plot_grid(mapa_casa_precio, mapa_apto_precio, 
                         labels = c("Casa", "Apto"), 
                         ncol = 2)
mapa_precio
# Guardar gráfico 

ggsave(file.path(paste0(view_path, "/mapa_precio.png")), 
       plot = mapa_precio, width = 10, height = 6, dpi = 300)

#----------------------------------------------
# 1.2 Mapa predial por base
#----------------------------------------------

p99_train <- quantile(train$AVALUO_COM, 0.99, na.rm = TRUE)

mapa_predial_train = ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = train, aes(color = AVALUO_COM/1000),
          shape = 15, size = 0.3) +
  lugares_clave + 
  scale_color_gradientn(
    colors = RColorBrewer::brewer.pal(9, "Blues"),
    limits = c(0, p99_train/1000),
    oob = squish,  
    name = "Predial (miles)"
  ) +
  theme_classic()

p99_test <- quantile(test$AVALUO_COM, 0.99, na.rm = TRUE)

mapa_predial_test = ggplot() +
  geom_sf(data = localidades, color = "#8B8989") + 
  geom_sf(data = test, aes(color = AVALUO_COM/1000),
          shape = 15, size = 0.3) +
  lugares_clave + 
  scale_color_gradientn(
    colors = RColorBrewer::brewer.pal(9, "Blues"),
    limits = c(0, p99_test/1000),
    oob = squish,  
    name = "Predial (miles)") +
  theme_classic()

mapa_predial = plot_grid(mapa_predial_train, mapa_predial_test, 
                        labels = c("Train", "Test"), 
                        ncol = 2)
mapa_predial
# Guardar gráfico 

ggsave(file.path(paste0(view_path, "/mapa_predial.png")), 
       plot = mapa_predial, width = 10, height = 6, dpi = 300)

#----------------------------------------------
# 2. Tabla test
#----------------------------------------------

skim(train)
skim(test)

#----------------------------------------------
# 2.1 Nombres y union train y test
#----------------------------------------------

variables = c("surface_total", "bedrooms", "bathrooms", 
              "piscina", "garaje", "seguridad", "balcon", 
              "gym", 
              "AVALUO_COM",
              "dist_parque","dist_centComercial", "dist_cai", "dist_transmi", 
              "num_paraderos_sitp", "num_colegios", 
              "num_restaurantes", "ESTRATO")

variables_train = train %>%
  dplyr::select(all_of(variables))  %>%
  st_drop_geometry() %>%
  mutate(base = "Train", 
         AVALUO_COM = AVALUO_COM/1000) %>%
  dummy_cols(select_columns = "ESTRATO", 
             remove_first_dummy = FALSE, remove_selected_columns = TRUE)

variables_test = test %>%
  dplyr::select(all_of(variables))  %>%
  st_drop_geometry() %>%
  mutate(base = "Test", 
         AVALUO_COM = AVALUO_COM/1000) %>%
  dummy_cols(select_columns = "ESTRATO", 
             remove_first_dummy = FALSE, remove_selected_columns = TRUE)

colnames(variables_train)
colnames(variables_test)
setdiff(colnames(variables_train), colnames(variables_test))
variables_gen = rbind(variables_train, variables_test)
colnames(variables_test)

vars_names = c("Superficie total (m2)", 
               "Habitaciones", 
               "Baños", 
               "Piscina", 
               "Garaje", 
               "Seguridad", "Balcón", 
               "Gimnasio",
               
               "Predial con avalúo comercial (miles COP)", 
               
               "Distancia parque (mts)", 
               "Distancia centro comercial (mts)", 
               "Distancia CAI (mts)", 
               "Distancia estación TM (mts)", 
               "Número paraderos SITP (300m)", 
               "Número de colegios (500m)", 
               "Número de restaurantes (500m)",
               "Base fuente",
               "Estrato NA",
               "Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4", "Estrato 5", 
               "Estrato 6")

table(train$ESTRATO)
names(variables_gen) = vars_names
colnames(variables_gen)
summary(variables_gen)

colnames(variables_train_com)

#----------------------------------------------
# 2.2 Tabla train
#----------------------------------------------

vars_names_1 <- c("Precio", vars_names[vars_names != "Base fuente"])

variables_train_com = train %>%
                      dplyr::select(price, all_of(variables))  %>%
                      st_drop_geometry() %>%
                      mutate(base = "Train", 
                             AVALUO_COM = AVALUO_COM/1000, 
                             price = price/1000000) %>%
                      dummy_cols(select_columns = "ESTRATO", 
                                 remove_first_dummy = FALSE, remove_selected_columns = TRUE) 

colnames(variables_train_com) <- gsub("_", ".", colnames(variables_train_com)) 

resumen <- variables_train_com %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    #N = ~sum(!is.na(.x)),
    Media = ~mean(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE),
    Min = ~min(.x, na.rm = TRUE),
    P25 = ~quantile(.x, 0.25, na.rm = TRUE),
    P50 = ~quantile(.x, 0.50, na.rm = TRUE),
    P75 = ~quantile(.x, 0.75, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))


resumen_t <- as.data.frame(t(resumen))
resumen_t$Variable <- rownames(resumen_t)

resumen_t <- resumen_t %>%
  separate(Variable, into = c("Variable", "Estadístico"), sep = "_") %>%
  pivot_wider(names_from = Estadístico, values_from = V1)

rownames(resumen_t) = vars_names_1
resumen_t$Variable = vars_names_1

resumen_t <- resumen_t %>%
  relocate(Variable, .before = everything())

xtab <- xtable(resumen_t, caption = "Resumen estadístico de variables numéricas")
print(xtab, file = file.path(view_path, "tabla_estadsiticas_train.tex"), include.rownames = FALSE, digits = 2)


#----------------------------------------------
# 2. Tabla balance
#----------------------------------------------

tab = CreateTableOne(vars = vars_names, smd = F, strata = "Base fuente", data = variables_gen, test = T)
print(tab, format = "text")

# Extraer data.frame imprimible
tab_df = print(tab, printToggle = FALSE)
rownames(tab_df) = gsub(" \\(mean \\(SD\\)\\)", "", rownames(tab_df))

xtab_2 <- xtable(tab_df, caption = "Resumen train y test")
print(xtab_2, file = file.path(view_path, "tabla_balance.tex"), include.rownames = FALSE, digits = 2)


