
#-----------------------------------------------------------------------------//
# Importar datos
# Problem Set 3 
# Fecha: 3 de mayo de 2025
#-----------------------------------------------------------------------------//

# 1. IMPORTAR DATOS ------------------------------------------------------------

# Ruta con las bases de datos en ZIP
zip_path <- file.path(raw_path, "uniandes-bdml-2025-10-ps-3.zip")

# Listar los archivos dentro del ZIP para verificar la estructura
zip_files <- zip::zip_list(zip_path)
print(zip_files$file)  # Para ver cómo están organizados dentro del ZIP

# Definir los nombres correctos de los archivos dentro del ZIP
test <- read_csv(unz(zip_path, "test.csv"))
sapply(test, class)

#Revisar missings
vis_dat(test)
skim(test)

# 2. PROPIEDADES DUPLICADAS  ---------------------------------------------------

#Identificar propiedades duplicadas y dejar solo una
test <- test %>%
  group_by(year, month, surface_total, surface_covered, rooms, bedrooms, bathrooms, price, lat, lon) %>%
  mutate(dup = if_else(n() > 1, 1, 0)) %>%
  ungroup()
table(test$dup)

test_nodups <- test %>%
  group_by(year, month, surface_total, surface_covered, rooms, bedrooms, bathrooms, price, lat, lon) %>%
  slice_head(n = 1) %>%  
  ungroup()

#Identificar propiedades que se publican varias veces en distintos momentos y dejar la más reciente
test_nodups <- test %>%
  group_by(surface_total, surface_covered, rooms, bedrooms, bathrooms, lat, lon) %>%
  arrange(desc(year), desc(month)) %>% 
  mutate(dup = case_when(
    n() > 1 ~ row_number(),  # Si hay duplicados, asigna un número secuencial (1 será la más reciente)
    TRUE ~ 0                 # Si no hay duplicados, asigna 0
  )) %>%
  ungroup() %>%
  filter(dup %in% c(0, 1))  
table(test_nodups$dup)

# 3. lIMPIEZA DATOS: CORREGIR VARIABLES EXISTENTES Y CREAR NUEVAS -------------

# Tildes y caracteres especiales del español
test_nodups$description_adj <- stri_trans_general(str = test_nodups$description, id = "Latin-ASCII")

# Minuscula
test_nodups$description_adj <- tolower(test_nodups$description)

# Espacios
test_nodups$description_adj <- gsub("\\s+", " ", test_nodups$description_adj)

### Extraer la superficie de la propiedad de la descripcion (surface_adj)
# Definir palabras clave a excluir
exclusion_words <- c("terraza", "balc[oó]n", "balcn", "patio", "chimenea",
                     "deposito", "jardin", "garaje", "parqueadero", "altillo",
                     "piscina", "gimnasio", "alcoba", "habitacion", "bano",
                     "garajes", "parque", "balcones", "bbq", "altura", "terrazas")

test_nodups <- test_nodups %>%
  mutate(
    # Extraer hasta 5 palabras antes y después del match
    contexto = str_extract(
      description,
      paste0(
        "(\\b(?:\\w+\\b\\W+){0,5})", # hasta 5 palabras antes
        "(\\b\\d+(\\.\\d+)?\\s?(m\\^?\\d{1,2}|m²|m2|mts2|mts|mt2|mtr?s?|mtrs2|mtrs|metrs2?|metros cuadrados?|mas|ms|m\\d{1,2}|m))", # unidades de superficie
        "((\\W+\\b\\w+\\b){0,5})" # hasta 5 palabras después
      )
    ),
    
    # Extraer número + unidad de superficie
    surface_raw = str_extract(
      contexto,
      "\\b(\\d+(\\.\\d+)?)\\s?(m\\^?\\d{1,2}\\b|m²\\b|m2\\b|mts2\\b|mts\\b|mt2\\b|mtr?s?\\b|mtrs2\\b|mtrs\\b|metrs2?\\b|metros cuadrados?\\b|mt\\b|\\bm\\b)"
      
    ),
    
    surface_adj = as.numeric(str_extract(surface_raw, "\\d+(\\.\\d+)?")),
    
    # Filtrar contextos que contengan palabras de exclusión
    surface_adj = if_else(
      str_detect(contexto, regex(str_c("\\b(", str_c(exclusion_words, collapse = "|"), ")\\b"), ignore_case = TRUE)),
      NA_real_,
      surface_adj
    )
  )

summary(test_nodups$surface_adj)

#Cuando superficie esta en forma x frente x fondo (area)
test_nodups <- test_nodups %>%
  mutate(
    # Extraer dimensiones con formato "número unidad x número unidad"
    dimension = str_extract(
      description_adj,
      "\\b(\\d+(\\.\\d+)?)(\\s*(m(²|2)?|mts?|mt2?|mtr?s?|metrs?|metros?|mts2|m2|\\*))?\\s*(de)?\\s*(frente|frente\\s+norte)?\\s*(de)?\\s*(x|por|\\*)\\s*(\\d+(\\.\\d+)?)(\\s*(m(²|2)?|mts?|mt2?|mtr?s?|metrs?|metros?|mts2|m2|\\*))?\\s*(de)?\\s*(fondo)?\\b"
    ),
    
    # Eliminar dimensiones tipo "24x7" o "7x24" (vigilancia)
    dimension = if_else(
      str_detect(dimension, "^\\s*24\\s*[xX]\\s*7\\s*$|^\\s*7\\s*[xX]\\s*24\\s*$"),
      NA_character_,
      dimension
    ),
    
    # Extraer ambos números usando una expresión que separe primero y segundo número
    dim1 = as.numeric(str_extract(dimension, "^\\s*(\\d+(\\.\\d+)?)")),
    dim2 = as.numeric(str_match(dimension, "(?i)(?:x|por|\\*)\\s*(\\d+(\\.\\d+)?)")[,2]),
    
    # Calcular área
    area = dim1 * dim2
  )

#buscar dentro de descripcion -> "area construida"
test_nodups <- test_nodups %>%
  mutate(
    area_etiquetada = str_extract(
      description_adj,
      paste0(
        "\\b(area|área|superficie)\\s+(total\\s+)?(construida|construido)\\b\\s*",  # frase clave
        "\\d+(\\.\\d+)?\\s*(m²|m2|mts2|mts|mt2|metros cuadrados?)"  # número + unidad
      )
    ),
    
    area_construida = as.numeric(str_extract(area_etiquetada, "\\d+(\\.\\d+)?"))
  )

###Reemplazos
#1. Asignar el valor de 'area_construida' cuando surface_total es NA
test_nodups$surface_total <- ifelse(
  is.na(test_nodups$surface_total),  
  test_nodups$area_construida,      
  test_nodups$surface_total         
)

#2. Asignar el valor de 'surface_adj' cuando existe
test_nodups$surface_total <- ifelse(
  is.na(test_nodups$surface_total) & test_nodups$surface_adj > 25, 
  test_nodups$surface_adj,  
  test_nodups$surface_total
)

#3. Asignar el valor de 'area' cuando existe
test_nodups$surface_total <- ifelse(
  is.na(test_nodups$surface_total) & test_nodups$area > 25, 
  test_nodups$area,  
  test_nodups$surface_total
)

summary(test_nodups$surface_total) ##4022 NA

###. Extraer tipo de vivienda de la descripcion: Casa o Apartamento -
keywords_apartamento <- c(
  "apartamento", "apto", "apt", "paratmento", "aparta estudio", 
  "apartamentos", "apartaestudio", "aparatmento", "penthouse", 
  "peth house", "edificio", "paratmento",  "pent house", 
  "apartanento", "aptos", "aptaestudio", "apartamneto", 
  "penhouse", "apatamento", "apta", "aptestudio"
)

#Buscar en "title" el tipo de propiedad
test_nodups <- test_nodups %>%
  mutate(type_housing = case_when(
    str_detect(str_to_lower(title), "casa") ~ "Casa", 
    str_detect(str_to_lower(title), str_c("\\b(", str_c(keywords_apartamento, collapse = "|"), ")\\b")) ~ "Apartamento",
    TRUE ~ "otro"
  ))

#Buscar en "descripcion" el tipo de propiedad
test_nodups <- test_nodups %>%
  mutate(type_housing_2 = case_when(
    str_detect(str_to_lower(description), "casa") ~ "Casa", 
    str_detect(str_to_lower(description), str_c("\\b(", str_c(keywords_apartamento, collapse = "|"), ")\\b")) ~ "Apartamento",
    TRUE ~ "otro"
  ))

# Completar "tipo_vivienda" con info de la descripción si estaba como "otro"
test_nodups <- test_nodups %>%
  mutate(type_housing = if_else(
    type_housing == "otro",
    type_housing_2,
    type_housing
  ))

# Cuando no logro identificar asigno el de la variable original (76 casos)
test_nodups <- test_nodups %>%
  mutate(type_housing = if_else(
    type_housing == "otro",
    property_type,
    type_housing
  ))

table(test_nodups$type_housing)
table(test_nodups$property_type)

###. Extraer # de baños
test_nodups <- test_nodups %>%
  mutate(
    # Extraer número de baños con formato numérico o en palabras
    num_bathrooms = str_extract(description_adj, "(?i)(\\d+)\\s*(baños?|banos?)|(?:uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\\s*(baños?|banos?)")
  ) %>%
  mutate(
    # Convertir el número en palabras a su equivalente numérico
    num_bathrooms = case_when(
      str_detect(num_bathrooms, "(?i)uno") ~ 1,
      str_detect(num_bathrooms, "(?i)dos") ~ 2,
      str_detect(num_bathrooms, "(?i)tres") ~ 3,
      str_detect(num_bathrooms, "(?i)cuatro") ~ 4,
      str_detect(num_bathrooms, "(?i)cinco") ~ 5,
      str_detect(num_bathrooms, "(?i)seis") ~ 6,
      str_detect(num_bathrooms, "(?i)siete") ~ 7,
      str_detect(num_bathrooms, "(?i)ocho") ~ 8,
      str_detect(num_bathrooms, "(?i)nueve") ~ 9,
      str_detect(num_bathrooms, "(?i)diez") ~ 10,
      TRUE ~ as.numeric(str_extract(num_bathrooms , "\\d+"))
    )
  ) %>%
  mutate(
    # Asignar NA si el número de baños es mayor a 22
    num_bathrooms = if_else(num_bathrooms >= 22, NA_real_, num_bathrooms)
  )

table(is.na(test_nodups$bathrooms))

# Asignar el valor de 'num_bathrooms' cuando bathrooms es NA
test_nodups <- test_nodups %>%
  mutate( bathrooms = if_else(
    is.na(bathrooms), 
    num_bathrooms, 
    bathrooms
  )
  )

###Dummy de presencia de piscina
test_nodups$piscina <- ifelse(agrepl("piscina", test_nodups$description_adj, max.distance = 1), 1, 0)

###Dummy de presencia de garaje
test_nodups$garaje <- ifelse(
  agrepl("garaje", test_nodups$description_adj, max.distance = 1) |
    agrepl("parqueadero", test_nodups$description_adj, max.distance = 1),
  1, 0
)

###Dummy de vigilancia
test_nodups$seguridad <- ifelse(
  agrepl("vigilancia", test_nodups$description_adj, max.distance = 1) |
    agrepl("seguridad", test_nodups$description_adj, max.distance = 1),
  1, 0
)

##Dummy de balcon
test_nodups$balcon <- ifelse(
  agrepl("balcon", test_nodups$description_adj, max.distance = 1) |
    agrepl("terraza", test_nodups$description_adj, max.distance = 1),
  1, 0
)

##Dummy de gimnasio
test_nodups$gym <- ifelse(
  agrepl("gimnasio", test_nodups$description_adj, max.distance = 1) |
    agrepl("gym", test_nodups$description_adj, max.distance = 1),
  1, 0
)

#4.IMPUTACION   --------------------------------------------------------------

#Para evitar data leakage imputamos en test los mismos valores para los missings
test_nodups$bathrooms[is.na(test_nodups$bathrooms)] <- 2
test_nodups$surface_total[is.na(test_nodups$surface_total)] <- 121

test_nodups <- test_nodups %>% select(property_id, city, month, year, 
                                        surface_total, bedrooms, bathrooms, 
                                        type_housing, description_adj, lat, lon,
                                        piscina, garaje, seguridad, balcon, gym)

# Guardar los archivos en formato .rds en la carpeta stores
saveRDS(test_nodups, file.path(stores_path, "test_data.rds"))

