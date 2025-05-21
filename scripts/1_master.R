#-----------------------------------------------------------------------------//
# Master
# Problem Set 3 
#-----------------------------------------------------------------------------//

rm(list = ls())

#-----------------------------------------------------------------------------//
# 1. Configurar rutas relativas con {here} ----
#-----------------------------------------------------------------------------//

# Instalar paquete 'here' si no está instalado
if (!require(here)) install.packages("here", dependencies = TRUE)
library(here)

# Definir la ruta principal como la raíz del proyecto
path_main <- here() 

# Definir subcarpetas dentro del proyecto
raw_path <- file.path(path_main, "raw") 
scripts_path  <- file.path(path_main, "scripts") 
stores_path   <- file.path(path_main, "stores")
view_path     <- file.path(path_main, "views")

#-----------------------------------------------------------------------------//
# 2. Cargar paquetes ----
#-----------------------------------------------------------------------------//

# Instalar paquetería {pacman} si no está instalada
if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
pacman::p_load(dplyr, readr, zip)
library(pacman)


# Cargar paquetes necesarios
p_load(tidyverse, 
       rvest,
       dplyr,
       stargazer, 
       foreign, 
       skimr, # summary data
       visdat,
       sf, 
       stringi, 
       tm, 
       leaflet,
       osmdata, 
       nnet 
)

