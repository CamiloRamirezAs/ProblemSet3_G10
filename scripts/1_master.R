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
submission_path   <- file.path(path_main, "submission")
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
      tidyverse, 
              rvest,
              dplyr,
              stargazer, 
              foreign, 
              skimr, # summary data
              visdat, # visualizing missing data
              corrplot, 
              scales, 
              broom,
              gridExtra, 
              survey,
              VIM, 
              fastDummies, 
              caret,
              boot,
              DescTools,
              Metrics,
              MLmetrics,
              rpart,
              rpart.plot,
              ipred,
              ranger, 
              ggplot2, 
              randomForest, 
              adabag,
              pROC,
              recipes,
              vip,
              MLmetrics,
              xgboost,
              DMwR2,
              themis,
              spatialsample,
              SuperLearner)

library("xtable")

#install.packages("xtable", dependencies = TRUE, type = "source")
