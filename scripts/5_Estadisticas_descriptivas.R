#-----------------------------------------------------------------------------//
# Estadísticas descriptivas
# Problem Set 3 G10 - BDML 202501
# Fecha: 3 de mayo 2025
#-----------------------------------------------------------------------------//

train =  readRDS(file.path(stores_path, "train_data.rds"))
skim(train)

library(skimr)
library(knitr)
library(kableExtra)
library(recipes)


skim(train) %>%
  kable(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))
