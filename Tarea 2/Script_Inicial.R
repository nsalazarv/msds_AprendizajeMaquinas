# Cargo librerias
pacman::p_load(tidyverse, umap, tidymodels, kknn)
set.seed(42)

# Define funciones de tidymodels por defecto
tidymodels_prefer()

# Cargamos la data
data_raw <- read_rds("dataTrain.csv")

# Identificamos el numero de filas
n <- nrow(data.num)
