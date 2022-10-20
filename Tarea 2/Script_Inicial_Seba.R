# Cargo librerias
pacman::p_load(tidyverse, umap, tidymodels, kknn)
set.seed(42)

# Define funciones de tidymodels por defecto
tidymodels_prefer()

# Cargamos la data
data <- read_csv("Tareas/msds_AprendizajeMaquinas/Tarea 2/dataTrain.csv")

# Identificamos el numero de filas
n <- nrow(data)
n
# Tenemos 100.000 filas iniciales

# Aplicamos los mismos tratamientos que en el caso de DBScan
# data.num <- data_raw %>%
#     dplyr::select(where(is.numeric)) %>%
#     drop_na()

# Identificamos el numero de filas
# n_new <- nrow(data.num)
# n_new

# Seleccionamos las variables relevantes para el análisis
colnames(data)
ncol(data)
# Tenemos 29 variables

# Observamos si existen datos nulos
Column_na <- sum(apply(X = is.na(data), MARGIN = 2, FUN = sum) > 0)
Column_na
# 23 Columnas tienen NA

# True/falso presencia nulos por columna/fila
valores_col <- apply(X = is.na(data), MARGIN = 2, FUN = sum) > 0
valores_col
# Las columnas 1 (indice), index, full_name, condition_code, neo y pha, NO tienen NA

# Columnas con su respectivo numero de nulos
apply(X = is.na(data), MARGIN = 2, FUN = sum)[valores_col]

#True/falso presencia nulos por columna/fila
valores_col<-apply(X=is.na(data),MARGIN=2,FUN =sum)>0
valores_tu<-apply(X=is.na(data),MARGIN=1,FUN =sum)>0

#Filas con su respectivo indice y n°de nulo
indice_nulos<-cbind(rownames(data)[valores_tu],apply(X=is.na(data),MARGIN=1,FUN =sum)[valores_tu])
valores_1<-apply(X=is.na(data),MARGIN=1,FUN =sum)>1

#Buscamos el indice con NA en diameter
which(is.na(data$diameter))
