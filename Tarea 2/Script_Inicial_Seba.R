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

# Correlaciones de DIAMETER con el resto de variables
cor.test(data$diameter, data$a, method = "pearson") # 0.2151
cor.test(data$diameter, data$e, method = "pearson") # -0.0485
cor.test(data$diameter, data$G, method = "pearson") # -0.1102
cor.test(data$diameter, data$i, method = "pearson") # 0.0544
cor.test(data$diameter, data$om, method = "pearson") # 0.006
cor.test(data$diameter, data$w, method = "pearson") # 0.0028
cor.test(data$diameter, data$q, method = "pearson") # 0.3047
cor.test(data$diameter, data$ad, method = "pearson") # 0.1807
cor.test(data$diameter, data$per_y, method = "pearson") # 0.0466
cor.test(data$diameter, data$data_arc, method = "pearson") # 0.4940
cor.test(data$diameter, data$condition_code, method = "pearson") #-0.072
cor.test(data$diameter, data$n_obs_used, method = "pearson") # 0.3618
cor.test(data$diameter, data$H, method = "pearson") # -0.5644
cor.test(data$diameter, data$extent, method = "pearson") # NO SE PUEDE
cor.test(data$diameter, data$albedo, method = "pearson") # -0.1037
cor.test(data$diameter, data$rot_per, method = "pearson") # 0.007
cor.test(data$diameter, data$GM, method = "pearson") # NO SE PUEDE
cor.test(data$diameter, data$BV, method = "pearson") # -0.3179
cor.test(data$diameter, data$UB, method = "pearson") # -0.2611
cor.test(data$diameter, data$IR, method = "pearson") # NO SE PUEDE
cor.test(data$diameter, data$spec_B, method = "pearson") # NO SE PUEDE
cor.test(data$diameter, data$spec_T, method = "pearson") # NO SE PUEDE
cor.test(data$diameter, data$neo, method = "pearson") # NO SE PUEDE
cor.test(data$diameter, data$pha, method = "pearson") # NO SE PUEDE
cor.test(data$diameter, data$moid, method = "pearson") # 0.3509

# Ordenamos de menor a mayor correlación
cor.test(data$diameter, data$H, method = "pearson") # -0.5644
cor.test(data$diameter, data$BV, method = "pearson") # -0.3179
cor.test(data$diameter, data$UB, method = "pearson") # -0.2611
cor.test(data$diameter, data$G, method = "pearson") # -0.1102
cor.test(data$diameter, data$albedo, method = "pearson") # -0.1037
cor.test(data$diameter, data$condition_code, method = "pearson") #-0.072
cor.test(data$diameter, data$e, method = "pearson") # -0.0485
cor.test(data$diameter, data$w, method = "pearson") # 0.0028
cor.test(data$diameter, data$om, method = "pearson") # 0.006
cor.test(data$diameter, data$rot_per, method = "pearson") # 0.007
cor.test(data$diameter, data$per_y, method = "pearson") # 0.0466
cor.test(data$diameter, data$i, method = "pearson") # 0.0544
cor.test(data$diameter, data$ad, method = "pearson") # 0.1807
cor.test(data$diameter, data$a, method = "pearson") # 0.2151
cor.test(data$diameter, data$q, method = "pearson") # 0.3047
cor.test(data$diameter, data$moid, method = "pearson") # 0.3509
cor.test(data$diameter, data$n_obs_used, method = "pearson") # 0.3618
cor.test(data$diameter, data$data_arc, method = "pearson") # 0.4940
cor.test(data$diameter, data$diameter, method = "pearson") # 1
