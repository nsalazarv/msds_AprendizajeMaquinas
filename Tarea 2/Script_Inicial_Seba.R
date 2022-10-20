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

#Eliminamos los indices:
data$X<-NULL
data$index<-NULL
data$full_name <- NULL

#Elimnamos las columnas con mayor cantidad de nulos.
data$G<-NULL
data$GM<-NULL
data$BV<-NULL
data$UB<-NULL
data$IR <- NULL

#Borramos variables categoricas que no afectan a nuestro.
data$neo<-NULL
data$pha<-NULL
data$extent<-NULL
data$spec_B<-NULL
data$spec_T<-NULL

# Analisis Descriptivo
str(data)
nombres<-names(select_if(data, is.numeric))
nombres

# Guardamos data con nulos:
data2=data
#Remplazar los nulos con ceros
for (i in 1:16){
  variable=nombres[i]
  data[[variable]] <- round(data[[variable]] %>%
      replace(is.na(.), 0))
}

#columnas con su respectivo numero de nulos
apply(X=is.na(data),MARGIN=2,FUN =sum)
# Correlaciones

muestra <- select_if(data, is.numeric)

#Menor a mayor considerando negativos:
cor(muestra, method="pearson")[13,][order(cor(muestra, method="pearson")[15,])]
#Menor a mayor en absoluto:
sort(abs(cor(muestra, method="pearson")[13,]))
sort(abs(cor(muestra, method="spearman")[13,]))

#Borramos data con correlacion bajo 0.2 en pearson y 0.3 en spearmann
data$w<-NULL
data$rot_per<-NULL
data$om<-NULL
data$e<-NULL
data$i<-NULL
data$albedo<-NULL
data$condition_code<-NULL
muestra<-select_if(data,is.numeric)
sort(abs(cor(muestra, method="pearson")[8,]))
sort(abs(cor(muestra, method="spearman")[8,]))
#Data hasta ahora considerando lo realizado con anterioridad.

#nulos por cero, elimine categoricas y elimine correlaciones segun pearson y spearmann.
write.csv(data, file="data_nulos_cero.csv")


#Variable a predecir:

summary(data["diameter"])
hist(x=data$diameter)

algo<-data$diameter[data["diameter"]<20]
hist(x=algo)
print(length(algo))
summary(algo)
summary(data$diameter)
