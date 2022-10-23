pacman::p_load(psych,ggplot2,tidyverse,proxy,dplyr,fastDummies)
data<-read.csv("dataTrain.csv")
test<-read.csv("dataEval.csv")

#presencia duplicados.
nrow(distinct(data))
nrow(distinct(test))
head(data)
#notamos que las variables categoricas sin datos no se toman como nulos.
data[data == ""] <- NA
test[test == ""] <- NA

## Viendo nulos de Eval: ####

#Tuplas con nulos:
Tuplas_na<-sum(apply(X=is.na(test),MARGIN=1,FUN =sum)>0)
Tuplas_na #Todas las tuplas tienen nulos

#columnas con nulos:
Column_na<-sum(apply(X=is.na(test),MARGIN=2,FUN =sum)>0)
#columnas con su respectivo numero de nulos
valores_col<-apply(X=is.na(test),MARGIN=2,FUN =sum)>0
a<-apply(X=is.na(test),MARGIN=2,FUN =sum)[valores_col]
a
Col_Eval_nulos<-names(a[a>37300])
Col_Eval_nulos

## Viendo nulos de Train: ####
#Tuplas con nulos:
Tuplas_na<-sum(apply(X=is.na(data),MARGIN=1,FUN =sum)>0)
Tuplas_na #Todas las tuplas tienen nulos

#columnas con nulos:
Column_na<-sum(apply(X=is.na(data),MARGIN=2,FUN =sum)>0)
#columnas con su respectivo numero de nulos
valores_col<-apply(X=is.na(data),MARGIN=2,FUN =sum)>0
b<-apply(X=is.na(data),MARGIN=2,FUN =sum)[valores_col]
b
Col_Train_nulos<-names(b[b>99000])
Col_Train_nulos

##Eliminación features nulas con 1% de datos.####

#features con 99% de las tuplas nulas.
Col_Train_nulos
Col_Eval_nulos
#Eliminamos los indices:
data$X<-NULL
data$index<-NULL
data$full_name<-NULL
#Elimnamos las columnas con mayor cantidad de nulos.
data$G<-NULL
data$GM<-NULL
data$BV<-NULL
data$UB<-NULL
data$IR<-NULL
data$extent<-NULL
data$spec_B<-NULL
data$spec_T<-NULL

apply(X=is.na(data),MARGIN=2,FUN =sum)

# Analisis Descriptivo ####
str(data)
nombres<-names(select_if(data, is.numeric))
nombres
#Vemos distribucion de las variables numericas.
for (i in 1:16){
  variable=nombres[i]
  hist(data[[variable]],main=nombres[i],col="blue")
}

table(data$neo)
table(data$pha)

#Variable a predecir:

summary(data["diameter"])
hist(x=data$diameter)

algo<-data$diameter[data["diameter"]<20]
hist(x=algo,freq = FALSE)
lines(density(algo),col="red",lwd=2)
print(length(algo))
summary(algo)
summary(data$diameter)


##Eliminamos las tuplas con datos extremos para diameter.
data<-data[data$diameter < 20, ] 
#conservamos el 98% de los datos.
#Vemos distribucion de las variables numericas.
for (i in 1:16){
  variable=nombres[i]
  hist(data[[variable]],main=nombres[i],col="red")
}

table(data$neo)
table(data$pha)


summary(data$a)
summary(data$i)
summary(data$ad)
summary(data$per_y)
summary(data$rot_per)

boxplot(data$a, main="a")
boxplot(data$i,main="i")
boxplot(data$ad,main="ad")
boxplot(data$per_y,main="per_y")
boxplot(data$rot_per,main="rot_per")

#Reemplazamos los datos por su mediana.

medianas<-c(median(data$a,na.rm = TRUE),median(data$e,na.rm = TRUE),median(data$i,na.rm = TRUE)
          ,median(data$om,na.rm = TRUE),median(data$w,na.rm = TRUE),median(data$q,na.rm = TRUE)
          ,median(data$ad,na.rm = TRUE),median(data$per_y,na.rm = TRUE),median(data$data_arc,na.rm = TRUE)
          ,median(data$condition_code,na.rm = TRUE),median(data$n_obs_used,na.rm = TRUE)
          ,median(data$H,na.rm = TRUE),median(data$diameter,na.rm = TRUE),median(data$albedo,na.rm = TRUE)
          ,median(data$rot_per,na.rm = TRUE),median(data$moid,na.rm = TRUE))
#guardamos data con nulos:
data2=data

#Remplazar los nulos con la mediana.
for (i in 1:16){
  variable=nombres[i]
  data[[variable]]<- round(data[[variable]] %>%
                             replace(is.na(.),medianas[i]), digits = 6)
}
for (i in 1:16){
  variable=nombres[i]
  hist(data[[variable]],main=nombres[i],col="green")
}

muestra<-select_if(data,is.numeric)
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

table(data$neo)
table(data$pha)

#buscamos realizar dummy a las variables categoricas.
data<-dummy_cols(data,  select_columns = c("neo", "pha")) %>% 
  select(-c("neo", "pha"))

#Guardamos la data.
write.csv(data, file="data_nulos_mediana_sinoutliers.csv")
