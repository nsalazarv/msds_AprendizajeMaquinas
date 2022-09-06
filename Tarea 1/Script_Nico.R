pacman::p_load(dplyr,psych)

data<-read.csv("datos_t1_centroides.csv",sep=",",header=TRUE)
#Analisis de datos nulos.

sum(is.na(data))

Tuplas_na<-sum(apply(X=is.na(data),MARGIN=1,FUN =sum)>0)
Tuplas_na
Tuplas_na*100/1661
#12.5 % de las tuplas tienen nulos.

Column_na<-sum(apply(X=is.na(data),MARGIN=2,FUN =sum)>0)
#11 columnas presentan NA

#True/falso presencia nulos por columna/fila
valores_col<-apply(X=is.na(data),MARGIN=2,FUN =sum)>0
valores_tu<-apply(X=is.na(data),MARGIN=1,FUN =sum)>0

#columnas con su respectivo numero de nulos
apply(X=is.na(data),MARGIN=2,FUN =sum)[valores]

#Filas con su respectivo indice y n°de nulo
indice_nulos<-cbind(rownames(data)[valores_tu],apply(X=is.na(data),MARGIN=1,FUN =sum)[valores_tu])
valores_1<-apply(X=is.na(data),MARGIN=1,FUN =sum)>1

#se sacan las filas con n°de nulos mayor a 1.
indices_e<-c(row.names(data)[!(valores_1)])
indices_e
#vemos que la cantidad de nulos sobrante es de 28, correspondiente a la columan ise.
#Es mejor eliminar 28 filas, que eliminar esa columna y perder los demás datos.
data2<-data[indices_e,]
apply(X=is.na(data2),MARGIN=2,FUN =sum)
#data_final
data_final<-data[c(row.names(data)[!(valores_tu)]),]
apply(X=is.na(data_final),MARGIN=2,FUN =sum)


#Estadistica Descriptiva.

#Notamos que las variables X1., cod_com, COMUNA, MANZ_EN no aportan valores diferentes.

data_final %>% count(COMUNA)
data_final %>% count(cod_com)
data_final %>% count(MANZ_EN)
#eliminamos X1 por ser los indices y las demás columnas por ser el mismo valor para todos los datos.

data_final$X.1<-NULL
data_final$COMUNA<-NULL
data_final$cod_com<-NULL
data_final$MANZ_EN<-NULL

cor(data_final, method="pearson")[cor(data_final, method="pearson")>0.75]

cor(data_final, method="pearson")>0.75
