pacman::p_load(heatmaply)

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
      