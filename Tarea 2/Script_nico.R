pacman::p_load(psych,ggplot2,tidyverse,proxy,dplyr)
data<-read.csv("dataTrain.csv")
test<-read.csv("dataEval.csv")

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
Col_Eval_nulos<-names(a[a>37500])
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
Col_Train_nulos<-names(b[b>99500])
Col_Train_nulos

#Elimnamos las columnas con mayor cantidad de nulos.
data$G<-NULL
data$GM<-NULL
data$BV<-NULL
data$UB<-NULL
data$IR<-NULL

#Siguen habiendo 99970 tuplas con nulos.

# Analisis Descriptivo ####
str(data)

#Variable a predecir:
summary(data["diameter"])
hist(x=data$diameter)

algo<-data$diameter[data["diameter"]>200]

