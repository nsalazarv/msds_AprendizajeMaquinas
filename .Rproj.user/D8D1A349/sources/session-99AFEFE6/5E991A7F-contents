pacman::p_load(tidyverse, tidymodels)
library("kernlab")
library(openintro)
data(starbucks)
attach(starbucks)
summary(starbucks)

set.seed(57206)
data<-starbucks
data$type<- ifelse(data$type == 'bakery', 1, 0)
# Y=data$type
# data$type<-NULL
data$item<-NULL
# X=as.matrix(data)

# Prepare the data frame containing the cross validation partitions
folds <- vfold_cv(data, v = 5, strata = "type" )
folds


cv_data <- folds %>% 
  mutate(
    # Extract the train data frame for each split
    train = map(splits, ~training(.x)), 
    # Extract the validate data frame for each split
    validate = map(splits, ~testing(.x))
  )

#3 es train
# cv_data[[3]][[1]][-1]
# #4 es test
# 
# Y=as.matrix(cv_data[[3]][[1]][6])
# X=as.matrix(cv_data[[3]][[1]][1:5])


###Pregunta 1. ####


##Sin Estandarizar##
resultados<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    X_train=as.matrix(cv_data[[3]][[i]][j])
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    
    #Testeo
    X_test=as.matrix(cv_data[[4]][[i]][j])
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    Precision=Table[2,2]/(Table[2,1]+Table[2,2])
    Recall=Table[2,2]/(Table[1,2]+Table[2,2])
    aux<-cbind(nombres[j],i,Accuracy,Precision,Recall)
    aux_2<-rbind(aux_2,aux)
  }
  resultados<-rbind(resultados,aux_2)
}


#resultados generales.
resultados<-data.frame(resultados)
colnames(resultados)<-c("Covariable", "Kfold", "Accuracy", "Precision", "Recall")
resultados_se<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  Precision=as.numeric(Precision),
                                  Recall=as.numeric(Recall))
str(resultados_se)
medias_se<-resultados_se %>%  group_by(Covariable) %>% summarise(Accuracy = mean(Accuracy), 
                                                   Precision = mean(Precision), 
                                                   Recall = mean(Recall))

#accuracy: proteina; precision carb y recall protein

##Estandarizado ##

resultados<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    X_train=cv_data[[3]][[i]][j]
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    
    #Testeo
    X_test=cv_data[[4]][[i]][j]
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    
    ## Estandarizando 
    
    mux = apply(X_train,2,mean)
    sdx = apply(X_train,2,sd)
    
    xtrain_est = X_train
    xtrain_est[,1] = (X_train[,1] - mux[1])/sdx[1]
    xtrain_est = as.matrix(xtrain_est)
    
    xtest_est = X_test
    xtest_est[,1] = (X_test[,1] - mux[1])/sdx[1]
    
    
    xtest_est = as.matrix(xtest_est)
    X_train=xtrain_est
    X_test=xtest_est
    
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    Precision=Table[2,2]/(Table[2,1]+Table[2,2])
    Recall=Table[2,2]/(Table[1,2]+Table[2,2])
    aux<-cbind(nombres[j],i,Accuracy,Precision,Recall)
    aux_2<-rbind(aux_2,aux)
  }
  resultados<-rbind(resultados,aux_2)
}


#resultados generales.
resultados<-data.frame(resultados)
colnames(resultados)<-c("Covariable", "Kfold", "Accuracy", "Precision", "Recall")
resultados_ce<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  Precision=as.numeric(Precision),
                                  Recall=as.numeric(Recall))
str(resultados_ce)
medias_ce<-resultados_ce %>%  group_by(Covariable) %>% summarise(Accuracy = mean(Accuracy), 
                                                           Precision = mean(Precision), 
                                                           Recall = mean(Recall))
medias_ce
medias_se



###Pregunta 2: ####

## Sin Estandarizar ##

resultados_2<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    X_train=as.matrix(cv_data[[3]][[i]][c(-j,-6)])
    
    
    #Testeo
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    X_test=as.matrix(cv_data[[4]][[i]][c(-j,-6)])
    
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    aux<-cbind(nombres[j],i,Accuracy)
    aux_2<-rbind(aux_2,aux)
  }
  resultados_2<-rbind(resultados_2,aux_2)
}

resultados_2<-data.frame(resultados_2)
colnames(resultados_2)<-c("Covariable_fuera", "Kfold", "Accuracy")
resultados_2_se<-resultados_2 %>% mutate(Accuracy=as.numeric(Accuracy))
str(resultados_2_se)
medias_2_se<-resultados_2_se %>%  group_by(Covariable_fuera) %>% summarise(Accuracy = mean(Accuracy))


## Estandarizado ##

resultados_2<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    X_train=cv_data[[3]][[i]][c(-j,-6)]
    
    
    #Testeo
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    X_test=cv_data[[4]][[i]][c(-j,-6)]
    
    ## Estandarizando 
    
    mux = apply(X_train,2,mean)
    sdx = apply(X_train,2,sd)
    
    xtrain_est = X_train
    xtrain_est[,1] = (X_train[,1] - mux[1])/sdx[1]
    xtrain_est[,2] = (X_train[,2] - mux[2])/sdx[2]
    xtrain_est[,3] = (X_train[,3] - mux[3])/sdx[3]
    xtrain_est[,4] = (X_train[,4] - mux[4])/sdx[4]
    
    xtrain_est = as.matrix(xtrain_est)
    
    xtest_est = X_test
    xtest_est[,1] = (X_test[,1] - mux[1])/sdx[1]
    xtest_est[,2] = (X_test[,2] - mux[2])/sdx[2]
    xtest_est[,3] = (X_test[,3] - mux[3])/sdx[3]
    xtest_est[,4] = (X_test[,4] - mux[4])/sdx[4]
    
    xtest_est = as.matrix(xtest_est)
    X_train=xtrain_est
    X_test=xtest_est
    
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    aux<-cbind(nombres[j],i,Accuracy)
    aux_2<-rbind(aux_2,aux)
  }
  resultados_2<-rbind(resultados_2,aux_2)
}

resultados_2<-data.frame(resultados_2)
colnames(resultados_2)<-c("Covariable_fuera", "Kfold", "Accuracy")
resultados_2_ce<-resultados_2 %>% mutate(Accuracy=as.numeric(Accuracy))
str(resultados_2_ce)
medias_2_ce<-resultados_2_ce %>%  group_by(Covariable_fuera) %>% summarise(Accuracy = mean(Accuracy))

medias_2_se
medias_2_ce
