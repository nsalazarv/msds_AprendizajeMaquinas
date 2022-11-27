### Tarea 2: Métodos de Aprendizaje de Máquinas
## Sebastián Novoa, Nelson Salazar y Nicolás Valenzuela

rm(list=ls())
pacman::p_load(tidyverse, tidymodels, randomForest, stacks, dplyr, purrr, workflowsets, discrim)
set.seed(23)

tidymodels_prefer()

## Datos y armando receta

data <- read.csv("/Users/nsalazarv/Library/CloudStorage/OneDrive-UniversidadAdolfoIbanez/Codes/Universidad/MAMDS/Tareas/ALUMNOS-trainData.csv")

muestra <- select_if(data, is.numeric)
muestra$id <- NULL
muestra$flight_number <- NULL

#Transformación variable:

muestra$noshow <- ifelse(muestra$noshow > 3, 1, 0)

#Balance de datos a predecir:
table(muestra$noshow)
barplot(table(muestra$noshow))

#Tienen casi una proporcion de 1:2 entre 0 y 1.

names<-colnames(muestra[-3])
corr<-c()
for (i in names){
  aux<-cor.test(muestra[[3]],muestra[[i]])[[4]]
  corr<-rbind(corr,aux)
}
corr<-cbind(names,corr)

muestra<-muestra %>% sample_n(50000)%>%mutate(noshow=as.factor(noshow)) 
barplot(table(muestra$noshow))

# Dividimos data entrenamiento prueba

data_split <- initial_split(muestra, prop = 3/4, strata = noshow)
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)

train_data %>% mutate(noshow=as.factor(noshow), fligth_number=as.factor(fligth_number)) 

muestra %>% glimpse()

x = train_data[-which(names(train_data) == "noshow")]
x = x[-which(names(x) == "fligth_number")]
x = x[-which(names(x) == "out_of_stock")]
x = x[-which(names(x) == "revenues_usd")]
x = x[-which(names(x) == "group_bookings")]
x = x[-which(names(x) == "denied_boarding")]

x_test = test_data[-which(names(test_data) == "noshow")]
x_test = x_test[-which(names(x_test) == "fligth_number")]
x_test = x_test[-which(names(x_test) == "out_of_stock")]
x_test = x_test[-which(names(x_test) == "revenues_usd")]
x_test = x_test[-which(names(x_test) == "group_bookings")]
x_test = x_test[-which(names(x_test) == "denied_boarding")]

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

x = remove_outliers(x)
x_test = remove_outliers(x_test)

mux = apply(x,2,mean)
sdx = apply(x,2,sd)

x_est = x
x_est[,1] = (x_est[,1] - mux[1])/sdx[1]
x_est[,2] = (x_est[,2] - mux[2])/sdx[2]
x_est[,3] = (x_est[,3] - mux[3])/sdx[3]
x_est[,4] = (x_est[,4] - mux[4])/sdx[4]
x_est[,5] = (x_est[,5] - mux[5])/sdx[5]
x_est[,6] = (x_est[,6] - mux[6])/sdx[6]
x_est[,7] = (x_est[,7] - mux[7])/sdx[7]
x_est[,8] = (x_est[,8] - mux[8])/sdx[8]
x_est[,9] = (x_est[,9] - mux[9])/sdx[9]
x_est[,10] = (x_est[,10] - mux[10])/sdx[10]
x_est[,11] = (x_est[,11] - mux[11])/sdx[11]

train_final = x_est

train_final['noshow'] = train_data['noshow']

train_final %>% mutate(noshow=as.factor(noshow)) 

x_test_est = x_test
x_test_est[,1] = (x_test_est[,1] - mux[1])/sdx[1]
x_test_est[,2] = (x_test_est[,2] - mux[2])/sdx[2]
x_test_est[,3] = (x_test_est[,3] - mux[3])/sdx[3]
x_test_est[,4] = (x_test_est[,4] - mux[4])/sdx[4]
x_test_est[,5] = (x_test_est[,5] - mux[5])/sdx[5]
x_test_est[,6] = (x_test_est[,6] - mux[6])/sdx[6]
x_test_est[,7] = (x_test_est[,7] - mux[7])/sdx[7]
x_test_est[,8] = (x_test_est[,8] - mux[8])/sdx[8]
x_test_est[,9] = (x_test_est[,9] - mux[9])/sdx[9]
x_test_est[,10] = (x_test_est[,10] - mux[10])/sdx[10]
x_test_est[,11] = (x_test_est[,11] - mux[11])/sdx[11]

test_final = x_test_est

test_final['noshow'] = test_data['noshow']

test_final %>% mutate(noshow=as.factor(noshow)) 

## Generamos la receta

data_rec <- 
  recipe(noshow ~ . , data = train_final)

## Modelo SVM

modelo <- svm_rbf(cost = 1000) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()

modelo_fit <- 
  workflow() %>% 
  add_model(modelo) %>% 
  add_recipe(data_rec) %>% 
  fit(data = train_final)

model_pred <- 
  predict(modelo_fit, test_final) 

y_pred<-as.matrix(model_pred)
y_test<-as.matrix(test_final$noshow)
y_pred<-as.factor(y_pred)
y_test<-as.factor(y_test)
confusionMatrix(y_pred,y_test, positive="0", mode="prec_recall")

## Modelo de ensamble (Random Forest)

forest <- randomForest(noshow ~ ., data = train_final, importance=TRUE)

forest_pred <- cbind(predict(forest, newdata = test_final, type="class")) #test_data[, -which(names(test_data) == "noshow")]))

rangeTest = 1:nrow(test_final)

for (i in rangeTest){
  if(forest_pred[i] == 2){
    forest_pred[i] = 1
  }
  else{
    forest_pred[i] = 0
  }
}

y_pred2<-as.matrix(forest_pred)
y_test2<-as.matrix(test_final$noshow)
y_pred2<-as.factor(y_pred2)
y_test2<-as.factor(y_test2)
confusionMatrix(y_pred2,y_test2, positive = "0", mode="prec_recall")


### Para este modelo stacked, ocuparemos tres modelos base: un SVM, un Naive Bayes y un Árbol de Decisión.

folds <- rsample::vfold_cv(train_final, v = 5)
metric <- metric_set(roc_auc)

ctrl_grid <- control_stack_grid()
ctrl_res <- control_stack_resamples()

## SVM

modelo_svm <- svm_rbf(cost = tune("cost"), rbf_sigma = tune("sigma")) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()

svm_wflow <- 
  workflow() %>% 
  add_model(modelo_svm) %>% 
  add_recipe(data_rec)

svm_res <- 
  tune_grid(
    svm_wflow, 
    resamples = folds, 
    grid = 6,
    metrics = metric,
    control = ctrl_grid
  )

## Naive Bayes

modelo_nb <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("naivebayes")

nb_wflow <- 
  workflow() %>% 
  add_model(modelo_nb) %>% 
  add_recipe(data_rec) 

nb_res <- 
  tune_grid(
    nb_wflow, 
    resamples = folds, 
    grid = 6,
    metrics = metric,
    control = ctrl_grid
  )

## Árbol de Decisión

modelo_ad <-
  decision_tree(tree_depth = tune("depth"), min_n = tune("min")) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

ad_wflow <- 
  workflow() %>% 
  add_model(modelo_ad) %>% 
  add_recipe(data_rec) 

ad_res <- 
  tune_grid(
    ad_wflow, 
    resamples = folds, 
    grid = 6,
    metrics = metric,
    control = ctrl_grid
  )

## Stacking

noShow_model <- 
  stacks() %>%
  add_candidates(svm_res) %>%
  add_candidates(nb_res) %>%
  add_candidates(ad_res)

noShow_model <-
  noShow_model %>%
  blend_predictions()

noShow_model <-
  noShow_model %>%
  fit_members()

## Predicción
test_final2 = test_final

test_final <- 
  test_final %>%
    predict(noShow_model, .)

y_pred3<-as.matrix(test_final)
y_test3<-as.matrix(test_final2$noshow)
y_pred3<-as.factor(y_pred3)
y_test3<-as.factor(y_test3)
confusionMatrix(y_pred3,y_test3, positive = "0", mode="prec_recall")

### El mejor modelo, desde el punto de vista del F1 Score, es el de Stacking (0.5065)

eval <- read.csv("/Users/nsalazarv/Library/CloudStorage/OneDrive-UniversidadAdolfoIbanez/Codes/Universidad/MAMDS/Tareas/ALUMNOS-evalData.csv")

eval1 <- select_if(eval, is.numeric)
eval1$id <- NULL
eval1$flight_number <- NULL

x2 = eval1[-which(names(eval1) == "fligth_number")]
x2 = x2[-which(names(x2) == "out_of_stock")]
x2 = x2[-which(names(x2) == "revenues_usd")]
x2 = x2[-which(names(x2) == "group_bookings")]
x2 = x2[-which(names(x2) == "denied_boarding")]

mux2 = apply(x2,2,mean)
sdx2 = apply(x2,2,sd)

x_est2 = x2
x_est2[,1] = (x_est2[,1] - mux2[1])/sdx2[1]
x_est2[,2] = (x_est2[,2] - mux2[2])/sdx2[2]
x_est2[,3] = (x_est2[,3] - mux2[3])/sdx2[3]
x_est2[,4] = (x_est2[,4] - mux2[4])/sdx2[4]
x_est2[,5] = (x_est2[,5] - mux2[5])/sdx2[5]
x_est2[,6] = (x_est2[,6] - mux2[6])/sdx2[6]
x_est2[,7] = (x_est2[,7] - mux2[7])/sdx2[7]
x_est2[,8] = (x_est2[,8] - mux2[8])/sdx2[8]
x_est2[,9] = (x_est2[,9] - mux2[9])/sdx2[9]
x_est2[,10] = (x_est2[,10] - mux2[10])/sdx2[10]
x_est2[,11] = (x_est2[,11] - mux2[11])/sdx2[11]

x_est2 <- 
  x_est2 %>%
  predict(noShow_model, .)

write.csv(x_est2,"/Users/nsalazarv/Library/CloudStorage/OneDrive-UniversidadAdolfoIbanez/Codes/Universidad/MAMDS/Tareas/pred-evalData.csv", row.names = FALSE)
