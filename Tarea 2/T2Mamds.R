### Tarea 2: Métodos de Aprendizaje de Máquinas
## Sebastián Novoa, Nelson Salazar y Nicolás Valenzuela

rm(list=ls())
pacman::p_load(tidyverse, tidymodels, kknn, caret)
set.seed(23)

tidymodels_prefer()

data <- read.delim(file = 'data_nulos_cero.txt', sep = ',')
dataEval <- read.delim(file = 'data_eval_nulos_cero.txt', sep = ',')

mod_test <-
  nearest_neighbor(neighbors = tune(), weight_func = "triangular") %>%
  set_mode("regression") %>%
  set_engine("kknn")

knn_param <- extract_parameter_set_dials(mod_test)
knn_param %>% extract_parameter_dials("neighbors")

## Haciendo un tune() rápido del parámetro "neighbors", 
## obtenemos que el óptimo estaría con un K entre 1 y 15.

ecm_vec = c() 

for(i in 1:15){

  print(paste("K: ", i))

  x_train = data[,-c(1, 10)]
  y_train = data[10]
  
  # Normalizando
  
  process1 <- preProcess(as.data.frame(x_train), method=c("range"))
  x_train_norm <- predict(process1, as.data.frame(x_train))
  
  x_train_norm['diameter'] = y_train['diameter']
  
  # Construcción del modelo
  
  knn_reg_spec <-
    nearest_neighbor(neighbors = i, weight_func = "triangular") %>%
    set_mode("regression") %>%
    set_engine("kknn")
  
  knn_reg_fit <- knn_reg_spec %>% fit(diameter ~ ., data = x_train_norm)
  
  ecm_vec = append(ecm_vec, knn_reg_fit$fit$MEAN.SQU)

}

ecm_vec 

## Con K = 10 se obtiene el menor ECM 

dataEval = dataEval[,-c(1)]

process2 <- preProcess(as.data.frame(dataEval), method=c("range"))
dataEval_norm <- predict(process2, as.data.frame(dataEval))

knn_reg_spec2 <-
  nearest_neighbor(neighbors = 10, weight_func = "triangular") %>%
  set_mode("regression") %>%
  set_engine("kknn")

knn_reg_spec2

knn_reg_fit2 <- knn_reg_spec2 %>% fit(diameter ~ ., data = x_train_norm)
knn_reg_fit2

pred2 = predict(knn_reg_fit2, dataEval_norm)

min(pred2)
max(pred2)


# write.csv(df,"/Users/nsalazarv/Library/CloudStorage/OneDrive-UniversidadAdolfoIbanez/Codes/Universidad/MAMDS/Tareas/pred_meteo.csv", row.names = FALSE)