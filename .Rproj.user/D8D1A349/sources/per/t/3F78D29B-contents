pacman::p_load(tidyverse, tidymodels, kknn, caret, umap)
set.seed(23)
tidymodels_prefer()
data <- read.csv("Tarea 3/ALUMNOS-trainData.csv")

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

muestra<-muestra %>% sample_n(25000)%>%mutate(noshow=as.character(noshow)) 
            

# dividimos data entrenamiento prueba
data_split <- initial_split(muestra, prop = 3/4, strata = noshow)
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)
train_data %>% nrow()

#Data Umap:
# muestra_2<-muestra %>%sample_n(10000)
# 
# model.umap <- umap(muestra_2[-3])
# data.umap <- 
#   model.umap$layout %>% 
#   as.data.frame()
# ggplot(data.umap) +
#   geom_point(aes(V1,V2, col=factor(muestra_2$noshow))) +
#   theme()


muestra %>% glimpse()

# generamos la receta
data_rec <- 
  recipe(noshow ~ . , data = muestra)

data_rec

#definimos el modelo rbf
modelo <- svm_rbf(cost = 1000) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()


modelo

#prediccion

modelo_fit <- 
  workflow() %>% 
  add_model(modelo) %>% 
  add_recipe(data_rec) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data) 

y_pred<-as.matrix(model_pred)
y_test<-as.matrix(test_data$noshow)
y_pred<-as.factor(y_pred)
y_test<-as.factor(y_test)
confusionMatrix(y_pred,y_test, positive="0", mode="prec_recall")


# definimos funcion fitea igual que otras oportunidades
# fitea <- function(mod){
#   
#   modelo_fit <- 
#     workflow() %>% 
#     add_model(mod) %>% 
#     add_recipe(data_rec) %>% 
#     fit(data = train_data)
#   
#   model_pred <- 
#     predict(modelo_fit, test_data, type = "prob") %>% 
#     bind_cols(test_data) 
#   
#   roc <- (model_pred %>% 
#             roc_auc(truth = as.factor(noshow),
#                      .pred_0))
#   
#   return(roc)
# }
# 
# # ajustamos el modelo
# fitea(modelo)



