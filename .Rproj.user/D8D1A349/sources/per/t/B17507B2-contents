pacman::p_load(tidyverse, tidymodels, kknn, caret, umap, discrim, klaR)
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

muestra<-muestra %>% sample_n(250000) %>% mutate(noshow=as.factor(noshow))

# dividimos data entrenamiento prueba
data_split <- initial_split(muestra, prop = 3/4, strata = c("noshow"))
train_data <- training(data_split)
test_data  <- testing(data_split)

nrow(test_data)
train_data %>% nrow()

# validacion cruzada

cv_folds <- vfold_cv(
  data = train_data, 
  v = 5) 

cv_folds

# Especificamos modelo ----

nb_model <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR")

# receta
data_rec <- 
  recipe(noshow ~ . , data = train_data) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

modelo_workf <- 
  workflow() %>% 
  add_model(nb_model) %>% 
  add_recipe(data_rec)

# Ajustamos el modelo ----

nb_fit <- 
  modelo_workf %>% 
  fit_resamples(resamples = cv_folds)

collect_metrics(nb_fit)

# predecimos con data de prueba

nb_final <- 
  modelo_workf %>% 
  last_fit(split = data_split)
collect_metrics(nb_final)

#Prediccion en la data de prueba.
nb_test_pred <- 
  bind_cols(test_data,
            nb_final %>% 
              collect_predictions() %>% 
              dplyr::select(starts_with(".pred_"))
  )

table("predicted class" = nb_test_pred$.pred_class,
      "observed class" = nb_test_pred$noshow)
nb_test_pred %>% glimpse()
nb_test_pred<-nb_test_pred %>% mutate(.pred_class=as.numeric(.pred_class))
nb_test_pred$.pred_class<-nb_test_pred$.pred_class-1
nb_test_pred %>% 
  roc_curve(
    truth = noshow,
    .pred_class
  ) %>% 
  autoplot()

#Evaluacion modelo.
y_pred<-as.matrix(nb_test_pred$.pred_class)
y_test<-as.matrix(nb_test_pred$noshow)
y_pred<-as.factor(y_pred)
y_test<-as.factor(y_test)
confusionMatrix(y_pred,y_test, positive="0", mode="prec_recall")
