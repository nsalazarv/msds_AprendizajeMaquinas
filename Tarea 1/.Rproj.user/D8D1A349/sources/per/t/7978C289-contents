pacman::p_load(dplyr,psych,ggplot2,tidyverse,proxy,dplyr,umap, ggdendro, cluster,dbscan)
pacman::p_load(tidyverse, umap, factoextra, flexclust, cluster)

data<-read.csv("datos_t1_centroides.csv",sep=",",header=TRUE)
source("clusteriza.R")
###Analisis de datos nulos.################

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


###Estadistica Descriptiva.###########
str(data)
#Notamos que las variables X1., cod_com, COMUNA, MANZ_EN no aportan valores diferentes.
unique(data["COMUNA"])
unique(data["cod_com"])
unique(data["MANZ_EN"])
#eliminamos X1 por ser los indices y las demás columnas por ser el mismo valor para todos los datos.
data_final$X.1<-NULL
data_final$COMUNA<-NULL
data_final$cod_com<-NULL
data_final$MANZ_EN<-NULL
#Analisis duplicados: No hay filas duplicadas.
sum(duplicated(data_final))
#Analisis de variables
str(data_final)
count(unique(data_final["ZONA"]))

#Boxplots, revisar outliers.
columnas<-colnames(data_final)
columnas[29:35]
outliers<-c()
boxplot(data_final["iav"])$out
for(i in colnames(data_final)){
  caja<-boxplot(data_final[i],main=i)
  caja
  #caja$out
}
#
simil(list(data_final$iav,data_final$X), method="pearson")
#generar una data que disminuya los computos, evaluar similitud y correlación.
muestra<-data_final %>%
  select(dim_acc, iav, icul,idep,isal,iser,ise)
simil(list(muestra$dim_acc,muestra$iav), method="pearson")
simil(list(muestra$dim_acc,muestra$icul), method="pearson")
simil(list(muestra$dim_acc,muestra$idep), method="pearson")
simil(list(muestra$dim_acc,muestra$isal), method="pearson")
simil(list(muestra$dim_acc,muestra$iser), method="pearson")
simil(list(muestra$dim_acc,muestra$ise), method="pearson")
cor(muestra, method="pearson")[1,]
for(i in c("pearson","kendal","spearman")){
  print(i)
  muestra<-data_final %>%
    select(dim_acc, iav, icul,idep,isal,iser,ise)
  print(cor(muestra, method=i)[1,])
  muestra1<-data_final %>%
    select(dim_amb, iata, icv)
  print(cor(muestra1, method=i)[1,])
  
  muestra2<-data_final %>%
    select(dim_soc, ivi, isv,iej,irh,iem,ipj)
  print(cor(muestra2, method=i)[1,])
  
  muestra3<-data_final %>%
    select(dim_seg, igpe, igpr,ilpe,ilpr)
  print(cor(muestra3, method=i)[1,])
  print("---------------------------------------------------")
}
#data_pp = data_final[, c('ID_MANZ', 'ZONA', 'ibt','E6A14', 'E15A24', 'dim_acc', 'iav', 'idep', 'isal', 'iata', 'icv', 'X', 'Y')]

#######################################CLUSTERS###################################
### analisis de clusters naturales ####
pacman::p_load(magick, factoextra)
get_clust_tendency(data_final[1:35], n = 50, graph = FALSE)

models <- c("kmeans", "hier", "gmm", "cmeans", "dbscan")
evaluacion_cluster <- function(data, clusters){
  coefSil <- silhouette(clusters,dist(data))
  cohesiones <- clus_cohes(data, clusters)
  separaciones <- clus_sep(data, clusters)
  correlacion <- clus_cor(data, clusters)
  return(list(correlacion, mean(coefSil[,3]), cohesiones, separaciones))
}

###Correlaciones y sillohuete promedio distintos modelos####
cor_s<-c()
sil_s<-c()
cor_xy<-c()
sil_xy<-c()
for(i in 1:4){
  cat("###############","Metodo de clasificacion:", models[i],"#########################")
  #CLUSTER SIN XY.
  print("cluster sin XY")
  model.umap <- umap(data_final[1:35])
  data.umap <- 
    model.umap$layout %>% 
    as.data.frame()
  modelo1<-clusteriza(data.umap, models[i], k = 13)
  
  ggplot(data.umap) +
    geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
    theme()
  eval <- evaluacion_cluster(data.umap, modelo1$cluster)
  cor_s<-cbind(cor_s,eval[1])
  sil_s<-cbind(sil_s,eval[2])
  
  #CLUSTER POR XY
  print("cluster por XY")
  modelo2<-clusteriza(data_final[36:37], models[i], k = 13)
  ggplot(data_final[36:37]) +
    geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
    theme()
  eval <- evaluacion_cluster(data_final[36:37], modelo2$cluster)
  #UMAP CON CLUSTER POR XY.
  ggplot(data.umap) +
    geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
    theme()
  cor_xy<-cbind(cor_xy,eval[1])
  sil_xy<-cbind(sil_xy,eval[2])
}
resultados<-rbind(models[1:4],cor_s,sil_s,cor_xy,sil_xy)
resultados<-cbind(c("metodo","cor_s","sil_s","cor_xy","sil_xy"),resultados)
resultados

####Kmeans############
cat("Metodo de clasificacion:", models[1])
#CLUSTER SIN XY.
print("cluster sin XY")
model.umap <- umap(data_final[1:35])
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-kmeans(data.umap,13)

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme()
eval <- evaluacion_cluster(data.umap, modelo1$cluster)
eval

#CLUSTER POR XY
print("cluster por XY")
modelo2<-kmeans(data_final[36:37],13)
ggplot(data_final[36:37]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()
eval <- evaluacion_cluster(data_final[36:37], modelo2$cluster)
eval
#UMAP CON CLUSTER POR XY.
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
  theme()

#### Arbol######
cat("Metodo de clasificacion:", models[2])
#CLUSTER SIN XY.
print("cluster sin XY")
model.umap <- umap(data_final[1:35])
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-clusteriza(data.umap, models[2], k = 13)

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme()
eval <- evaluacion_cluster(data.umap, modelo1$cluster)
eval

#CLUSTER POR XY
print("cluster por XY")
modelo2<-clusteriza(data_final[36:37], models[2], k = 13)
ggplot(data_final[36:37]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()
eval <- evaluacion_cluster(data_final[36:37], modelo2$cluster)
eval
#UMAP CON CLUSTER POR XY.
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
  theme()

#### gmm ######
cat("Metodo de clasificacion:", models[3])
#CLUSTER SIN XY.
print("cluster sin XY")
model.umap <- umap(data_final[1:35])
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-clusteriza(data.umap, models[3], k = 13)

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme()
eval <- evaluacion_cluster(data.umap, modelo1$cluster)
eval

#CLUSTER POR XY
print("cluster por XY")
modelo2<-clusteriza(data_final[36:37], models[3], k = 13)
ggplot(data_final[36:37]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()
eval <- evaluacion_cluster(data_final[36:37], modelo2$cluster)
eval
#UMAP CON CLUSTER POR XY.
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
  theme()

#### Cmeans ######
cat("Metodo de clasificacion:", models[4])
#CLUSTER SIN XY.
print("cluster sin XY")
model.umap <- umap(data_final[1:35])
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-clusteriza(data.umap, models[4], k = 13)

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme()
eval <- evaluacion_cluster(data.umap, modelo1$cluster)
eval

#CLUSTER POR XY
print("cluster por XY")
modelo2<-clusteriza(data_final[36:37], models[4], k = 13)
ggplot(data_final[36:37]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()
eval <- evaluacion_cluster(data_final[36:37], modelo2$cluster)
eval
#UMAP CON CLUSTER POR XY.
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
  theme()

#### Dbs Scan######
sprintf("Metodo de clasificacion:", models[5])
#CLUSTER SIN XY.
print("cluster sin XY")
model.umap <- umap(data_final[1:35])
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-clusteriza(data.umap, models[5], k = 13)

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme()
eval <- evaluacion_cluster(data.umap, modelo1$cluster)
eval

modelo1
?dbscan
#CLUSTER POR XY
print("cluster por XY")
modelo2<-clusteriza(data_final[36:37], models[5], k = 13)
ggplot(data_final[36:37]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()
eval <- evaluacion_cluster(data_final[36:37], modelo2$cluster)
eval
#UMAP CON CLUSTER POR XY.
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
  theme()


















clusters_jer <- function(data, i_d, i_a, k){
  metodos_distancias <- c("mahalanobis", "euclidean", "maximum", "manhattan", "binary", "minkowski")
  metodos_agregacion <- c("complete", "average", "median", "centroid", 
                          "single", "ward.D","ward.D2", "mcquitty")
  d_i <- dist_dd(data, method = metodos_distancias[i_d])
  
  model_i <- hclust(d_i, method=metodos_agregacion[i_a]) 
  
  # encontramos la minima distancia que cumple el nro de clusters
  groups <- cutree(model_i, k)
  data$cluster <- factor(groups)
  
  ggplot(data) +
    geom_point(aes(V1,V2, col=cluster)) +
    theme() +
    ggtitle(paste0(k," clusters con distancia ",
                   metodos_distancias[i_d],
                   "\n y método de agregación ",
                   metodos_agregacion[i_a]))
  
}

clusters_jer(data.umap, 1, 8, 13)
plot(clusters_jer(data.umap, 1, 8, 13))
xy=data_final[36:37]%>% rename(V1=X,V2=Y)
clusters_jer(xy, 1, 7, 13)
#Evaluar el arbol sin XY
model.umap <- umap(data_final[,1:35])

data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
# exploramos la data 
data.umap %>% summary()

# exploramos graficamente la data

ggplot(data.umap) +
  geom_point(aes(V1,V2))

#para clusterizar:
distancia <- dist(data.umap)
model_complete <- hclust(distancia, method="complete")
summary(model_complete)
plot(model_complete)

# visualizamos los grupos resultantes
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(groups))) +
  theme()
# evaluemos como evoluciona el numero de clusters con h
d <- dist(data.umap)

# analizo graficamente la distribucion de las distancias entre puntos
hist(d)

# creamos un vector vacio para almacenar los resultados
res <- tibble("h" = quantile(d, probs  = (1:99)/100), n = 0, silh = 0)

# recorremos los 100 percentiles y vamos rellenando el vector con la distancia intra cluster
for (i in 1:99){
  groups <- cutree(model_complete, h = res$h[i])
  
  res$silh[i] <- summary(silhouette(groups,dist(data.umap)))$si.summary[3]
  res$n[i] <- groups %>% unique() %>% length()
}  

# visualizamos la silueta vs h
ggplot(res, aes(h, silh)) + 
  geom_point()
#H=20.75 TENGO 13 CLUSTERS
groups <- cutree(model_complete, h = 19)  
groups %>% unique() %>% length()
#INDICA QUE LO MEJOR SERIAN 16 CLUSTERS.

############################ EVALUACION DE CLUSTERS #####################################
#CLUSTER SIN XY.
model.umap <- umap(data_final[1:35])
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-kmeans(data.umap,13)
modelo1$cluster

mat_dis <- 
  data.umap %>% 
  arrange(modelo1$cluster) %>% 
  desc() %>%
  dist() %>% 
  as.matrix() 

image(mat_dis)
inspeccion_visual(data.umap, modelo1$cluster)



eval <- evaluacion_cluster(data.umap, modelo1$cluster)
eval

?mclust

