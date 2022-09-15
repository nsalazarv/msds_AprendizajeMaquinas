### Script Tarea 1 MAAMS: Sebastián Novoa, Nelson Salazar y Nicolás Valenzuela.

rm(list=ls())
pacman::p_load(dplyr,psych,ggplot2,tidyverse
               ,proxy,dplyr,umap, ggdendro, cluster,dbscan,
               factoextra, flexclust,magick, ClustGeo)
set.seed(5720)
##Cargamos los datos y clusteriza.####
data<-read.csv("datos_t1_centroides.csv",sep=",",header=TRUE)
source("clusteriza.R")


##Pre-Procesamiento:##
#Analisis nulos####
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
apply(X=is.na(data),MARGIN=2,FUN =sum)[valores_col]

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


#Estadistica Descriptiva.####
str(data)
count(unique(data_final["ZONA"]))
unique(data["COMUNA"])
unique(data["cod_com"])
unique(data["MANZ_EN"])
#Notamos que las variables X1., cod_com, COMUNA, MANZ_EN no aportan valores diferentes.

#eliminamos X1 por ser los indices y las demás columnas por ser el mismo valor para todos los datos.
data_final$X.1<-NULL
data_final$COMUNA<-NULL
data_final$cod_com<-NULL
data_final$MANZ_EN<-NULL
#Analisis duplicados: No hay filas duplicadas.
sum(duplicated(data_final))
#Analisis de variables
str(data_final)

#Analizamos las dimensiones y las variables dentro de ella con el objeto de
#Encontrar correlaciones y poder disminuir la dimensionalidad del dataset.
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

#En base al analisis realizado anteriormente y de un estudio de las variables
#Obtenemos el nuevo dataset:
data_pp_xy = data_final[, 
                        c('ID_MANZ', 'ZONA', 'ibt',"dim_acc",  
                          "iav", "idep","isal","ise", 'iata', 'icv', "dim_soc",  
                          "ivi","irh","iem","ipj","dim_seg",'E6A14', 'E15A24',"AREA", 
                          "TOTAL_V" ,"HOG_N","PERSONAS")]
data_pp_xy['E6A24'] = data_final$E6A14 + data_final$E15A24
data_pp_xy[, c('E6A14', 'E15A24')] <- list(NULL) 
data_pp_xy<-cbind(data_pp_xy,data_final[36:37])



##Analisis de clusters:####
#Revisamos la tendencia natural a formar cluster de las distintas datas:
#SIN XY:
print("Data sin limpiar variables:")
get_clust_tendency(data_final[1:35], n = 40, graph = FALSE)
print("Data con menos dimensiones:")
get_clust_tendency(data_pp_xy[1:21], n = 40, graph = FALSE)
#Data XY:
print("Data XY:")
get_clust_tendency(data_pp_xy[22:23], n = 40, graph = FALSE)
#Notamos que la data con menos dimensiones mejora la tendencia a formar cluster
#de forma pequeña y que la data de XY posee menos tendencia a cluster.

#Definimos los metodos de cluster que revisaremos:
models <- c("kmeans", "hier", "gmm", "cmeans")
#funcion de evaluacion:
evaluacion_cluster <- function(data, clusters){
  coefSil <- silhouette(clusters,dist(data))
  cohesiones <- clus_cohes(data, clusters)
  separaciones <- clus_sep(data, clusters)
  correlacion <- clus_cor(data, clusters)
  return(list(correlacion, mean(coefSil[,3]), cohesiones, separaciones,coefSil))
}
#Correlaciones y Sillohuete promedio distintos modelos en Data sin XY y con solo XY.

cor_s<-c()
sil_s<-c()
cor_xy<-c()
sil_xy<-c()
data_model<-data_pp_xy[1:21]
for(i in 1:4){
  cat("###############","Metodo de clasificacion:", models[i],"#########################")
  #CLUSTER SIN XY.
  print("cluster sin XY")
  model.umap <- umap(data_model)
  data.umap <- 
    model.umap$layout %>% 
    as.data.frame()
  modelo1<-clusteriza(data.umap, models[i], k = 13)
  
  eval <- evaluacion_cluster(data.umap, modelo1$cluster)
  cor_s<-cbind(cor_s,eval[1])
  sil_s<-cbind(sil_s,eval[2])
  
  #CLUSTER POR XY
  print("cluster por XY")
  modelo2<-clusteriza(data_pp_xy[22:23], models[i], k = 13)
  eval <- evaluacion_cluster(data_pp_xy[22:23], modelo2$cluster)
  cor_xy<-cbind(cor_xy,eval[1])
  sil_xy<-cbind(sil_xy,eval[2])
}
resultados<-rbind(models,cor_s,sil_s,cor_xy,sil_xy)
resultados<-cbind(c("metodo","cor_s","sil_s","cor_xy","sil_xy"),resultados)
resultados


##Problema 1:####
#En base a resultados anteriores decidimos usar GMM:
cat("Metodo de clasificacion:", models[3])
#CLUSTER SIN XY.
model.umap <- umap(data_model)
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-clusteriza(data.umap, models[3], k = 13)

eval <- evaluacion_cluster(data.umap, modelo1$cluster)
eval
coefSil <- silhouette(modelo1$cluster,dist(data.umap))
fviz_silhouette(coefSil) + 
  coord_flip()

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme() + labs(title= "Cluster sin variables de posición")

#Cluster sin XY en mapa XY
ggplot(data_pp_xy[22:23]) +
  geom_point(aes(X,Y, col=factor(modelo1$cluster))) +
  theme()+
  labs(title =  "Cluster sin variables de posición",col="Clusters")




##Problema 2:####
modelo2<-kmeans(data_pp_xy[22:23],13)
ggplot(data_pp_xy[22:23]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()+labs(title = "Clusters por posición", col="Clusters")
coefSil <- silhouette(modelo2$cluster,dist(data_pp_xy[22:23]))
fviz_silhouette(coefSil) + 
  coord_flip()
eval <- evaluacion_cluster(data_pp_xy[22:23], modelo2$cluster)
eval
#Comparacion problema 1 y 2:
library(patchwork)
a<-ggplot(data_pp_xy[22:23]) +
  geom_point(aes(X,Y, col=factor(modelo1$cluster))) +
  theme()+
  labs(title =  "Cluster sin variables de posición",col="Clusters")
b<-ggplot(data_pp_xy[22:23]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+labs(title = "Clusters por posición", col="Clusters")

a+b
##Problema 3:####
#Generamo data solo de XY
xy =data_pp_xy[22:23]
#Matriz de distancia de las variables
dists_vars = dist(data_model, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dists_vars = as.dist(dists_vars)
#Distancia de XY
dists = dist(xy, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dists = as.dist(dists) 
dists_alt = as.matrix(dists)
#Generamos matriz llena de nulos con las dimenciones de distancia.
bin = matrix(data=NA, nrow=nrow(dists), ncol=ncol(dists))

#Llenamos con 1 los que cumplen con la condición de la restriccion
for(i in 1:ncol(dists)){
  for(j in 1:nrow(dists)){
    if(dists_alt[i,j]<= 2000){
      
      bin[i,j] = 1
    }
    else{
      bin[i,j] = 0
    }
  }
}

#Generamos matriz de disimilitud.
D1 <- as.dist(1-bin)

#Generamos un rango de alpha para ver que alpha seleccionar para nuestro modelo.
range.alpha <- seq(0,1,0.1)
K <- 13
cr <- choicealpha(dists_vars, D1, range.alpha,
                  K, graph=FALSE)
#generamos el plot
plot(cr)
#Tabla de valores.
cr$Q

#Como queremos darle prioridad a la restriccion de 2000 metros utilizaremos un alpha=0.8.
#el modelo corresponde a:
tree <- hclustgeo(dists_vars,D1,alpha=0.8)
P5bis <- cutree(tree,13)

ggplot(xy) +
  geom_point(aes(X,Y, col=factor(P5bis))) +
  theme()+labs(title = "Clusters por modelo mixto", col="Clusters")

#Evaluación del modelo y comparación.
coefSil <- silhouette(P5bis,dist(data_pp_xy))
fviz_silhouette(coefSil) + 
  coord_flip()
eval <- evaluacion_cluster(data_pp_xy, P5bis)
eval

#Comparacion, asociamos los cluster de cada modelo a las coordenadas XY.
xy['cluster1'] = modelo1$cluster
xy['cluster2'] = modelo2$cluster
xy['cluster3'] = P5bis

vec = c()
vec2= c()
vec3= c()

for(i in 1:13){
  
  xy3 = xy[xy$cluster1 == i, ]
  dists3 = as.matrix(dist(xy3[1:2], method = "euclidean", diag = FALSE, upper = FALSE, p = 2))
  dists3[lower.tri(dists3)] <- 0
  bin1 = dists3 > 2000
  
  vec = cbind(vec, sum(bin1))
  
}
for(i in 1:13){
  
  xy1 = xy[xy$cluster2 == i, ]
  dists1 = as.matrix(dist(xy1[1:2], method = "euclidean", diag = FALSE, upper = FALSE, p = 2))
  dists1[lower.tri(dists1)] <- 0
  bin2 = dists1 > 2000
  
  vec2 = cbind(vec2, sum(bin2))
  
}
for(i in 1:13){
  xy2 = xy[xy$cluster3 == i, ]
  dists2 = as.matrix(dist(xy2[1:2], method = "euclidean", diag = FALSE, upper = FALSE, p = 2))
  dists2[lower.tri(dists2)] <- 0
  bin3 = dists2 > 2000
  
  vec3 = cbind(vec3, sum(bin3))
}

print("Datos sobre 2000 por cluster")
cluster_r=list("modelo GMM sin_xy"=vec,"modelo Kmeans Xy"=vec2, "modelo con restriccion"=vec3)
cluster_r
print("Datos totales sobre 2000 por modelo")
cluster_sum=list("modelo GMM sin_xy"=sum(vec),"modelo Kmeans Xy"=sum(vec2), "modelo con restriccion"=sum(vec3))
cluster_sum

#Modelo disminuye la cantidad de datos sobre 2000 de GMM pero aumenta respecto al clustering de solo XY.
#La restricción o el modelo empeora respecto a los otros dos incisos las metricas de evaluacion.
