rm(list=ls())
pacman::p_load(heatmaply, stuart, lavaan, FSinR, magick, factoextra, umap, ClustGeo, dplyr)

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

data_final[, c('X.1', 'cod_com', 'COMUNA', 'MANZ_EN')] <- list(NULL)

# # Fuerza bruta 
# 
# results <- bruteforce(data_final, list(ra = names(data_final)), 18)
# 
# summary(results) 

# Test 1

evaluator <- filterEvaluator('determinationCoefficient')
searcher <- searchAlgorithm('sequentialForwardSelection')

results <- featureSelection(data_final, 'ID_MANZ', searcher, evaluator)
results$bestFeatures

# Test 2

directSearcher <- directSearchAlgorithm('selectKBest', list(k=18))

results2 <- directFeatureSelection(data_final, 'ID_MANZ', directSearcher, evaluator)
results2$bestFeatures

# Test 3

evaluator <- filterEvaluator('chiSquared')

results3 <- directFeatureSelection(data_final, 'ID_MANZ', directSearcher, evaluator)
results3$bestFeatures

## Data con variables: E6A14 E15A24 dim_acc iav idep isal iata icv

data_pp = data_final[, c('ID_MANZ', 'ZONA', 'ibt','E6A14', 'E15A24', 'dim_acc', 'iav', 'idep', 'isal', 'ise', 'iata', 'icv', 'dim_soc', 'ivi', 'irh', 'iem', 'ipj', 'dim_seg', "AREA", 
                         "TOTAL_V" ,"HOG_N","PERSONAS", 'X', 'Y')]
data_pp['E6A24'] = data_pp$E6A14 + data_pp$E15A24
data_pp[, c('E6A14', 'E15A24', 'X', 'Y')] <- list(NULL)



data_pp_xy = data_final[, c('ID_MANZ', 'ZONA', 'ibt','E6A14', 'E15A24', 'dim_acc', 'iav', 'idep', 'isal', 'ise', 'iata', 'icv', 'dim_soc', 'ivi', 'irh', 'iem', 'ipj', 'dim_seg', "AREA", 
                            "TOTAL_V" ,"HOG_N","PERSONAS", 'X', 'Y')]
data_pp_xy['E6A24'] = data_final$E6A14 + data_final$E15A24
data_pp_xy[, c('E6A14', 'E15A24')] <- list(NULL)

get_clust_tendency(data_pp, n = 5, graph = FALSE)

# #CLUSTER SIN XY.

# model.umap <- umap(data_pp)
# data.umap <-
#   model.umap$layout %>%
#   as.data.frame()
# modelo1<-kmeans(data.umap,13)
# modelo1$cluster
# ggplot(data.umap) +
#   geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
#   theme()

######################################################################################################
# GMM

source("clusteriza.R")
models <- c("kmeans", "hier", "gmm", "cmeans", "dbscan")
cat("Metodo de clasificacion:", models[3])
#CLUSTER SIN XY.
print("cluster sin XY")
model.umap <- umap(data_pp)
data.umap <-
  model.umap$layout %>%
  as.data.frame()
modelo1<-clusteriza(data.umap, models[3], k = 13)

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme() + labs(title= "Cluster sin variables de posición")

#Cluster sin XY en mapa XY
ggplot(data_pp_xy[21:22]) +
  geom_point(aes(X,Y, col=factor(modelo1$cluster))) +
  theme()+
  labs(title =  "Cluster sin variables de posición",col="Clusters")

##############################################################################


#CLUSTER POR XY en kmedias

modelo2<-kmeans(data_pp_xy[,21:22],13)
ggplot(data_pp_xy[,21:22]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()

#UMAP CON CLUSTER POR XY.

ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
  theme()

# Parte 3

xy =  data_final[, c('X', 'Y')]

dists_vars = dist(data_pp, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dists_vars = as.dist(dists_vars)

dists = dist(xy, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dists = as.dist(dists) 

dists_alt = as.matrix(dists)
  
bin = matrix(data=NA, nrow=nrow(dists), ncol=ncol(dists))
bin

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

bin

D1 <- as.dist(1-bin)
D1

range.alpha <- seq(0,1,0.1)
K <- 13
cr <- choicealpha(dists_vars, D1, range.alpha,
                  K, graph=FALSE)
plot(cr)
cr$Q

tree <- hclustgeo(dists_vars,D1,alpha=0.8)
P5bis <- cutree(tree,13)

ggplot(xy) +
  geom_point(aes(X,Y, col=factor(P5bis))) +
  theme()

#Cluster 2 y 3

modelo2$cluster
xy['cluster1'] = modelo1$cluster
xy['cluster2'] = modelo2$cluster
xy['cluster3'] = P5bis

vec2 = c()

for(i in 1:13){
  
  xy1 = xy[xy$cluster2 == i, ]
  dists1 = as.matrix(dist(xy1[1:2], method = "euclidean", diag = FALSE, upper = FALSE, p = 2))
  dists1[lower.tri(dists1)] <- 0
  bin2 = dists1 > 2000
  
  vec2 = cbind(vec2, sum(bin2))
  
}

vec3 = c()

for(i in 1:13){
  xy2 = xy[xy$cluster3 == i, ]
  dists2 = as.matrix(dist(xy2[1:2], method = "euclidean", diag = FALSE, upper = FALSE, p = 2))
  dists2[lower.tri(dists2)] <- 0
  bin3 = dists2 > 2000
    
  vec3 = cbind(vec3, sum(bin3))
  
}

vec2
vec3

#Cluster 1 y 3

vec = c()

for(i in 1:13){
  
  xy3 = xy[xy$cluster1 == i, ]
  dists3 = as.matrix(dist(xy3[1:2], method = "euclidean", diag = FALSE, upper = FALSE, p = 2))
  dists3[lower.tri(dists3)] <- 0
  bin1 = dists3 > 2000
  
  vec = cbind(vec, sum(bin1))
  
}

sum(vec)
sum(vec2)
sum(vec3)



