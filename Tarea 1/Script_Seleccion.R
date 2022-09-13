rm(list=ls())
pacman::p_load(heatmaply, stuart, lavaan, FSinR, magick, factoextra, umap, ClustGeo)

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

data_pp = data_final[, c('ID_MANZ', 'ZONA', 'ibt','E6A14', 'E15A24', 'dim_acc', 'iav', 'idep', 'isal', 'ise', 'iata', 'icv', 'dim_soc', 'irh', 'iem', 'ipj', 'dim_seg', 'X', 'Y')]
data_pp['E6A24'] = data_pp$E6A14 + data_pp$E15A24
data_pp[, c('E6A14', 'E15A24', 'X', 'Y')] <- list(NULL)

get_clust_tendency(data_pp, n = 5, graph = FALSE)

#CLUSTER SIN XY.
model.umap <- umap(data_pp)
data.umap <- 
  model.umap$layout %>% 
  as.data.frame()
modelo1<-kmeans(data.umap,13)
modelo1$cluster
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo1$cluster))) +
  theme()

data_pp_xy = data_final[, c('ID_MANZ', 'ZONA', 'ibt','E6A14', 'E15A24', 'dim_acc', 'iav', 'idep', 'isal', 'ise', 'iata', 'icv', 'dim_soc', 'irh', 'iem', 'ipj', 'dim_seg', 'X', 'Y')]
data_pp_xy['E6A24'] = data_pp$E6A14 + data_pp$E15A24
data_pp_xy[, c('E6A14', 'E15A24')] <- list(NULL)

#CLUSTER POR XY
modelo2<-kmeans(data_pp_xy[,16:17],13)
ggplot(data_pp_xy[,16:17]) +
  geom_point(aes(X,Y, col=factor(modelo2$cluster))) +
  theme()
#UMAP CON CLUSTER POR XY.
ggplot(data.umap) +
  geom_point(aes(V1,V2, col=factor(modelo2$cluster))) +
  theme()

# Parte 3: Kmeans artesanal

xy =  data_final[, c('X', 'Y')]

#dists = as.matrix(dist(data_pp_xy[, c('X', 'Y')], method = "euclidean", diag = FALSE, upper = FALSE, p = 2))

vec_norm <- norm(xy, type = "2")  
vec_norm

kmedias <- function(data, coords, k, itermax = 10){

  # defino los centroides aleatoriamente seleccionando una muestra de la data recibida
  centroids <- data[sample(1:nrow(data), k),]
  
  # Distancia entre puntos 
  
  dists = as.matrix(dist(coords, method = "euclidean", diag = FALSE, upper = FALSE, p = 2))
  
  # creo 2 variables auxiliares del mismo largo que me permitiran comparar si el algoritmo convergio
  cluster <- 1:nrow(data)
  cluster_iter <- cluster*0

  # creo un for para iterar hasta las iteraciones maximas
  
  for(i in 1:itermax){
    for(j in 1:ncol(dists)){
      for(k in 1:nrow(dists)){
        if(dists[j,k] <= 2000){
          
        
    
          if(!mean(cluster_iter == cluster)==1){ # si el algoritmo aun no converge
            # calculo distancia de puntos con centroides de acuerdo a funcion
            distk <- sqrt(matrix(rowSums(expand.grid(rowSums(centroids*centroids),rowSums(data*data))), # hago calculo de producto punto enntre matrices
                                 nrow=nrow(centroids)) - # calculo el tamaño de la matriz resultante
                            2. * as.matrix(centroids) %*% t(as.matrix(data)))
            
      
            cluster_iter <- cluster # reasigno la variable auxiliar al cluster obtenido en la iteracion anterior
            cluster <- apply(distk, 2, function(x) which(x==min(x))[1]) # identifico el cluster mas cercano a cada punto
            dist_min <- apply(distk, 2, function(x) min(x)) # identifico la distancia minima al cluster mas cercano
            output <- data.frame(dist_min, data, cluster) # construyo salida del modelo juntando la data con sus clusters asignados
            dist_clusts <- aggregate(.~cluster, output, mean) # agrego los datos por clusters obteniendo coordenadas y distancias medias
            centroids <- dist_clusts[,-(1:2)] # redefino los centroides
          }
        }
      }
    }
  }

  return(list("clusters" = cluster, "centroides" = centroids)) # funcion devuelve una lista con los clusters de cada punto y con los centroides

}

test = kmedias(data_pp, coords = xy, k = 13)

