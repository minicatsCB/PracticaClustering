
# Calcula el centro más cercano de cada instancia
calcularDistribucion <- function(sepalMatrix, k, metrix, stand, l, m, centros){
  
  # Guarda la distancia de una instancia al centro
  distanciaACentro <- c(0,0);
  # Guarda la distancia desde cada centro a cada una de las instancias
  distanciasTotales = matrix(nrow = k, ncol = 150)
  
  for(centro in 1:k){
    for(instancia in 1:150){
      distanciaACentro[1] <- sepalMatrix[instancia, 1] - centros[centro, 1]
      distanciaACentro[2] <- sepalMatrix[instancia, 2] - centros[centro, 2]
      moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
      
      distanciasTotales[centro, instancia] <- moduloDistancia
    }
  }
 
  #x <- dist(rbind(sepalMatrix[1:2], centros))
  print(sepalMatrix[1:2])
  
  
  # Guarda cada instancia (SU ÍNDICE, no sus datos) en su cluster más cercano
  for(instancia in 1:150){
    indiceCluster <- which.min(distanciasTotales[ , instancia])
    sepalMatrix$clusterIndex[instancia] <- indiceCluster
  }
  
  erroresAbsolutos <- c(0, 0, 0) # El error absoluto para cada cluster
  distanciaACentro <- c(0, 0)
  # Calcula el error absoluto de cada cluster
  for(cluster in 1:k){
    # Si hay algo en el cluster, operamos
    erroresAbsolutos[k] <- 0
    for(instancia in 1:150){
      if(sepalMatrix$clusterIndex[instancia] == k){
        distanciaACentro[1] <- sepalMatrix[instancia, 1] - centros[cluster , 1]
        distanciaACentro[2] <- sepalMatrix[instancia, 2] - centros[cluster , 2]
        moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
        
        erroresAbsolutos[cluster] <- erroresAbsolutos[cluster] + (moduloDistancia ^ 2)
      }
    }
  }
  
  errorAbsolutoTotal <- 0
  for(cluster in 1:k){
    errorAbsolutoTotal <- errorAbsolutoTotal + erroresAbsolutos[cluster]
  }
  
  listaADevolver <- list("errorAbsolutoTotal" = errorAbsolutoTotal, "sepalMatrix" = sepalMatrix, "centros" = centros)
  return(listaADevolver)
} # Fin función calcularDistribucion()


init <- function(sepalMatrix, k, metrix, stand, l, m){
  # Elige un número de instancias aleatorias (guarda el índice de la fila)
  print("En init")
  k <- 3
  indicesAleatorios <- c();
  for(i in 1: k){
    indicesAleatorios <- c(indicesAleatorios, sample(1:150, 1))
  }
  
  # Esta matriz contiene las instancias que van a ser los centros de los clusters
  centros = matrix(nrow = k, ncol = 2)
  print(centros)
  
  # Guarda las instancias cuyos índices han sido elegidos
  for(i in 1:k){
    for(j in 1:2){
      centros[i, j] <- petalMatrix[indicesAleatorios[i], j]
    }
  }
  
  l <- 0
  m <- 0 # Contador de las iteraciones NO mejoradas
  while(l < 5){
    while(m < 5){
      print("l")
      print(l)
      print("m")
      print(m)
      
      # Primera iteración
      listaRecibida <- calcularDistribucion(sepalMatrix, 3, "euclidean", false, 5, 5, centros)
      
      # Selecciona el cluster para hacer el cambio
      clusterParaHacerCambio <- sample(1:3, 1)
      
      # Selecciona una instancia de su cluster (coge los índices)
      instancias <- c()
      indice <- sample(3, 1)
      for(i in 1:150){
        if(listaRecibida$sepalMatrix$clusterIndex[i] == indice){
          instancias <- c(instancias, i)
        }
      }
      
      if(length(instancias) == 0){
        print("No hay elementos en el cluster")
        print(indice)
      }else{
      
      # Coge los datos
      instanciaParaHacerCambio <- listaRecibida$sepalMatrix[sample(length(instancias), 1), ]
      
      # Guarda una copia de seguridad del centro del cluster
      antiguoCentroCluster <- centros[indice, ]
      # Actualiza los centros
      centros[indice, 1] <- instanciaParaHacerCambio[, 1]
      centros[indice, 2] <- instanciaParaHacerCambio[, 2]
      # Recalcula la distribucion
      listaRecibidaNueva <- calcularDistribucion(sepalMatrix, 3, "euclidean", false, 5, 5, centros)
      # Si mejoramos el error absoluto, reemplazamos los centros
      if(listaRecibidaNueva$errorAbsolutoTotal < listaRecibida$errorAbsolutoTotal){
        print("Error 1111111 mejorado!")
        #print(centros)
        m <- 0
      }
      # Si no mejoramos el error absoluto, volvemos hacia atrás
      else{
        print("Error 1111111 NO Mejorado!")
        centros[indice, 1] <- antiguoCentroCluster[1]
        centros[indice, 2] <- antiguoCentroCluster[2]
        m = m + 1
      }
     }
     # Actualizamos el contador de iteraciones
     l <- l + 1
    }
  }
  
  return(listaRecibidaNueva)
}


# Dividimos el dataset iris en por el sépalo y el pétalo de la flor
sepalMatrix <- iris
petalMatrix <- iris
sepalMatrix$Petal.Length <- NULL
sepalMatrix$Petal.Width <- NULL
petalMatrix$Sepal.Length <- NULL
petalMatrix$Sepal.Width <- NULL
sepalMatrix$Species <- NULL
petalMatrix$Species <- NULL
# Añadimos una columna el petal que guarda el cluster al que pertenece
petalMatrix["clusterIndex"] <- 0
sepalMatrix["clusterIndex"] <- 0

listaRecibida <- init(sepalMatrix, 3, "euclidean", false, 5, 5)

#print(listaRecibida$sepalMatrix)

centrosFinales <- as.data.frame(listaRecibida$centros)

plot(listaRecibida$sepalMatrix$Sepal.Length, listaRecibida$sepalMatrix$Sepal.Width, pch=17, col = ifelse(listaRecibida$sepalMatrix$clusterIndex == 1, "red", ifelse(listaRecibida$sepalMatrix$clusterIndex == 2, "blue", "black")))
points(centrosFinales$V1, centrosFinales$V2, pch = 16, col = "green")
