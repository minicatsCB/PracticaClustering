distance = "euclidean"
k = 3
l = 5
m = 10

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
manhattan.dist <- function(x1, x2) abs(x1[,1] - x2[,1]) + abs(x1[,2] - x2[,2])

calculateDistance <- function(x1, x2, distanceType) {
  if(distanceType == "euclidean") {
    return (euc.dist(x1, x2))
  } else if(distanceType == "manhattan") {
    return (manhattan.dist(x1,x2))
  } else {
    return (0)
  }
}



calculateMediansAndDistances <- function(x, medians, distanceType){
  # Aquí guardamos la distancia de cada mediana a cada una de las instancias
  # Es un dataframe para manejarlo mejor
  result = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  colnames(result) <- c("Median", "DistanceToMedian")
  
  # Por cada instancia (i) en "x" calculamos a cuánto está de la mediana (j) y
  # lo guardamos en la matriz
  for(i in 1:nrow(x)){
    # Cada nueva instacia empezamos de nuevo
    distances <- c()
    
    for(j in 1:nrow(medians)){
      #x[i,] es la fila entera de la instancia. Ej: 3.5 6.1 (width lenght)
      #medians[j] es la fila entera de la mediana. Ej 2.5 2.1
      # distanceType es el tipo de distancia. Ej "Euclidean"
      distances[j] <- calculateDistance(x[i,], medians[j,], distanceType)
    }
    result[i, 1] = row.names(medians[which.min(distances), ])
    result[i, 2] = min(distances)
  }
  
  return (result)
}

clarans <- function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 10){
  # Initializations
  mediansWithDistances = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  colnames(mediansWithDistances) <- c("Medians","DistanceToMedian")
  
  tempMediansWithDistances = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  colnames(tempMediansWithDistances) <- c("Medians","DistanceToMedian")
  
  finalClusters = NA
  finalMedians = NA;
  finalBestAbsoluteError = NA;
  
  #1. Repetir l veces
  while(l > 0){
    #1.1 Selecciona k instancias al azar como medianas
    medians = x[sample(nrow(x), k), ]
    
    iterations = 0
    
    #2. Repetir
    repeat{
      #2.1 Re/asignar instancias a la partición con la mediana
      # más próxima
      mediansWithDistances = calculateMediansAndDistances(x, medians, metric)
      
      #2.2 Selecciona una de las medianas al azar y otra instancia del cluster
      # de la mediana al azar
      medianIndex <- sample(nrow(medians), 1)
      median = medians[medianIndex, ]
      # row.names nos da el nombre o ID de la mediana de la que vamos a coger la instancia aleatoria
      instance = x[sample(which(mediansWithDistances$Median == row.names(median)), 1), ]
      
      #2.3 Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana
      # El error absoluto es la suma de las distancias al cuadrado de cada instancia a su mediana[medianIndex, ]
      tempMedians <- medians[-medianIndex, ]
      tempMedians[medianIndex, ] <- instance[]
      
      # Ahora que hemos "swapeado" temporalmente la mediana y la instancia,
      # calculamos las distancias temporales del cada una de las instancias
      # a las medianas, para poder comparar los errores absolutos
      tempMediansWithDistances = calculateMediansAndDistances(x, tempMedians, metric)
      
      
      # Calculamos el error absoluto
      originalAbsError = `^`(sum(mediansWithDistances$DistanceToMedian), 2)
      tempAbsError = `^`(sum(tempMediansWithDistances$DistanceToMedian), 2)
      
      if(tempAbsError < originalAbsError){
        # Cambiamos "definitivamente" los datos originales
        mediansWithDistances$Median = tempMediansWithDistances$Median
        mediansWithDistances$DistanceToMedian = tempMediansWithDistances$DistanceToMedian
        # Esto se podría hacer directamente: mediansWithDistances = tempMediansWithDistances???
        medians = tempMedians
        # Comenzamos de nuevo
        iterations = 0
      }
      else{
        # Intentamos de nuevo
        iterations = iterations + 1
      }
      
      # Mientras haya cambios en las medians o se alcancen m iteraciones sin cambios
      if(iterations >= m){
        break
      }
      
      # Si no funciona, quitarlo (pero los valores salen muy diferentes)
      #if((is.na(finalClusters) && is.na(finalBestAbsoluteError) && is.na(finalMedians)) || !is.na(finalBestAbsoluteError) && originalAbsError < finalBestAbsoluteError) {
      #  finalClusters = mediansWithDistances$Median
      #  finalMedians = medians
      #  finalBestAbsoluteError = originalAbsError
      #}
      
      l <- l - 1
    }
    
    return(list(clusters =  mediansWithDistances$Median, medoids = medians, finalBestAbsoluteError = originalAbsError))
  }
}

cluster <- clarans(x <- iris[3:4], k, distance, FALSE, l, m)

cluster$clusters <- as.factor(cluster$clusters) # Convert to int
cluster$medoids <- as.data.frame(cluster$medoids)

colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clusters)) +
  geom_point() +
  ggtitle("CLARANS Petal") +
  geom_point(shape = 15, size = 2, data = cluster$medoids, mapping = aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1) +
  theme(plot.title = element_text(hjust = 0.5))
