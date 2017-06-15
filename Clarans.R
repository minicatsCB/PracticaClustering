# Funciones para calcular la distancia euclídea o manhattan
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
manhattan.dist <- function(x1, x2) abs(x1[,1] - x2[,1]) + abs(x1[,2] - x2[,2])

calculateDistance <- function(x1, x2, type) {
  if(type == "euclidean") {
    return (euc.dist(x1, x2))
  } else if(type == "manhattan") {
    return (manhattan.dist(x1,x2))
  } else {
    return (0)
  }
}



calculateMedoidsAndDistances <- function(dataset, medoids, metric){
  # Aquí guardamos la distancia de cada mediana a cada una de las instancias
  # Es un dataframe para manejarlo mejor
  result = data.frame(matrix(0, ncol = 2, nrow = nrow(dataset)))
  # Le ponemos nombres a las columnas del dataframe
  colnames(result) <- c("Medoid", "DistanceToMedoid")
  
  # Por cada instancia (i) en el dataframe, calculamos a cuánto está de la mediana (j) y
  # lo guardamos en la matriz
  for(i in 1:nrow(dataset)){
    # Cada nueva instacia empezamos de nuevo
    distances <- c()
    
    for(j in 1:nrow(medoids)){
      # dataset[i,] es la fila entera de la instancia. Ej: 3.5 6.1 (el ancho y el alto)
      # medoids[j] es la fila entera de la mediana. Ej 2.5 2.1
      # metric es el tipo de distancia. Ej "Euclidean"
      distances[j] <- calculateDistance(dataset[i,], medoids[j,], metric)
    }
    result[i, 1] = row.names(medoids[which.min(distances), ])
    result[i, 2] = min(distances)
  }
  
  return (result)
}



# metric = Tipo de distancia, Euclídea o Manhattan
# k = Número de clústers
# l = Número máximo de iteraciones en general de CLARANS
# m = Número máximo de iteraciones de PAM dentro de CLARANS
clarans <- function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 12){
  # Initializations
  # Aquí guardamos la distancia de cada mediana a cada una de las instancias
  # Es un dataframe para manejarlo mejor
  medoidsWithDistances = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  # Le ponemos nombres a las columnas del dataframe
  colnames(medoidsWithDistances) <- c("Medoid","DistanceToMedoid")
  
  # Aquí guardamos la distancia de cada mediana a cada una de las instancias
  # Es un dataframe para manejarlo mejor
  # La diferencia es que esta es auxiliar para actualizar el error absoluto más adelante
  tempMedoidsWithDistances = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  # Le ponemos nombres a las columnas del dataframe
  colnames(tempMedoidsWithDistances) <- c("Medoid","DistanceToMedoid")
  
  #1. Repetir l veces
  while(l > 0){
    #1.1 Selecciona k instancias al azar como medianas
    medoids = x[sample(nrow(x), k), ]
    
    iterations = 0
    
    #2. Repetir
    repeat{
      # 2.1 Re/asignar instancias a la partición con la mediana más próxima
      medoidsWithDistances = calculateMedoidsAndDistances(x, medoids, metric)
      
      # 2.2 Selecciona una de las medianas al azar y otra instancia del cluster
      # de la mediana al azar
      medoidIndex <- sample(nrow(medoids), 1)
      medoid = medoids[medoidIndex, ]
      # row.names nos da el nombre o ID de la mediana de la que vamos a coger la instancia aleatoria
      instance = x[sample(which(medoidsWithDistances$Medoid == row.names(medoid)), 1), ]
      
      # 2.3 Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana
      # El error absoluto es la suma de las distancias al cuadrado de cada instancia a su mediana[medoidIndex, ]
      tempMedoids <- medoids
      tempMedoids <- tempMedoids[-medoidIndex, ]  # Con el índice negativo estamos eliminando lo que hay en esa posición
      #tempMedoids[medoidIndex, ] <- instance[]  # En la posición que acabamos de eliminar insertamos la nueva instancia
      tempMedoids <- rbind(tempMedoids, instance)
      
      # Ahora que hemos "swapeado" temporalmente la mediana y la instancia,
      # calculamos las distancias temporales del cada una de las instancias
      # a las medianas, para poder comparar los errores absolutos
      tempMedoidsWithDistances = calculateMedoidsAndDistances(x, tempMedoids, metric)
      
      
      # Calculamos el error absoluto
      originalAbsError = sum(medoidsWithDistances$DistanceToMedoid)
      tempAbsError = sum(tempMedoidsWithDistances$DistanceToMedoid)
      
      if(tempAbsError < originalAbsError){
        # Cambiamos "definitivamente" los datos originales
        medoidsWithDistances$Medoid = tempMedoidsWithDistances$Medoid
        medoidsWithDistances$DistanceToMedoid = tempMedoidsWithDistances$DistanceToMedoid
        # Esto se podría hacer directamente: medoidsWithDistances = tempMedoidsWithDistances???
        medoids = tempMedoids
        # En cuanto hay un cambio, comenzamos de nuevo
        iterations = 0
      }
      else{
        # Intentamos de nuevo
        iterations = iterations + 1
      }
      
      # Cuando se alcancen m iteraciones sin cambios
      if(iterations >= m){
        break
      }
      
      l <- l - 1
    }
    
    return(list(clusters =  medoidsWithDistances$Medoid, medoids = medoids, finalBestAbsoluteError = originalAbsError))
  }
}

# CLARANS SEPAL
k = 3
metric = "euclidean"
l = 12
m = 5
cluster <- clarans(x <- iris[1:2], k, metric, FALSE, l, m)
cluster$clusters <- as.factor(cluster$clusters) # Convert to int
cluster$medoids <- as.data.frame(cluster$medoids)

colors <- paste(row.names(cluster$medoids), " - Medoid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clusters)) +
  geom_point() +
  ggtitle("CLARANS Sepal") +
  geom_point(shape = 15, size = 2, data = cluster$medoids, mapping = aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1) +
  theme(plot.title = element_text(hjust = 0.5))

print(cluster$finalBestAbsoluteError)

# CLARANS Petal
cluster <- clarans(x <- iris[3:4], k, metric, FALSE, l, m)
cluster$clusters <- as.factor(cluster$clusters)  # Convert to int
cluster$medoids <- as.data.frame(cluster$medoids)  # Convert matrix to dataframe

colors <- paste(row.names(cluster$medoids), " - Medoid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clusters)) +
  geom_point() +  # Dibuja los puntosoriginales del dataset
  ggtitle("CLARANS Petal") +  # Define el título de la gráfica
  geom_point(shape = 15, size = 2, data = cluster$medoids, mapping = aes(Petal.Length, Petal.Width, color = factor(colors))) +  # Colorea según el cluster
  coord_fixed(ratio = 1) +  # Para que la gráfica salga en proporción 1:1
  theme(plot.title = element_text(hjust = 0.5))  # Para que el título de la gráfica salga centrado

print(cluster$finalBestAbsoluteError)  # Imprime el error absoluto

# Este condicional hace de comentario multilínea
if(FALSE){
  # PAM Sepal
  p <- pam(iris[1:2], 3, metric="manhattan")
  p$clustering <- as.factor(p$clustering)
  p$medoids <- as.data.frame(p$medoids)
  
  colors <- paste(row.names(p$medoids), " - Medoid", sep="")
  ggplot(iris, aes(Sepal.Length, Sepal.Width, color = p$clustering)) +
    geom_point() +
    ggtitle("PAM Sepal - Manhattan") +
    geom_point(shape = 15, size = 2, data = p$medoids, mapping = aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
    coord_fixed(ratio = 1) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p$objective)
  
  # PAM Petal
  p <- pam(iris[3:4], 3, metric="manhattan")
  p$clustering <- as.factor(p$clustering)
  p$medoids <- as.data.frame(p$medoids)
  
  colors <- paste(row.names(p$medoids), " - Medoid", sep="")
  ggplot(iris, aes(Petal.Length, Petal.Width, color = p$clustering)) +
    geom_point() +
    ggtitle("PAM Petal - Manhattan") +
    geom_point(shape = 15, size = 2, data = p$medoids, mapping = aes(Petal.Length, Petal.Width, color = factor(colors))) +
    coord_fixed(ratio = 1) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p$objective)
  
  # KMEANS Sepal
  k <- kmeans(iris[1:2], 3)
  k$cluster <- as.factor(k$cluster)
  k$centers <- as.data.frame(k$centers)
  colors <- paste(row.names(k$centers), " - Centroid", sep="")
  ggplot(iris, aes(Sepal.Length, Sepal.Width, color = k$cluster)) +
    geom_point() +
    ggtitle("KMEANS Sepal") +
    geom_point(shape = 15, size = 2, data = k$centers, mapping = aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
    coord_fixed(ratio = 1) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(k$totss) # Total sum of squares
  
  # KMEANS Petal
  k <- kmeans(iris[3:4], 3)
  k$cluster <- as.factor(k$cluster)
  k$centers <- as.data.frame(k$centers)
  colors <- paste(row.names(k$centers), " - Centroid", sep="")
  ggplot(iris, aes(Petal.Length, Petal.Width, color = k$cluster)) +
    geom_point() +
    ggtitle("KMEANS Petal") +
    geom_point(shape = 15, size = 2, data = k$centers, mapping = aes(Petal.Length, Petal.Width, color = factor(colors))) +
    coord_fixed(ratio = 1) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(k$totss)
  
  # CLARAN Sepal
  c <- clara(iris[1:2], 3, metric="manhattan")
  c$clustering <- as.factor(c$clustering)
  c$medoids <- as.data.frame(c$medoids)
  
  colors <- paste(row.names(c$medoids), " - Medoid", sep="")
  ggplot(iris, aes(Sepal.Length, Sepal.Width, color = c$clustering)) +
    geom_point() +
    ggtitle("CLARA Sepal- Manhattan") +
    geom_point(shape = 15, size = 2, data = c$medoids, mapping = aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
    coord_fixed(ratio = 1) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(c$objective)
  
  # CLARAN Petal
  c <- clara(iris[3:4], 3, metric="manhattan")
  c$clustering <- as.factor(c$clustering)
  c$medoids <- as.data.frame(c$medoids)
  
  colors <- paste(row.names(c$medoids), " - Medoid", sep="")
  ggplot(iris, aes(Petal.Length, Petal.Width, color = c$clustering)) +
    geom_point() +
    ggtitle("CLARA Petal - Manhattan") +
    geom_point(shape = 15, size = 2, data = c$medoids, mapping = aes(Petal.Length, Petal.Width, color = factor(colors))) +
    coord_fixed(ratio = 1) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(c$objective)
}

