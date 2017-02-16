
clarans <- function(x, k, l, m){
  init(x, k, l, m)
}

calcularDistribucion <- function(x, k){
  # Calcula el centro más cercano de cada instancia
  
  # Guarda la distancia de una instancia al centro
  distanciaACentro <- c(0,0);
  # Guarda la distancia desde cada centro a cada una de las instancias
  distanciasTotales = matrix(nrow = 5, ncol = 150)
  
  for(centro in 1:5){
    for(instancia in 1:150){
      distanciaACentro[1] <- petalMatrix[instancia, 1] - centros[centro, 1]
      distanciaACentro[2] <- petalMatrix[instancia, 2] - centros[centro, 2]
      moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
      
      distanciasTotales[centro, instancia] <- moduloDistancia
    }
  }
  
  # Reserva de los clusters (variables globales)
  cluster1 <<- c();
  cluster2 <<- c();
  cluster3 <<- c();
  cluster4 <<- c();
  cluster5 <<- c();
  
  # Guarda cada instancia (SU ÍNDICE, no sus datos) en su cluster más cercano
  for(instancia in 1:150){
    indiceCluster <- which.min(distanciasTotales[ , instancia])
    switch(indiceCluster, 
           {cluster1 <<- c(cluster1, instancia)}, 
           {cluster2 <<- c(cluster2, instancia)},
           {cluster3 <<- c(cluster3, instancia)},
           {cluster4 <<- c(cluster4, instancia)},
           {cluster5 <<- c(cluster5, instancia)},
           {print("Cluster no exists!")}
          )
  }
  
  
  # Calcula el error absoluto de cada cluster
  # Calcula el error absoluto del cluster 1
  errorAbsolutoCluster1 <- NULL
  if(length(cluster1) != 0){
  errorAbsolutoCluster1 <- 0
    for(instancia in 1:length(cluster1)){
      distanciaACentro[1] <- petalMatrix[cluster1[instancia], 1] - centros[1 , 1]
      distanciaACentro[2] <- petalMatrix[cluster1[instancia], 2] - centros[1 , 2]
      moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
      
      errorAbsolutoCluster1 <- errorAbsolutoCluster1 + (moduloDistancia ^ 2)
    }
  }
  
  # Calcula el error absoluto del cluster 2
  errorAbsolutoCluster2 <- NULL
  if(length(cluster2) != 0){
  errorAbsolutoCluster2 <- 0
    for(instancia in 1:length(cluster2)){
      distanciaACentro[1] <- petalMatrix[cluster2[instancia], 1] - centros[1 , 1]
      distanciaACentro[2] <- petalMatrix[cluster2[instancia], 2] - centros[1 , 2]
      moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
      
      errorAbsolutoCluster2 <- errorAbsolutoCluster2 + (moduloDistancia ^ 2)
    }
  }
  
  # Calcula el error absoluto del cluster 3
  errorAbsolutoCluster3 <- NULL
  if(length(cluster3) != 0){
    for(instancia in 1:length(cluster3)){
      errorAbsolutoCluster3 <- 0
      distanciaACentro[1] <- petalMatrix[cluster3[instancia], 1] - centros[1 , 1]
      distanciaACentro[2] <- petalMatrix[cluster3[instancia], 2] - centros[1 , 2]
      moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
      
      errorAbsolutoCluster3 <- errorAbsolutoCluster3 + (moduloDistancia ^ 2)
    }
  }
  
  # Calcula el error absoluto del cluster 4
  errorAbsolutoCluster4 <- NULL
  if(length(cluster4) != 0){
    errorAbsolutoCluster4 <- 0
    for(instancia in 1:length(cluster4)){
      distanciaACentro[1] <- petalMatrix[cluster4[instancia], 1] - centros[1 , 1]
      distanciaACentro[2] <- petalMatrix[cluster4[instancia], 2] - centros[1 , 2]
      moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
      
      errorAbsolutoCluster4 <- errorAbsolutoCluster4 + (moduloDistancia ^ 2)
    }
  }
  
  # Calcula el error absoluto del cluster 5
  errorAbsolutoCluster5 <- NULL
  if(length(cluster5) != 0){
    errorAbsolutoCluster5 <- 0
    for(instancia in 1:length(cluster5)){
      distanciaACentro[1] <- petalMatrix[cluster5[instancia], 1] - centros[1 , 1]
      distanciaACentro[2] <- petalMatrix[cluster5[instancia], 2] - centros[1 , 2]
      moduloDistancia <- sqrt((distanciaACentro[1]^2)+(distanciaACentro[2]^2))
      
      errorAbsolutoCluster5 <- errorAbsolutoCluster5 + (moduloDistancia ^ 2)
    }
  }
  
  # PARCHE CUTRE: si algún error absoluto es NULL, le damos valor cero para poder sumar el resto
  if(is.null(errorAbsolutoCluster1)){
    errorAbsolutoCluster1 <- 0
  }
  if(is.null(errorAbsolutoCluster2)){
    errorAbsolutoCluster2 <- 0
  }
  if(is.null(errorAbsolutoCluster3)){
    errorAbsolutoCluster3 <- 0
  }
  if(is.null(errorAbsolutoCluster4)){
    errorAbsolutoCluster4 <- 0
  }
  if(is.null(errorAbsolutoCluster5)){
    errorAbsolutoCluster5 <- 0
  }
  
  errorAbsolutoTotal <<- errorAbsolutoCluster1 + errorAbsolutoCluster2 + errorAbsolutoCluster3 + errorAbsolutoCluster4 + errorAbsolutoCluster5
  } # Fin función calcularDistribucion()




# Crea un array con los datos reales a partir de los índices de lso mismos guardados en cada cluster
crearMatrizConDatos <- function(clusterACopiar){
  # En esta nueva matriz vamos a copiar los datos del cluster 1 para plotearlos
  plotCluster = matrix(nrow = length(clusterACopiar), ncol = 2)
  for(i in 1:length(clusterACopiar)){
    plotCluster[i, 1] = petalMatrix[clusterACopiar[i], 1]
    plotCluster[i, 2] = petalMatrix[clusterACopiar[i], 2]
  }
  return(plotCluster)
}




init <- function(x, k, l, m){
  ll <- 0
  mm <- 0 # Contador de las iteraciones NO mejoradas
  while(ll < l){
    while(mm < m){
      print("l")
      print(ll)
      print("m")
      print(mm)
      
      # Primera iteración
      z <- calcularDistribucion(x, k)
      
      # Selecciona el cluster para hacer el cambio
      # Comprueba que no selecciona un cluster sin datos
      clusterVacio <- FALSE
      repeat{
        clusterParaHacerCambio <- sample(1:5, 1)
        switch(clusterParaHacerCambio, 
               {
                 if(!(is.null(cluster1))){
                  break;
                 }
               }, 
               {
                 if(!(is.null(cluster2))){
                  break;
                 }
               },
               {
                 if(!(is.null(cluster3))){
                  break;
                 }
               },
               {
                 if(!(is.null(cluster4))){
                  break;
                 }
               },
               {
                 if(!(is.null(cluster5))){
                  break;
                 }
               },
               {
                 print("Cluster no exists!")
               }
        )
      }
      
      switch(clusterParaHacerCambio, 
             {
               # Selecciona una instancia de su cluster
               indice <- sample(length(cluster1), 1)
               instanciaParaHacerCambio1 <- cluster1[indice]
               # Guarda una copia de seguridad del centro del cluster 1
               antiguoCentroCluster1 <- centros[1, ]
               #print(centros)
               # Actualiza los centros
               centros[1, 1] <- petalMatrix[instanciaParaHacerCambio1, 1]
               centros[1, 2] <- petalMatrix[instanciaParaHacerCambio1, 2]
               # Recalcula la distribucion
               calcularDistribucion()
               # Si mejoramos el error absoluto, reemplazamos los centros
               if(errorAbsolutoTotal < z){
                 print("Error 1111111 mejorado!")
                 #print(centros)
                 mm <- 0
               }
               # Si no mejoramos el error absoluto, volvemos hacia atrás
               else{
                 print("Error 1111111 NO Mejorado!")
                 centros[1, 1] <- antiguoCentroCluster1[1]
                 centros[1, 2] <- antiguoCentroCluster1[2]
                 mm = mm + 1
               }
             }, 
             {
               # Selecciona una instancia de su cluster
               indice <- sample(length(cluster2), 1)
               instanciaParaHacerCambio2 <- cluster2[indice]
               # Guarda una copia de seguridad del centro del cluster 2
               antiguoCentroCluster2 <- centros[2, ]
               #print(centros)
               # Actualiza los centros
               centros[2, 1] <- petalMatrix[instanciaParaHacerCambio2, 1]
               centros[2, 2] <- petalMatrix[instanciaParaHacerCambio2, 2]
               # Recalcula la distribucion
               calcularDistribucion()
               # Si mejoramos el error absoluto, reemplazamos los centros
               if(errorAbsolutoTotal < z){
                 print("Error 2222222 mejorado!")
                 #print(centros)
                 mm <- 0
               }
               # Si no mejoramos el error absoluto, volvemos hacia atrás
               else{
                 print("Error 2222222 NO Mejorado!")
                 centros[2, 1] <- antiguoCentroCluster2[1]
                 centros[2, 2] <- antiguoCentroCluster2[2]
                 mm = mm + 1
               }
             },
             {
               # Selecciona una instancia de su cluster
               indice <- sample(length(cluster3), 1)
               instanciaParaHacerCambio3 <- cluster3[indice]
               # Guarda una copia de seguridad del centro del cluster 1
               antiguoCentroCluster3 <- centros[3, ]
               #print(centros)
               # Actualiza los centros
               centros[3, 1] <- petalMatrix[instanciaParaHacerCambio3, 1]
               centros[3, 2] <- petalMatrix[instanciaParaHacerCambio3, 2]
               # Recalcula la distribucion
               calcularDistribucion()
               # Si mejoramos el error absoluto, reemplazamos los centros
               if(errorAbsolutoTotal < z){
                 print("Error 3333333 mejorado!")
                 #print(centros)
                 mm <- 0
               }
               # Si no mejoramos el error absoluto, volvemos hacia atrás
               else{
                 print("Error 3333333 NO Mejorado!")
                 centros[3, 1] <- antiguoCentroCluster3[1]
                 centros[3, 2] <- antiguoCentroCluster3[2]
                 mm = mm + 1
               }
             },
             {
               # Selecciona una instancia de su cluster
               indice <- sample(length(cluster4), 1)
               instanciaParaHacerCambio4 <- cluster4[indice]
               # Guarda una copia de seguridad del centro del cluster 1
               antiguoCentroCluster4 <- centros[4, ]
               #print(centros)
               # Actualiza los centros
               centros[4, 1] <- petalMatrix[instanciaParaHacerCambio4, 1]
               centros[4, 2] <- petalMatrix[instanciaParaHacerCambio4, 2]
               # Recalcula la distribucion
               calcularDistribucion()
               # Si mejoramos el error absoluto, reemplazamos los centros
               if(errorAbsolutoTotal < z){
                 print("Error 4444444 mejorado!")
                 #print(centros)
                 mm <- 0
               }
               # Si no mejoramos el error absoluto, volvemos hacia atrás
               else{
                 print("Error 4444444 NO Mejorado!")
                 centros[4, 1] <- antiguoCentroCluster4[1]
                 centros[4, 2] <- antiguoCentroCluster4[2]
                 mm = mm + 1
               }
             },
             {
               # Selecciona una instancia de su cluster
               indice <- sample(length(cluster5), 1)
               instanciaParaHacerCambio5 <- cluster5[indice]
               # Guarda una copia de seguridad del centro del cluster 1
               antiguoCentroCluster5 <- centros[5, ]
               #print(centros)
               # Actualiza los centros
               centros[5, 1] <- petalMatrix[instanciaParaHacerCambio5, 1]
               centros[5, 2] <- petalMatrix[instanciaParaHacerCambio5, 2]
               # Recalcula la distribucion
               calcularDistribucion()
               # Si mejoramos el error absoluto, reemplazamos los centros
               if(errorAbsolutoTotal < z){
                 print("Error 5555555 mejorado!")
                 #print(centros)
                 mm <- 0
               }
               # Si no mejoramos el error absoluto, volvemos hacia atrás
               else{
                 print("Error 5555555 NO Mejorado!")
                 centros[5, 1] <- antiguoCentroCluster5[1]
                 centros[5, 2] <- antiguoCentroCluster5[2]
                 mm = mm + 1
               }
             },
             {
               print("Tiri!")
               print("Cluster no exists!")
             }
      )
    }
    # Actualizamos el contador de iteraciones
    ll <- ll + 1
  }
  
  condicion <- TRUE
  numero <- 3
  repeat{
    print("Introduce un numero")
    if(numero > 3){
      print("Numero NO valido. Intentalo de nuevo")
      condicion <- TRUE
    }
    if(numero <= 3){
      print("Numero Valido")
      break;
    }
  }
  
  plotCluster1 <- crearMatrizConDatos(cluster1)
  plot(plotCluster1)
  
  plotCluster2 <- crearMatrizConDatos(cluster2)
  plot(plotCluster2)
  
  plotCluster3 <- crearMatrizConDatos(cluster3)
  plot(plotCluster3)
  
  plotCluster4 <- crearMatrizConDatos(cluster4)
  plot(plotCluster4)
  
  plotCluster5 <- crearMatrizConDatos(cluster5)
  plot(plotCluster5)
  
  # Escribe los datos de los clusters reales a un CSV
  write.csv(plotCluster1, file = "clusterPetal1.csv")
  write.csv(plotCluster2, file = "cluster2Petal.csv")
  write.csv(plotCluster3, file = "cluster3Petal.csv")
  write.csv(plotCluster4, file = "cluster4Petal.csv")
  write.csv(plotCluster5, file = "cluster5Petal.csv")
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

# Elige un número de instancias aleatorias (guarda el índice de la fila)
k = 5
indicesAleatorios = c();
for(i in 1: k){
  indicesAleatorios <- c(indicesAleatorios, sample(1:150, 1))
}

# Esta matriz contiene las instancias que van a ser los centros de los clusters
centros = matrix(nrow = k, ncol = 2)

# Guarda las instancias cuyos índices han sido elegidos
for(i in 1:k){
  for(j in 1:2){
    centros[i, j] <- petalMatrix[indicesAleatorios[i], j]
  }
}

clarans(petalMatrix, 5, 10 ,5)
