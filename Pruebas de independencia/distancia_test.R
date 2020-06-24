prueba.distancia <- function(x, n = 3, a = 0.3, b = 0.7){
  # Calculamos la probabilidad asociada a cada clase.
  pi <- c(); teta <- b - a
  for(i in 0:n){
    if(i < n){
      pi[i + 1] <- teta*((1-teta)**i)
    }
    else{
      pi[i + 1] <- 1 - sum(pi)
    }
  }
  xi <- c(0:n)
  
  # Calculamos las observaciones que caen dentro del intervalo fijado
  resp <- c(); s <- 1; I <- c()
  for(j in 1:length(x)){
    if(x[j] <= b && x[j] >= a){
      I[s] <- j
      if(length(I) > 1){
        resp[s-1] <- (I[s] - I[s-1]) - 1
      }
      s <- s + 1
    }
  }
  
  # Calculamos la frecuencia observada para cada clase
  Foi <- c()
  for(k in 1:length(xi)){
    if(k < length(xi)){
      Foi[k] <- sum(resp == xi[k])
    }
    else{
      Foi[k] <- sum(resp >= xi[k])
    }
  }
  
  # Perzonalizamos para presentar.
  Fei <- pi*sum(Foi)
  
  X2c <- ((Foi - Fei)**2)/Fei
  X2c <- c(X2c, sum(X2c))
  
  Fei <- c(Fei, sum(Fei))
  
  Foi <- c(Foi, sum(Foi))
  
  pi <- c(pi, sum(pi))
  
  xi <- c(0:(n-1),paste0(">=", n), "Totales")
  
  temp <- data.frame(xi, pi, Foi, Fei, X2c)
  
  knitr::kable(x = temp, digits = 3, align = 'c')
  
}

prueba.distancia(x = datos.1)
