
prueba.distancia <- function(x, n = 3, a = 0.3, b = 0.7, alpha = 0.05){
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
  Fei <- pi*sum(Foi)
  
  # Validar si la frecuencia esperada es menor a 5.
  l <- 1
  while(l <= length(Fei)){
    if(Fei[l] < 5){
      pi[l + 1] <- pi[l] + pi[l + 1]
      Foi[l + 1] <- Foi[l] + Foi[l + 1]
      Fei[l + 1] <- Fei[l] + Fei[l + 1]
      
      pi <- pi[-l]
      Foi <- Foi[-l]
      Fei <- Fei[-l]
      xi <- xi[-l]
      
      l <- l
      if(length(Fei) == l){
        break
      }
      
    }else{
      l <- l + 1
    }
  }
  
  # Perzonalizamos para presentar.
  if(length(xi) == n){
    xi <- c(0:(n-1), paste0(">=", n), "Totales")
  }else{
    xi2 <- c(paste0("0-", xi[1]))
    for(r in 2:length(xi)){
      if(r == length(xi)){
        xi <- c(xi2, paste0(">=", (xi[1] + 1)), "Totales")
      }else{
        xi2 <- c(xi2, paste0((xi[r -1 ] + 1), "-", xi[r]))
      }
    }
  }
  
  X2c <- ((Foi - Fei)**2)/Fei
  X2c <- c(X2c, sum(X2c))
  
  Fei <- c(Fei, sum(Fei))
  
  Foi <- c(Foi, sum(Foi))
  
  pi <- c(pi, sum(pi))
  
  temp <- data.frame(xi, pi, Foi, Fei, X2c)
  
  return(list(tbl = knitr::kable(x = temp, digits = 3, align = 'c'),
              rpta = ifelse(test = sum(X2c) < qchisq(p = alpha, df = 3, lower.tail = F),
                            "Los números generados son independientes",
                            "Los números generados no son independientes")))
  
}

prueba.distancia(x = datos.1)

