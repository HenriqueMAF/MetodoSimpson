
resultados <- list()

for(w in 0:1){
  a <- x[1]
  b <- x[length(x)]
  h <- (b-a)/n
  
  tam <- length(x)
  valorx <- c()
  soma <- a
  for (i in 1:tam){
    valorx <- c(valorx,soma)
    soma <- soma + h
  } #end for valores X
  
  if (w == 0){
    y0 <- c()
    for (i in valorx){
      y <-  (-8.3414*(i^2))+(64.638*i)+6190.9
      y0 <- c(y0,y)
    } #end for valores de Y
  }
  else{
    y0 <- c()
    for (i in valorx){
      y <-  (-58.583*(i^2))+(794.7*i)+3471.9
      y0 <- c(y0,y)
    } #end for valores de Y
  }
  
  length(y0)
  integral<-c()
  for (i in 0:length(y0)-1){
    if(i == 0){
      integral <- c(integral,(y0[i+1])*((3/8)*h))
    }
    else if (i %% 3 == 0){
      integral <- c(integral, (2*y0[i+1])*((3/8)*h))
    }
    else if(i == 12){
      integral <- c(integral, (y0[i+1])*((3/8)*h))
    }
    else{
      integral <- c(integral, (3*y0[i+1])*((3/8)*h))
    }
  } #end integral
  #integral[length(integral)] <- F
  Som.integral <- sum(integral)
  resul <- list(valorx,y0,Som.integral)
  resultados <- c(resultados,resul)
} #end for