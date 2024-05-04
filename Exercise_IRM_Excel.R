################################################################################
########################## Ejercicio sobre MRI #################################
################################################################################

# Halla la distribución de S = X1 + X2 + X3 + X4

# P(X = j)  |   0.2    0.5    0.2    0.1
#     j     |    0      1      2      3



# (i) Convolución Tradicional

Convolution <- function(n,vec1,vecprob1){
  n = n - 1
  sup <- vec1
  sup1 <- vec1
  pmf <- vecprob1
  pmf1 <- vecprob1
  for (i in 1:n){
    c1 <- c()
    for (i in sup1){
      x = i
      for (i in sup){
        y = i
        z = x + y
        c1 <- c(c1, z)
      }
    }
    
    pmfs <- c()
    for (i in 1:length(pmf1)){
      x = pmf1[i]
      for (i in 1:length(pmf)){
        y = pmf[i]
        z = x * y
        pmfs <- c(pmfs, z)
      }
    }
    
    c1_unique <- unique(c1)
    c1_unique
    
    AcumSum <- c()
    for (i in 1:length(c1_unique)){
      AcumSum1 <- c()
      AcumSum2 <- c()
      for (j in 1:length(c1)){
        if (c1_unique[i] == c1[j]){
          x = pmfs[j]
          AcumSum1 <- c(AcumSum1, x)
        }
      }
      AcumSum2 <- c(AcumSum2, sum(AcumSum1))
      AcumSum <- c(AcumSum, AcumSum2)
    }
    
    sup1 <- c1_unique
    pmf1 <- AcumSum
  }
  
  return(list(c1_unique,AcumSum))
}

# Distribución de S

Convolution(n = 4, vec1 = c(0,1,2,3), vecprob1 = c(0.2,0.5,0.2,0.1))

# Comprobación Suma 1

sum(Convolution(n = 4, vec1 = c(0,1,2,3), vecprob1 = c(0.2,0.5,0.2,0.1))[[2]])




# (ii) Fórmula de DePril versión 2

DePril <- function(n,vec1,prob1){
  n = n - 1
  sup <- vec1
  sup1 <- vec1
  for (i in 1:n){
    c1 <- c()
    for (i in sup1){
      x = i
      for (i in sup){
        y = i
        z = x + y
        c1 <- c(c1, z)
      }
    }
    
    c1_unique <- unique(c1)
    c1_unique
    
    sup1 <- c1_unique
  }
  
  n = n + 1
  
  deprilvec <- c()
  
  for (i in 1:length(c1_unique)){
    if (c1_unique[i] == 0){
      g0 = (prob1[i])^n
      deprilvec <- c(deprilvec,g0)
    } else {
      gx <- 0
      for (j in 1:c1_unique[i]){
        if (is.na(prob1[j+1]) == T){
          x = 0
        } else {
          x = prob1[j+1]
        }
        if (is.na(deprilvec[i-j]) == T){
          y = 0
        } else {
          y = deprilvec[i-j]
        }
        gx <- gx + (j*(n+1)/c1_unique[i] - 1)*x*y
      }
      gx <- gx*(1/prob1[1])
      deprilvec <- c(deprilvec,gx)
    }
  }
  
  return(list(c1_unique,deprilvec))
}

# Distribución de S

DePril(n = 4, vec1 = c(0,1,2,3), prob1 = c(0.2,0.5,0.2,0.1))

# Comprobación Suma 1

sum(DePril(n = 4, vec1 = c(0,1,2,3), prob1 = c(0.2,0.5,0.2,0.1))[[2]])


# Verificación que la distribución de S sea igual

Convolution(n = 4, vec1 = c(0,1,2,3), vecprob1 = c(0.2,0.5,0.2,0.1))

DePril(n = 4, vec1 = c(0,1,2,3), prob1 = c(0.2,0.5,0.2,0.1))