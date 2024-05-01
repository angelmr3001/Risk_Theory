# Esta función calcula la distribución de la suma de v.a.'s discretas i.i.d., 
# i.e., con la misma fmp

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

# Ejemplo visto en clase
vec1 <- c(0,1,2)
vecprob1 <- c(0.50, 0.30, 0.20)
Convolution(3, vec1 = vec1, vecprob1 = vecprob1 )

# Ejemplo de la tarea 3  MRI
vec1 <- c(1,5)
vecprob1 <- c(0.75, 0.25)
Convolution(5, vec1 = vec1, vecprob1 = vecprob1 )