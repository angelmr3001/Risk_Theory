# Este programa genera la distribución de probabilidad de la v.a. Sn donde
# Sn = X1 + ... + Xn utilizando la fórmula de DePril[ii] de la página
# 17 del pdf del libro "Introducción a la Teoría del Riesgo de Luis Rincón

# Limitantes: Esta función sólo arroja resultados coherentes para
# v.a.'s i.i.d., i.e., con la misma fmp

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


# Ejemplo de la página 19

DePril(n = 3,vec1 = c(0,1,2),prob1 = c(0.5,0.2,0.3))