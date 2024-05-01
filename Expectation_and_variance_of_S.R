M <- t(matrix(c(1,3,1,
              3,5,4,
              5,3,4,
              2,2,6,
              2,3,4), nrow = 3))
M

# Dimension of the matrix
dim(M)

# Rows of the matrix
dim(M)[1]

# Columns of the matrix
dim(M)[2]

# Sumproduct of the row of the matrix with the probabilities
sum(M[1,]*c(0.03,0.04,0.05))

for (i in 1:dim(M)[1]){
  print(i)
}


################################################################################

# Probability of claim
prob_qj <- c(0.03,0.04,0.05)

# Probability of not-claim
prob_1mqj <- 1-prob_qj

# Number of insurers with i claim and qj probability
Mnij <- t(matrix(c(1,3,1,
                3,5,4,
                5,3,4,
                2,2,6,
                2,3,4), nrow = 3))

# Amount of claim
I <- c()
for (i in 1:dim(Mnij)[1]){
  I <- c(I,i)
}
I[1]

# E[S]
ExpS <- c()
for (i in 1:dim(Mnij)[1]){
  x = I[i]*sum(Mnij[i,]*prob_qj)
  ExpS <- c(ExpS, x)
}

ExpS
sum(ExpS)

# Var[S]
VarS <- c()
for (i in 1:dim(Mnij)[1]){
  x = (I[i]^2)*sum(Mnij[i,]*(prob_qj*prob_1mqj))
  VarS <- c(VarS, x)
}

VarS
sum(VarS)