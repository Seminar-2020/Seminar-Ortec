# Shapley kernel k(M,S) weights
# 
# Input:
# M       total number of features
# S       number of features in the subset
# 
# Output:
# A numerical value 
# k       shapley kernel k(M,S) weight

kernel.weights <- function (M, S) {
  comb <- factorial(M)/(factorial(S)*factorial(M-S))
  k <- (M-1)/(comb*S*(M-S))
  k[!is.finite(k)] <- 10^6
  return(k)
}

# Binary matrix Z representing all possible combinations
# of inclusion/exclusion of M features
#
# Input:
# M       total number of features
#
# Output: 
# Z       a binary matrix containing all possible combinations of 
# inclusion/exclusion of the M features

matrix.Z <- function(M) {
  binary.list <- rep(list(0:1), M)
  z <- expand.grid(binary.list)
  Ones <- rep(1,nrow(z))
  Z <- cbind(Ones,z)
  return(as.matrix(Z))
}

# A diagonal matrix W containing the kernel weights k(M,S)
#
# Input: 
# M       total number of features
# 
# Output: 
# W       a diagonal matrix W containing the kernel weights k(M,S)

matrix.W <- function(M) {
  Z <- matrix.Z(M)
  z <- Z[,-1]
  subset <- apply(z,1,sum)
  w <- sapply(subset, kernel.weights, M=M)
  W <- diag(w)
}

# Calculate the Shapley values phi
#
# Input:
# W         a diagonal matrix containing the kernel weights k(M,S)
# Z         a binary matrix containing all possible combinations of features
# v         vector of contributions v(S)
#
# Output: 
# phi       a vector containing the shapley values

shapley.values <- function(W,Z,v) {
  phi <- solve(t(Z)%*%W%*%Z)%*%(t(Z)%*%W%*%v)
  rownames(phi) <- c()
  colnames(phi) <- c("Phi")
  return(phi)
} 


# Function to compute the contribution function v(S)
#
# Input:
#
# M           number of input features
# x.star      instance of interest (not from training data)
# X           data frame of explanatory variables, the training data set used to fit a model
# K           number of samples from the training data
# fit.model   fitted model such as lm(y ~ X)
#
# Output:
#
# v         vector of contributions v(S) for every subset

func <- function (M, x.star, X, K, fit.model) {
  Z <- matrix.Z(M)
  z <- Z[,-1] 
  x.star.list <- replicate(nrow(z),x.star)
  x.star.matrix <- matrix(unlist(x.star.list), ncol = M, byrow = TRUE)
  x.star.S <- z*x.star.matrix #x*_S for all possible subsets S
  
  prediction <- replicate(K, {
    x.random.df <- X[sample(nrow(X),size=2^M,replace=FALSE),] #sample random instance for every subset S from original X data
    x.random.matrix <- as.matrix(x.random.df)
    x.new <- matrix(NA, nrow = nrow(x.random.matrix), ncol = M) #initialise new matrix
    
    #combine x*_S with x.random
    for(i in 1:nrow(x.star.S)){
      for (j in 1:M) {
        x.new[i,j] <- replace(x.star.S[i,j], x.star.S[i,j] ==0, x.random.matrix[i,j])
      }
    }
    x.new.df <- as.data.frame(x.new)
    colnames(x.new.df) <- names(X) #even checken of dit nu goed werkt met die input X
    f.hat <- predict(fit.model, newdata = x.new.df)
    f.hat
  }
  )
  v <- as.matrix(rowMeans(prediction))
}
