#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)
library(dplyr)
library(pROC)
library(traineR)
library(matlib)

# nalozi jih tukaj, ne po klicu RNGkind spodaj
# load them here and not after the call of the RNGkind method below


#########################################################################
# Ignoriraj opozorilo (ignore the warning)
# RNGkind(sample.kind = "Rounding") : non-uniform 'Rounding' sampler used
#########################################################################
RNGkind(sample.kind = "Rounding")

#####################################
# Nekaj testov
# Some tests
#####################################
test_runif()
test_sample()


#####################################
# Nalozi se potrebne funkcije
# Load the necessary functions
#####################################
# setwd("pot do mape (path to directory)")

naloga_problem = 1
source(sprintf("funkcije%d.R", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################

source("funkcije3.R"); seme = 288; podatki3 = naloziPodatke(seme);

# 3.1
# prvi način -  sfunkcijo poly
RMSE <- c()
for (i in 1:10){
  f <- bquote(y ~ poly(x, .(i), raw = TRUE))
  model<- train(as.formula(f), data = podatki3 ,method = "lm")
  rmse <- RMSE(podatki3$y, predict(model, podatki3))
  RMSE[[i]] <- rmse
}
min(RMSE)
which.min(RMSE)

# drugi način
# podatkom dodamo različne potence
data <- podatki3
for (i in 2:10){
  potenca <- '^'(podatki3$x,i)
  #assign(paste0("x_",i),potenca)
  data <- cbind(data, potenca )
}

stolpci <- c("x","y",paste0("x_", 2:10))
colnames(data) <- stolpci

potence <- data

napake <- c()
for (i in 1:10){
  data <- potence[,1:(i+1)]
  model <- train(y ~., data = data ,method = "lm")
  rmse <- RMSE(potence$y, predict(model, potence))
  napake[[i]] <- rmse
}

min(napake)

which.min(RMSE)
s_0 <- which.min(napake)


# 3.2
razbij <-function(podatki,k){
  n <- nrow(podatki)
  P <- list()
  for (j in 1:k){
    P_j <- c()
    q <- 0
    i <- 0
    while (q * k + j <= n){
      i <- q * k + j
      q <- q + 1
      P_j <- append(P_j,i)
    }
    P[[j]] <- P_j
  }
  return(P)
}

# k = 4
P <- razbij(podatki3,4)
P1 <- P[[1]]

podatki_p1 <- podatki3[P1,]
mean(podatki_p1$x)

# 3.3

k <- 4
# razbitje na U in T
P <- razbij(podatki3,k)
U <- list()
TT <- list()

for (j in 1:k){
  T_j <- P[[j]]
  U_j <- c()
  for  (i in 1:k){
    if (i != j){
      U_j <- append(U_j , P[[i]])
    }
  }
  U_j <- sort(U_j)
  
  U[[j]] <- U_j
  TT[[j]] <- T_j
}

# RMSE
napake_s <- c()
for (s in 1:10){
  # napake <- c()
  e_s <- 0
  for (j in 1:k){
    U_j <- U[[j]]
    T_j <- TT[[j]]
    
    data <- potence[,1:(s+1)]
    model <- train(y ~., data[U_j,] ,method = "lm")
    
    e_s_j <- RMSE(data[T_j,]$y, predict(model, data[T_j,]))
    e_s <- e_s + e_s_j / k
  }
  napake_s[[s]] <- e_s
  #sum(napake)/k # e_s
}
min(napake_s)
which.min(napake_s)

# 3.4
# s0 = 10

sigma <- 1.0
X_zvezdica <- matrix(1,1,11)
X <- potence
X$y <- NULL
X <- as.matrix(X)
n <- nrow(potence)
prosti_clen <- matrix(1,n,1)
X <- cbind(prosti_clen, X)
X_t <- t(X)
produkt <- X_t %*% X
inverz <- solve(produkt)

var <- sigma^2 * (X_zvezdica %*% inverz %*% t(X_zvezdica))
var


# 3.5 
# s1 = 2

sigma <- 1.0
X_zvezdica <- matrix(1,1,3)
X <- potence
X <- X[,c("x","x_2")]
X <- as.matrix(X)
n <- nrow(potence)
prosti_clen <- matrix(1,n,1)

X <- cbind(prosti_clen, X)
X_t <- t(X)
produkt <- X_t %*% X
inverz <- solve(produkt)

var <- sigma^2 * (X_zvezdica %*% inverz %*% t(X_zvezdica))
var











###############################################
# Kode pod tem ne spreminjaj
# Do not change the code below
###############################################

test_runif = function(){
  set.seed(1234)
  x = runif(5);
  x1 = c(0.1137034113053232, 0.6222994048148394, 0.6092747328802943, 0.6233794416766614, 0.8609153835568577)
  if (sum(abs(x - x1)) > 10^-10){
    stop("Test runif ni ok (has failed)")
  }
}

test_sample = function(){
  set.seed(1234)
  x = sample(20);
  x1 = c(3, 12, 11, 18, 14, 10, 1, 4, 8, 6, 7, 5, 20, 15, 2, 9, 17, 16, 19, 13)
  if (sum(abs(x - x1)) > 0){
    stop("Test sample ni ok (has failed)")
  }
}