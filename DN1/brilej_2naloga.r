#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)
library(Metrics)
library(glmnet)
library(dplyr)
library(psych)
library(matlib)

options(digits = 16)

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

naloga_problem = 2
source(sprintf("funkcije%d.R", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################

source("funkcije2.R"); seme = 288; podatki = naloziPodatke(seme);




# 2.1

model <- train(y ~ ., data=podatki, method="lm", tuneGrid = expand.grid(intercept=FALSE)) # tuneGrid uporabimo za zanemarjanje prostega èlena
napovedi <- predict(model, newdata = podatki)


# Napako raèunamo s pomoèjo funkcije rmse iz paketa Metrics
em <- rmse(podatki$y, napovedi)
#em <- model$results$RMSE






# 2.2

# Spomnimo se 1. naloge pri 2. vajah, lahko pa direktno upoštevamo zgolj koeficiente
koef <- model$finalModel$coefficients

# Definiramo funkcijo za iskanje k najpomembnejših koeficientov
najboljsih_k <- function(koef, k) {
  while (length(koef) > k) {
    koef <- koef[koef != min(koef)]
  }
  return(koef)
}


# Išèemo k0 -> najprej ponovimo definicije funkcij 1. naloge 2. vaj

narediFormulo <- function(imena){
  as.formula(paste("y ~", paste(imena, collapse = "+")))
}


k0 <- 1
ek <- 100*em
while (ek > 1.1*em) {
  izbrani <- names(najboljsih_k(koef, k0))
  formula <- narediFormulo(izbrani)
  nov_model <- train(formula, data=podatki, method="lm", tuneGrid = expand.grid(intercept=FALSE))
  #ek <- nov_model$results$RMSE
  ek <- rmse(podatki$y, predict(nov_model, newdata = podatki))
  k0 <- k0 + 1
}

k0 <- k0 - 1 # Ker smo na zadnjem koraku k0 za 1 poveèali brez potrebe
ek0 <- ek










# 2.3

podatki2 <- podatki
for (i in 21:40) {
  podatki2[paste0("x", as.character(i))] <- podatki2[i-20]^2
}

model2 <- train(y ~ .-1, data=podatki2, method="lm", tuneGrid = expand.grid(intercept=FALSE))
napovedi2 <- predict(model2, newdata = podatki2)

napaka2 <- rmse(podatki2$y, napovedi2)
#napaka2 <- model2$results$RMSE











# 2.4 Regularizacija

# Prvi naèin: direktno po najdeni formuli

lambda <- 0.01
model.lm <- train(y ~ ., data = podatki, method = "lm", tuneGrid  = expand.grid(intercept = FALSE))
bete <- model.lm$finalModel$coefficients
X <- podatki[, 1:20]
p <- ncol(X)
X <- as.matrix(X)
X_t <- t(X)
produkt <- X_t %*% X
rezultat <- inv(diag(p)  + lambda * inv(produkt)) %*% bete
alpha2 <- sum(rezultat^2) 
alpha2




# Drugi naèin: po navodilih naloge

norma <- function(lambda){
  y <- as.matrix(podatki2 %>% select(y))
  id <- sqrt(lambda) * diag(p)
  X.REG <- rbind(X,id)
  y.REG <- rbind(y,matrix(0, p, 1))
  pod <- cbind(X.REG,y.REG)
  
  model.REG <- train(y ~ ., data = pod, method = "lm", tuneGrid  = expand.grid(intercept = FALSE))
  bete.REG <- model.REG$finalModel$coefficients
  alpha <- sum(bete.REG^2)  
  return(alpha)
}

norma(lambda)














# 2.5
alpha <- norma(0.01)
alpha_10 <- alpha/10
alpha_10

# izberemo interval za lambda_0, na katerem bomo izvajali bisekcijo (ta interval skrajšamo za pospešitev metode)
min1 <- 1
min2 <- 1
while (min1 >= min2) {
  if (norma(min1) <  alpha_10) {
    min2 <- min1
    min1 <- min1 / 10
  } else {
    min1 <- min1 * 10
  }
}

min <- min1
max <- min2 # torej lambda bomo iskali med min in max
vrednost <- alpha # neka zaèetna vrednost, pomembno samo da je veèja kot konèna ocena

# bisekcija
tol <- 1e-10
while(abs(vrednost - alpha_10) > tol) {
  c <- (min + max)/2
  vrednost <- norma(c)
  if(vrednost < alpha_10) {
    max <- c
  } else {
    min <- c
  }
}

lambda_0 <- c
lambda_0

















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
