# 2 Linearna regresija

#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)
library(mltools)
library(matlib)

# nalozi jih tukaj, ne po klicu RNGkind spodaj
# load them here and not after the call of the RNGkind method below


#########################################################################
# Ignoriraj opozorilo (ignore the warning)
# RNGkind(sample.kind = "Rounding") : non-uniform 'Rounding' sampler used
#########################################################################
RNGkind(sample.kind = "Rounding")

#####################################
# Nekaj testov: ne spreminjaj
# Some tests: do not change
#####################################

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

test_runif()
test_sample()


#####################################
# Nalozi se potrebne funkcije
# Load the necessary functions
#####################################
setwd("C:/Users/Tina/Documents/FAKS/magisterij/machine_learning/DN1")

naloga_problem = 2
source(sprintf("funkcije%d.R", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################

seme = 288
podatki = naloziPodatke(seme)

# 2.1 Osnovna naloga

ucna = podatki
testna = podatki

model0 = train(y ~ ., data=ucna, method="lm", tuneGrid = expand.grid(intercept= FALSE))
napovedi = predict(model0, testna)
e.m <- rmse(napovedi, testna$y)
e.m

# 2.2 Koristne znacilke
koef = model0$finalModel$coefficients
sort.koef <- sort(abs(koef), decreasing = TRUE)
sort.koef
znacilke <- names(sort.koef)
znacilke

narediFormulo = function(imena){
  as.formula(paste("y ~", paste(imena, collapse = "+")))
}

e.k = 1000000
for (k in 1:length(znacilke)){
  formula = narediFormulo(znacilke[1:k])
  print(formula)
  model.k = train(formula, data=ucna, method="lm", tuneGrid = expand.grid(intercept= FALSE))
  napovedi.k = predict(model.k, testna)
  e.k = rmse(testna$y, napovedi.k)
  print(k)
  print(e.k)
  if (e.k <= 1.1*e.m) {
    break
  }
}
e.k0 <- e.k
e.k0

# 2.3 Dodatne znacilke
znacilke3 = names(podatki[, 1:20])
formula3 = "y ~ "
for (i in znacilke3){
  if (i == znacilke3[length(znacilke3)]){
    formula3 = paste(formula3, i, "+ I(", i, "^2)")
  }
  else {formula3 = paste(formula3, i, "+ I(", i, "^2) +")}
}
formula3

model3 = train(as.formula(formula3), data=ucna, method="lm", tuneGrid = expand.grid(intercept= FALSE))
napovedi3 = predict(model3, testna)
e.m3 <- rmse(napovedi3, testna$y)
e.m3


# 2.4 Regularizacija
# GLEJ ZVEZEK

beta.optimal = function(lambda, podatki){
  p <- ncol(podatki) - 1
  X <- podatki[,1:p]
  identiteta = sqrt(lambda) * diag(p)
  colnames(identiteta) <- colnames(X)
  X.vijuga = as.matrix(rbind(X, identiteta))
  
  y <- podatki$y
  nicle <- c(rep(0,p))
  y.vijuga <- c(y, nicle)
  
  beta.zvezdica = solve(t(X.vijuga) %*% X.vijuga) %*% t(X.vijuga) %*% y.vijuga
  return(sum(beta.zvezdica^2))
}

lambda <- 0.01
alpha <- beta.optimal(lambda, podatki)
alpha

# 2.5 Kako velik lambda?
alpha_10 <- alpha/10
alpha_10

# izberemo interval za lambda_0, na katerem bomo izvajali bisekcijo (ta interval skrajsamo za pospesitev metode)
min1 <- 1
min2 <- 1
while (min1 >= min2) {
  if (beta.optimal(min1, podatki) <  alpha_10) {
    min2 <- min1
    min1 <- min1 / 10
  } else {
    min1 <- min1 * 10
  }
}

min <- min1
max <- min2 # torej lambda bomo iskali med min in max
vrednost <- alpha # neka za?etna vrednost, pomembno samo da je ve?ja kot kon?na ocena

# bisekcija
tol <- 1e-7
while(abs(vrednost - alpha_10) > tol) {
  c <- (min + max)/2
  vrednost <- beta.optimal(c, podatki)
  if(vrednost < alpha_10) {
    max <- c
  } else {
    min <- c
  }
}

lambda_0 <- c
lambda_0