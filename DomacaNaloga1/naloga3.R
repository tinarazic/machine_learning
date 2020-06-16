########################################################################################
# Domača naloga 1
# # 3 Variance in se kaj
########################################################################################

#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)
library(matlib)
library(mltools)

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
#setwd("D:/Dokumenti/FAKS/magisterij/machine_learning/DomacaNaloga1")
setwd(paste(getwd(),"/DomacaNaloga1", sep=""))

naloga_problem = 3
source(sprintf("funkcije%d.R", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################
seme = 288
podatki = naloziPodatke(seme)

# 3.1 Kdo se najbolj prilega?

narediPolinom = function(stopnja){
  formula = "y ~ 1 + I(x)"
  if (stopnja >= 2){
    for (i in 2:stopnja){
      formula = paste(formula, "+ I(x**", i, ")")
    }
  }
  return(formula)
}

polinomsko.prileganje = function(stopnja, ucna, testna){
  # vrne rmse za dano stopnjo polinoma
  formula = as.formula(narediPolinom(stopnja))
  model.s = train(formula, data=ucna, method="lm")
  napovedi.s = predict(model.s, testna)
  napaka.s <- rmse(napovedi.s, testna$y)
  return(napaka.s)
}
  
  
e.s <- vector()
max.stopnja = 10 
for (stopnja in 1:max.stopnja){
    napaka.s = polinomsko.prileganje(stopnja, podatki, podatki)
    e.s <- c(e.s, napaka.s)
}
stopnja_3.1 <- which.min(e.s)
rmse <- min(e.s)
rmse


# 3.2 Precno preverjanje prvic

razbij = function(podatki, k){
  P = vector()
  n = length(podatki[,1])
  for (j in 1:k){
    Pj = vector()
    for (q in ceiling((1-j)/k):floor((n-j)/k)){
      Pj = c(Pj, q*k + j)
    }
    print(Pj)
    P = cbind(P, Pj)
  }
  return(P)
}

razbitje = razbij(podatki,4)
P1 <- razbitje[,1]
izbrani.podatki <- podatki[P1, c("x")]
povprecje <- mean(izbrani.podatki)
povprecje

# 3.3 Precno preverjanje drugic

k = 4
razbitje = razbij(podatki, k)
e.s.all = vector()
for (s in 1:10){
  e.sj.all = vector()
  for (j in 1:k){
    Pj = razbitje[,j]
    T.j = podatki[Pj, ]
    U.j = podatki[-Pj, ]
    index <- as.numeric(row.names(U.j))
    U.j = U.j[order(index), ]
    polinomska.napaka.e.s.j = polinomsko.prileganje(s, U.j, T.j)
    e.sj.all = c(e.sj.all, polinomska.napaka.e.s.j)
  }
  e.s.mean = mean(e.sj.all)
  e.s.all = c(e.s.all, e.s.mean)
}
stopnja_ps1 <- which.min(e.s.all)
e.s1 <- min(e.s.all)


# Varianca p_i0

varianca = function(varianca.epsilon, matrikaX, X_zvezdica){
  matrikaX_t <- t(matrikaX)
  produkt <- matrikaX_t %*% matrikaX
  inverz <- solve(produkt)
  varianca = varianca.epsilon * (X_zvezdica %*% inverz %*% t(X_zvezdica))
  return(varianca)
}

# 3.4 Varianca ps0

sigma.epsilon_ps0 <- 1.0^2
stopnja_ps0 <- which.min(e.s)
x <- podatki$x
prosti.clen = rep(1,dim(podatki)[1])
matrikaX <- cbind(prosti.clen, x)
for (i in 2:stopnja_ps0){
  matrikaX = cbind(matrikaX, x**i)
}
rows = stopnja_ps0 + 1
X_zvezdica <-  matrix(1,1,rows)

varianca_ps0 <- varianca(sigma.epsilon_ps0, matrikaX, X_zvezdica)
varianca_ps0

# 3.5 Varianca ps1

sigma.epsilon_ps1 <- 1.0^2
stopnja_ps1 <- which.min(e.s.all)
x <- podatki$x
prosti.clen.1 = rep(1,dim(podatki)[1])
matrikaX.1 <- cbind(prosti.clen.1, x)
for (i in 2:stopnja_ps1){
  matrikaX.1 = cbind(matrikaX.1, x**i)
}
rows.1 = stopnja_ps1 + 1
X_zvezdica.1 <-  matrix(1,1,rows.1)

varianca_ps1 <- varianca(sigma.epsilon_ps1, matrikaX.1, X_zvezdica.1)
varianca_ps1




