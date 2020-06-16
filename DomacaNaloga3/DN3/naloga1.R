########################################################################################
# Domača naloga 3
# 1 Ansambli
########################################################################################

# working directory
#setwd("D:/Dokumenti/FAKS/magisterij/machine_learning/DomacaNaloga3")
#setwd(paste(getwd(),"/DomacaNaloga3", sep=""))

# naložimo knjižnice in podatke
library(limSolve)

podatki1 <- read.csv("podatki1.csv", header = FALSE)
colnames(podatki1)[1] <- "y" 
koristnosti1 <- read.csv("koristnosti1.csv", header = FALSE)

# ukazi za kviz
options(digits = 16)
print("Srecno in good luck")


# 1.1 Kako podobna so si drevesa?
korelacija.vektorjev <- function(a,b){
  # funkcija sprejme enako dolga vektorja a in b ter zračuna njuno korelacijo
    povprecje.a <- mean(a)
    povprecje.b <- mean(b)
    stevec <- sum((a-povprecje.a)*(b-povprecje.b))
    imenovalec <- sqrt(sum((a-povprecje.a)^2))* sqrt(sum((b-povprecje.b)^2))
    izracun <- stevec/imenovalec
  return(izracun)
}

korelacija <- function(podatki){
  n <- dim(podatki)[2]
  povprecna.korelacija <- c()
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      korelacija.para <- korelacija.vektorjev(podatki[,i], podatki[,j])
      povprecna.korelacija <- c(povprecna.korelacija,korelacija.para)
    }
  }
  povprecna.korelacija <- mean(povprecna.korelacija)
  return(povprecna.korelacija)
}

# kviz 
korelacija.kviz <- korelacija(podatki1[,-1])
korelacija.kviz


# 1.2 Optimizacija uteži

optimalne.utezi <- function(podatki){
  m <- dim(podatki)[1]
  meja <- m/2
  validacija <- podatki[1:meja,]
  P <- as.matrix(validacija[, -1])
  y <- as.vector(validacija[, 1])
  # Funkcija lsei iz paketa limSolve, ki išče rešitev z najmanjšo kvadratno napako,
  utezi <- lsei(A = P, B = y, fulloutput = FALSE, verbose = FALSE)$X
  return(utezi)
}

utezi <- optimalne.utezi(podatki1)

razlika.napak <- function(podatki, utezi){
  m <- dim(podatki)[1]
  n.plus1 <- dim(podatki)[2]
  meja <- m/2
  test <- podatki[(meja +1):m,]
  velikost.test <- dim(test)[1]
  napoved.std <- matrix(0,velikost.test,1)
  napoved.opt <- matrix(0,velikost.test,1)
  pravi.y <- test$y
  for (i in 1:velikost.test){
    napoved.std[i,1] <- sum(test[i,2:n.plus1])/(n.plus1 - 1)
    napoved.opt[i,1] <- sum(utezi* test[i,2:n.plus1])
  }
  error.std <- sum((pravi.y - napoved.std)^2)/velikost.test
  error.opt <- sum((pravi.y - napoved.opt)^2)/velikost.test
  return(error.std-error.opt)
}

# kviz
razlika.kviz <- razlika.napak(podatki1,utezi)
razlika.kviz


# 1.3 Koliko dreves zadošča?

ustalitev <- function(drevesneKakovosti) {
  p <- nrow(drevesneKakovosti)
  n <- ncol(drevesneKakovosti)
  
  povprecne.koristnosti <- rowMeans(drevesneKakovosti) 
  V <- sort(povprecne.koristnosti, index.return = TRUE)$ix
  
  for (n.vijuga in (n-1):1) {
    if (n.vijuga > 1) {
      povprecne <- rowMeans(drevesneKakovosti[, 1:n.vijuga])
    } else {
      povprecne <- drevesneKakovosti[, 1:n.vijuga]
    }
    V.vijuga <- sort(povprecne, index.return = TRUE)$ix
    if (all(V.vijuga == V) == FALSE) {
      return(n.vijuga + 1)
    }
  }
  return(1)
}

ustalitev.kviz <- ustalitev(koristnosti1)
ustalitev.kviz

