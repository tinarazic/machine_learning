########################################################################################
# Domača naloga 1
# 1 Najbližji sosedje
########################################################################################

#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)
library(readr)
library(pROC)
library(caret)
library(dplyr)

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

naloga_problem = 1
source(sprintf("funkcije%d.R", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################

options(digits = 16)
seme = 288
podatki = naloziPodatke(seme)

podatki1 <- podatki

# 1.1 Pravilno pozitivni
podatki1$tp[podatki1$z >= 0.6] = 1
podatki1$tp[podatki1$z < 0.6] = 0

true.positive = sum(podatki1$y == podatki1$tp & podatki1$y == 1)
true.positive

TPR = true.positive/sum(podatki1$y==1)
TPR

# 1.2 Ploščina pod krivuljo
roc(podatki1$y, podatki1$z, auc = TRUE, plot = TRUE)
# Area under the curve: 0.9433


# 1.3 Pomembna značilka
po_0 <- podatki1 %>% filter(y == 0)
po_1 <- podatki1 %>% filter(y == 1)

max(po_0$x1)
min(po_1$x1)

max(po_0$x2)
min(po_1$x2)

max(po_0$x3)
min(po_1$x3)

# Pri x2 se ne prekrivajo. -> max interval
dolzina.intervala = min(po_1$x2)- max(po_0$x2)
dolzina.intervala

# 1.4 Predelaj podatke
for (i in 1:length(podatki1$y)){
  podatki1$y2[i] = paste(podatki1$x6[i], podatki1$y[i], sep = "")
}

# Novi razredi a0, a1, b0, b1.
length(podatki1[podatki1$y2=="a0", 10])
length(podatki1[podatki1$y2=="a1", 10])
length(podatki1[podatki1$y2=="b0", 10])
length(podatki1[podatki1$y2=="b1", 10])

# 1.5 Končni model

podatki.model <- cbind(podatki1[,1:5], podatki1[,10])
colnames(podatki.model) <- c("x1", "x2", "x3", "x4", "x5", "y")

indeks_ucna = createDataPartition(podatki.model$y, p=3/4, list=FALSE)
ucna = podatki.model[1:600,]  # list=FALSE, da se tukaj pri indeksiranju ne sesuje
testna = podatki.model[601:800,] 
natrenirano = train(y ~ ., data=ucna, method="knn", tuneGrid = data.frame(k = 1))
napovedi = predictKviz(natrenirano, testna, seme)

mikro.priklic <- function(razredi, y, z){
  n = length(y)
  p = 0 # stevilo pozitivnih primerov
  tp = 0 # stevilo pravilno pozitivnih primerov
  for (a in razredi){
    p = p + sum(y == a)
    tp = tp + sum(y == z & z == a)
  }
  p = n
  return(tp/p)
}

mikro.priklic.model <- mikro.priklic(c("a0", "a1", "b0","b1"), testna$y, napovedi)
mikro.priklic.model


# a) MIKRO NATANČNOST definiramo kot (TP1+TP2)/(TP1+TP2+FP1+FP2)

mikro.natančnost <- function(razredi, y, z){
  n = length(y)
  fp = 0 # stevilo napačno pozitivnih primerov
  tp = 0 # stevilo pravilno pozitivnih primerov
  for (a in razredi){
    tp = tp + sum(y == z & z == a)
    fp = fp + sum(y != z & y == a)
  }
  p = n
  return(tp/(tp + fp))
}

# b) MAKRO PRIKLIC definiramo kot (R1 + R2)/2,
#   kjer je R1 = TP1/(TP1+FN1)

makro.priklic <- function(razredi, y, z){
  n = length(y)
  priklic = vector()
  p = 0 # stevilo pozitivnih primerov
  tp = 0 # stevilo pravilno pozitivnih primerov
  for (a in razredi){
    p = p + sum(y == a)
    tp = tp + sum(y == z & z == a)
    priklic = c(priklic, tp/p)
  }
  return(mean(priklic))
}







