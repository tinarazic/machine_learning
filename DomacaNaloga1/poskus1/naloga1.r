# 1. NAJBLIŽJI SOSEDI

library(readr)
library(pROC)
library(caret)
library(dplyr)

options(digits = 16)
source("funkcije1.R")
seme = 602
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

indeks_ucna = createDataPartition(podatki.model$y, p=4/5, list=FALSE)
ucna = podatki.model[indeks_ucna,]  # list=FALSE, da se tukaj pri indeksiranju ne sesuje
testna = podatki.model[-indeks_ucna,] 
natrenirano = train(y ~ ., data=ucna, method="knn", tuneGrid = data.frame(k = 5))
napovedi = predictKviz(natrenirano, testna, seme)

mikro.priklic <- function(razredi, y, z){
  n = length(y)
  #p = 0 # stevilo pozitivnih primerov
  tp = 0 # stevilo pravilno pozitivnih primerov
  for (a in razredi){
    # p = p + sum(y == a)
    tp = tp + sum(y == z & z == a)
  }
  p = n
  return(tp/p)
}

priklic.model <- mikro.priklic(c("a0", "a1", "b0","b1"), podatki.model$y, napovedi)
priklic.model
# 0.492


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
