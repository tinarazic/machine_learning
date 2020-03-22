# 1. NAJBLIŽJI SOSEDI

library(readr)
library(pROC)
library(caret)
library(dplyr)

options(digits = 16)
source("funkcije1.R")
seme = 601
podatki = naloziPodatke(seme)

podatki1 <- podatki

# 1.1 Pravilno pozitivni
podatki1$tp[podatki1$z >= 0.6] = 1
podatki1$tp[podatki1$z < 0.6] = 0

true.positive = sum(podatki1$y == podatki1$tp & podatki1$y == 1)
true.positive