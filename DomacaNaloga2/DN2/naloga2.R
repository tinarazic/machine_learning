########################################################################################
# Domača naloga 2
# 2 Podporni vektorji
########################################################################################

# working directory
#setwd("D:/Dokumenti/FAKS/magisterij/machine_learning/DomacaNaloga2")
#setwd(paste(getwd(),"/DomacaNaloga2", sep=""))

# naložimo knjižnice
options(digits = 16)
library(ggplot2)
library(dplyr)
library(raster)

# ukazi za kviz
source("kviz2.R")
x1 = c(1, 2, 3, 4)
x2 = c(3, 1, 4, 1)
sigma = 10

# 2.1 Radialno jedro

radialnoJedro <- function(x1,x2,sigma=1){
  norma <- norm(x1-x2,type = "2")
  rezultat <- exp(-norma^2/(2*sigma^2))
  return(rezultat)
}

vrednost.kviz <- radialnoJedro(x1,x2,sigma)


# 2.2 Ločimo kroglo
# v zvezku napisano
najmanjsiSigma <- function(b){
  #vselej velja b <= exp(-1/(2*sigma^2))
  #ko je dosežena enakost je to najmanjše
  if (b < 1){
    sigma <- sqrt(-1/(2*log(b)))
  }
  else if (b > 1){
    sigma <- sqrt(-2/(log(b)))
  }
  return(sigma)
}

sigma.kviz <- najmanjsiSigma(0.5)


# 2.3 Jedrni trik
podatki23 <- read.csv("podatki23.csv")
ggplot() + geom_point(data = podatki23, aes(x1,x2, color = y))
# pozitivni primeri so znotraj krogov, negativni pa zunaj krogov

# samo podatki znotraj krogov, ti so označeni z 1
podatki.krog <- (podatki23 %>% filter(podatki23$y == 1))[,1:2]

# podatki za levi in desni krog
levi.krog <- podatki.krog %>% filter(podatki.krog$x1 < 2)
desni.krog <- podatki.krog %>% filter(podatki.krog$x1 >2)

## PODPORNA VEKTORJA

# poščemo točki , ki bosta najbližje približnemu središču kroga -> to bosta podporna vektorja
# glej definicijo funkcije radialnoJedro (zato središče iščemo)
# iz slike opazimo kaj je središče
p1 <- levi.krog[which.min(pointDistance(levi.krog,c(0,1),lonlat = F)),]
p2 <- desni.krog[which.min(pointDistance(desni.krog,c(5,0),lonlat = F)),]

# lahko pa izračunamo tudi povprečje in to nastavimo za središče,
# p1 in p2 sta enaka v tem primeru
center1 <- matrix(colMeans(levi.krog),ncol=2)
center2 <- matrix(colMeans(desni.krog),ncol=2)
p1 <- levi.krog[which.min(pointDistance(levi.krog,center1,lonlat = F)),]
p2 <- desni.krog[which.min(pointDistance(desni.krog,center2,lonlat = F)),]


## POLMER KROGA

# r1 <- 1
# r2 <- 2

# največja razdalja točke od središča bo polmer
r1 <- max(pointDistance(levi.krog,p1,lonlat = F))
r2 <- max(pointDistance(desni.krog,p2,lonlat = F))

# razdalja med središčema
d <- pointDistance(p1,p2,lonlat = F)

# risanje
# rišemo vsoto dveh radialnih jeder 
# na začetku si izberemo neki vrednosti alpha in beta
# če želimo dobiti ožjo krivuljo, potem povečamo alpha/beta
# če želimo dobiti širšo krivuljo, potem zmanjšamo alpha/beta
# če vidimo, da nam npr primere iz levega kroga, razvršča tudi med negativne, potem malo zmanjšamo alpha
alpha <- 1.6
beta <- 0.43
distribution<-function(x){exp(-alpha * (x + d/2)^2 ) + exp(-beta * (x - d/2)^2 )}
ggplot(data.frame(x=seq(-ceiling(d),ceiling(d),0.001)), aes(x=x)) + 
  stat_function(fun=distribution)+
  geom_vline(xintercept = d/2, col = "red")+
  geom_vline(xintercept = -d/2, col = "red")+
  geom_vline(xintercept = -d/2 - r1, col = "blue")+
  geom_vline(xintercept = -d/2 + r1, col = "blue")+
  geom_vline(xintercept = d/2 - r2, col = "blue")+
  geom_vline(xintercept = d/2 + r2, col = "blue")
# rdeči črti predstavljata kjer je središče kroga
# desni črti predstavljata meji kroga

# izračunamo ven sigmi
# alpha = 1/(2*sigma^2)
# beta = 1/(2*sigma^2)
sigma1 <- sqrt(1/(2*alpha))
sigma2 <- sqrt(1/(2*beta))


# vrednost funkcije v robnih točkah kroga, nekje okrog teh vrednosti bo b
distribution(-d/2 + r1)
distribution(d/2 - r2)
b1 <- 0.16

preveri.kviz1 <- preveri(p1, p2, sigma1, sigma2, b1)


