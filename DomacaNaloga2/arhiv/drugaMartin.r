# 115 120

source("kviz2.R"); x1 = c(1, 2, 3, 4); x2 = c(3, 1, 4, 1); sigma = 10;
#2.1.
radialnoJedro <- function(x1,x2,sigma=1){
  norma <- norm(x1-x2,type = "2")
  rezultat <- exp(-norma^2/(2*sigma^2))
  return(rezultat)
}


#2.2.
najmanjsiSigma <- function(b){
  #vselej velja b <= exp(-1/(2*sigma^2))
  #ko je dosežena enakost je to najmanjše
  sigma <- sqrt(-1/(2*log(b)))
  return(sigma)
}


#2.3.
library(readr)
library(dplyr)
library(raster)
library(ggplot2)
podatki <- read_csv("podatki23.csv")
#dobiti moram vektorja p1,p2
#glede na ta vektorja dobimo še sigma1 sigma 2,
#nato v kviz2.R zaženemo preveri
#b je ??

podatki23 <- read.csv("podatki23.csv")
plot(podatki23[,c(1,2)])
# pozitivni primeri so znotraj krogov, negativni pa zunaj krogov

# samo podatki znotraj krogov, ti so označeni z 1
podatki_krog <- (podatki23 %>% filter(podatki23$y == 1))[,1:2]

# podatki za levi in desni krog
levi_krog <- podatki_krog %>% filter(podatki_krog$x1 < 2)
desni_krog <- podatki_krog %>% filter(podatki_krog$x1 >2)

# poščemo točki , ki bosta najbližje približnemu središču kroga -> to bosta podporna vektorja
# iz slike opazimo kaj je središče
p1 <- levi_krog[which.min(pointDistance(levi_krog,c(0,1),lonlat = F)),]
p2 <- desni_krog[which.min(pointDistance(desni_krog,c(5,0),lonlat = F)),]

# lahko pa izračunamo tudi povprečje, p1 in p2 sta enaka v tem primeru
center1 <- matrix(colMeans(levi_krog),ncol=2)
center2 <- matrix(colMeans(desni_krog),ncol=2)
p1 <- levi_krog[which.min(pointDistance(levi_krog,center1,lonlat = F)),]
p2 <- desni_krog[which.min(pointDistance(desni_krog,center2,lonlat = F)),]


# izračun polmera kroga

# r1 <- 1
# r2 <- 2

# največja razdalja točke do središča bo polmer
r1 <- max(pointDistance(levi_krog,p1,lonlat = F))
r2 <- max(pointDistance(desni_krog,p2,lonlat = F))

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
sigma1 <- sqrt(0.5/alpha)
sigma2 <- sqrt(0.5/beta)

# vrednost funkcije v robnih točkah kroga, nekje okrog teh vrednosti bo b
distribution(-d/2 + r1)
distribution(d/2 - r2)
b = 0.15
b = 0.165
# oba b sta ok
preveri(p1, p2, sigma1, sigma2, b)
preveri(p1, p2, sigma1, sigma2, 0.174616)

# napoved, da lahko pregledamo podatke, če se kje predznaki ne ujemajo
prediction <- rep(0, nrow(podatki23))
for (i in 1:nrow(podatki23)){
  pred <- radialnoJedro(p1, podatki23[i,1:2], sigma1) + radialnoJedro(p2,podatki23[i,1:2], sigma2) - b
  prediction[i] <- pred
}
podatki23$pred <- prediction


plot(x = podatki$x1,y = podatki$x2)


preveri = function(p1, p2, sigma1, sigma2, b){
  data = read.csv("podatki23.csv")
  y = data$y
  for (i in 1:nrow(data)){
    pred = radialnoJedro(p1, data[i,1:2], sigma1) + radialnoJedro(p2,data[i,1:2], sigma2) - b
    pred2 = sign(pred)
    if (pred2 * y[i] < 0){
      return(0)
    }
  }
  return(1)
}

check = function(p1, p2, sigma1, sigma2, b){
  preveri(p1, p2, sigma1, sigma2, b)
}

