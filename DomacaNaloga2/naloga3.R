########################################################################################
# Domača naloga 2
# 3 Nevronske mreže
########################################################################################

# working directory
setwd("D:/Dokumenti/FAKS/magisterij/machine_learning/DomacaNaloga2")

# naložimo knjižnice
options(digits = 16)
library(png)

# ukazi za kviz
source("kviz3.R")
slika = naloziSliko()

# 3.1 Enostaven perceptron
podatki31 <- read.csv("podatki31.csv")

enostaven.perceptron <- function(w0,w1,w2, podatki){
  # funkcija, ki ob danih vhodnih
  # podatkih najde natančno spodnjo mejo za primerne w3
  velikost <- dim(podatki)[1]
  vrednosti <- rep(0,velikost)
  matrika <- data.frame(podatki$y,vrednosti)
  colnames(matrika) <- c("y", "vrednosti")
  for(i in 1:velikost){
    matrika$vrednosti[i] <- -(w0+w1*podatki$x1[i]+w2*podatki$x2[i])/podatki$x3[i]
  }
  #za negativne
  negativni <- matrika$vrednosti[matrika$y==-1]
  pozitivni <- matrika$vrednosti[matrika$y== 1]
  
  maxspodaj <- max(negativni)
  minspodaj <- min(negativni)
  maxzgoraj <- max(pozitivni)
  minzgoraj <- min(pozitivni)
  #na kvizu je rešitev maxzgoraj
  
  vektor <- list(maxspodaj,minspodaj,maxzgoraj, minzgoraj)
  return(vektor)
}

# kviz
w0 <- -0.2989601680217311
w1 <- 0.1
w2 <- 0.2
podatki <- podatki31
spodnja.meja.kviz <- enostaven.perceptron(w0,w1,w2, podatki)
# glej 3 številko v tem primeru

# 3.2 Kako velike so mreže?

korak <- function(dim,kanal,filter,st.konvolucij){
  utezi <- 0
  for (i in 1:st.konvolucij){
    konvolucija <- filter * (9 * kanal + 1)
    #print(konvolucija)
    utezi <- utezi + konvolucija
    dim <- dim - 2
    kanal <- filter
  }
  dim <- floor(dim/2)
  return(c(dim,kanal,filter,utezi))
}

k <- data.frame(st_konvolucij = c(2,2,3,3,3),
                filter = c(64,128,256,512,512))

utezi <- function(k){
  dim <- 224
  kanal <-  1
  # filter <- 64
  st.utezi <- 0
  
  for (j in 1:nrow(k)){
    st.konvolucij <- k[j,1]
    filter <- k[j,2]
    rezultat <- korak(dim,kanal,filter,st.konvolucij)
    dim <- rezultat[1]
    print(dim)
    kanal <- rezultat[2]
    filter <- rezultat[3]
    st.utezi <- st.utezi + rezultat[4]
  }
  return(st.utezi)
}


skupaj.utezi <- utezi(k) + 4096 * ( 512 + 1) + 4096 * (4096+1) + 2 * (4096 + 1)
skupaj.utezi

# 3.3 Odkrijmo moder kvadrat

# ker so kvadrati velikost 5, poiščemo spodnji levi kvadrata in nato prištejemo 2, da pridemo do središča
odkrijModerKvadrat <- function(slika){
  # funkcija sprejme barvno sliko in odkrije položaj (kot par (x; y)) središča 
  # modrega kvadrata velikosti 5 x 5 pikslov

  # X je tenzor
  # X ima tri dimenzije; X[i,j,k] za vsak par pikslov (i,j)
  # tretja dimenzija je barva X[,,i], i in 1,2,3
  velikost <- dim(X)
  for(i in 1:velikost[1]){
    for(j in 1:velikost[2]){
      if(X[i,j,1] == 0 && X[i,j,2] == 0 && X[i,j,3] > 0){
        levo <- i + 2
        spodaj <- j + 2
        polozaj <- list(levo,spodaj)
        return(polozaj)
      }
    }
  }
}


# kviz
sredisce.kviz <- odkrijModerKvadrat(slika)
resitev.kviz <- 97*sredisce.kviz[[1]] + 101*sredisce.kviz[[2]]
