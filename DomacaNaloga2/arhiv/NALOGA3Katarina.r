#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)

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
# setwd("pot do mape (path to directory)")

naloga_problem = 1
source(sprintf("funkcije%d.R", naloga_problem))

#########################################################################
# NEVRONSKE MREŽE
#########################################################################

#########################################################################
# 3.1 ENOSTAVNI PERCEPTRON
#########################################################################
source("kviz3.R")
podatki31 <- read.csv("podatki31.csv")
slika = naloziSliko(); image = slika;
w0 <- -0.2989601680217311
w1 <- 0.1
w2 <- 0.2

enostavni_perceptron <- function(){
  n <- nrow(podatki31)
  podatki_pozitivni <- podatki31 %>% filter(podatki31$y == 1)
  podatki_negativni <- podatki31 %>% filter(podatki31$y == -1)
  n1 <- nrow(podatki_pozitivni)
  n2 <- n - n1
  pozitivni <- rep(0,n1)
  negativni <- rep(0,n2)
  for (i in 1:n1){
    sum <- w0 + w1 * podatki_pozitivni[i,1]+ w2 * podatki_pozitivni[i,2]
    pogoj <-  -sum/podatki_pozitivni[i,3]
    pozitivni[i] <- pogoj
  }
  # w3 mora biti večji od vseh, tudi od največjega pogoja
  najvecji <- max(pozitivni)
  
  for (i in 1:n2){
    sum <- w0 + w1 * podatki_negativni[i,1]+ w2 * podatki_negativni[i,2]
    pogoj <-  -sum/podatki_negativni[i,3]
    negativni[i] <- pogoj
  }
  # w3 mora biti manjši od vseh, tudi od najmanjšega pogoja
  najmanjsi <- min(negativni)
  if(najvecji < najmanjsi){
    return(najvecji)
  }else{
    print("napaka")
  }
}

w3 <- enostavni_perceptron()

# funkcija preveri, če izbran w3 popolnoma loči razreda
preveri_w = function(w3){
  data = read.csv("podatki31.csv")
  y = data$y
  for (i in 1:nrow(data)){
    v = w0 + w1 * data[i,1]+ w2 * data[i,2] + w3 * data[i,3]
    predznak = sign(v)
    if (predznak * y[i] < 0){
      return(0)
    }
  }
  return(1)
}


preveri_w(w3)

#########################################################################
# 3.2 KAKO VELIKE SO MREŽE?
#########################################################################

korak <- function(dim,kanal,filter,st_konvolucij){
  utezi = 0
  for (i in 1:st_konvolucij){
    konvolucija <- filter * (9 * kanal + 1)
    #print(konvolucija)
    utezi <- utezi + konvolucija
    dim = dim - 2
    kanal = filter
  }
  dim = floor(dim/2)
  return(c(dim,kanal,filter,utezi))
}

k <- data.frame(st_konvolucij = c(2,2,3,3,3),
                filter = c(64,128,256,512,512))

utezi <- function(k){
  dim = 224
  kanal = 1
  filter = 64
  st_utezi = 0
  
  for (j in 1:nrow(k)){
    st_konvolucij <- k[j,1]
    filter <- k[j,2]
    rezultat <- korak(dim,kanal,filter,st_konvolucij)
    dim <- rezultat[1]
    kanal <- rezultat[2]
    filter <- rezultat[3]
    st_utezi <- st_utezi + rezultat[4]
  }
  return(st_utezi)
}


utezi(k) + 4096 * ( 512 + 1) + 4096 * (4096+1) + 2 * (4096 + 1)
#########################################################################
# 3.3 ODKRIJMO MODER KVADRAT
#########################################################################

moder_kvadrat <- function(slika){
  n <- nrow(slika)
  for(i in 1:n){
    for(j in 1:n){
      if(
        slika[i,j,1] == 0 &
        slika[i,j,2] == 0 &
        slika[i,j,3] > 0 
      ){
        # c(i,j) bo zgornji levi kot
        # prištejemo 2 obema koordinatama, da dobimo središče
        # vemo, da je dimenzija kvadrata 5x5
        return(c(i +  2 ,j + 2))
      }
    }
  }
}

sredisce <- moder_kvadrat(slika)
97 * sredisce[1] + 101 * sredisce[2]


