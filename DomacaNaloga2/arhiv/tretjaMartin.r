#115 120
#3.1.
library(readr)
podatki31 <- read_csv("podatki31.csv")

razvrstitev <- function(w0,w1,w2){
  vrednosti <- rep(0,dim(podatki31)[1])
  matrika <- data.frame(podatki31$y,vrednosti)
  for(i in 1:dim(podatki)[1]){
    matrika$vrednosti[i] <- -(w0+w1*podatki$x1[i]+w2*podatki$x2[i])/podatki$x3[i]
  }
  #za negativne
  negativni <- matrika$vrednosti[matrika$podatki31.y==-1]
  pozitivni <- matrika$vrednosti[matrika$podatki31.y== 1]
  
  maxspodnje <- max(negativni)
  minspodaj <- min(negativni)
  minzgoraj <- min(pozitivni)
  maxzgoraj <- max(pozitivni)
  #na kvizu je rešitev maxzgoraj
  
  vektor <- list(maxspodnje,minspodaj,minzgoraj,maxzgoraj)
  return(vektor)
}


#3.2.

#korak pri številu uteži nima vpliva!

#vir https://www.learnopencv.com/number-of-parameters-and-tensor-sizes-in-convolutional-neural-network/
# https://stackoverflow.com/questions/28232235/how-to-calculate-the-number-of-parameters-of-convolutional-neural-networks
#na stack drugi odgovor!



#nas primer
#input 224,224,1
#conv3-64 [222x222X1]
#conv3-64 [220x220X64]
#pool2-64 110x110x64
#conv3-128 [108x108x64]
#conv3-128 [106x106X128]
#pool2-128 53x53x128
#conv3-256 [51x51x128]
#conv3-256 [49x49x256]
#conv3-256 [47x47x256]
#pool2-256 23.5x23.5x256
#conv3-512 [21.5x21.5x256]
#conv3-512 [19.5x19.5x512]
#conv3-512 [17.5x17.5x512]
#pool2-512 8.75x8.75x512
#conv3-512 [6.75x6.75x512]
#conv3-512 [4.75x4.75x512]  
#conv3-512 [2.75x2.75x512]
#pool2-512 1.375x1.375x512
#fc4096
#fc4096
#fc2


stevilo_konvolucija <- function(k,n,c){
  #cnn
  #K = Size (width) of kernels used in the Conv Layer. oblika filtra
  #N = Number of kernels. število filtrov
  #C = Number of channels of the input image. velikost vhoda
  rezultat <- k^2*n*c
  return(rezultat)
}

stevilo_zbiranje<- function(){
  #pooling
  #to ne vpliva, doda uteži!
  return(0)
}

stevilo_polnopovezan_konvolucija <- function(o,n,f){
  #O = Size (width) of the output image of the previous Conv Layer.
  #N = Number of kernels in the previous Conv Layer.
  #F = Number of neurons in the FC Layer.
  rezultat <- o^2*n*f
  return(rezultat)
  }

stevilo_polnopovezan_polnopovezan <- function(f,f_){
  #F = Number of neurons in the FC Layer.
  #F_ = Number of neurons in the previous FC Layer.
  rezultat <- f*f_
  return(rezultat)
}

stevilo_input <- function(){
  return(0)
} 

stevilo_output <- function(){
  return(0)
} 

hidden_conv <- function(h1,f,p,s){
  #h1 je oblika vhoda
  #f je oblika filra
  #p je padding, pri nas 0
  #s je korak!
  rezultat <- (h1-f+2*p)/s+1
  return(rezultat)
}

hidden_pool <- function(h1,f,s){
  #h1 je oblika vhoda
  #f je oblika filra
  #s je korak!
  rezultat <- (h1-f)/s+1
  return(rezultat)
}

#pri nas je padding 0!!!!!!!
#https://cs231n.github.io/convolutional-networks/
nas_primer <- stevilo_input()+stevilo_konvolucija(3,64,1)+stevilo_konvolucija(3,64,64)+
  stevilo_zbiranje()+
  stevilo_konvolucija(3,128,64)+stevilo_konvolucija(3,128,128)+
  stevilo_zbiranje()+
  stevilo_konvolucija(3,256,128)+
  stevilo_konvolucija(3,256,256)+
  stevilo_konvolucija(3,256,256)+
  stevilo_zbiranje()+
  stevilo_konvolucija(3,512,256)+
  stevilo_zbiranje()+
  stevilo_konvolucija(3,512,512)+
  stevilo_konvolucija(3,512,512)+
  stevilo_konvolucija(3,512,512)+
  stevilo_zbiranje()+
  stevilo_konvolucija(3,512,512)+ 
  stevilo_konvolucija(3,512,512)+ 
  stevilo_konvolucija(3,512,512)+ 
  stevilo_zbiranje()+
  #tukaj so koraki povezani z poolingom!
  stevilo_polnopovezan_konvolucija(1,512,4096)+
  stevilo_polnopovezan_polnopovezan(4096,4096)+
  stevilo_polnopovezan_polnopovezan(4096,2)+
  stevilo_output()
  

#3.3
library(png)
naloziSliko = function(){  # loadImage
  slika = readPNG("slikaKviz.png")
  slika = slika[,,1:3]
  return(slika)
}
X <- naloziSliko()
#ker so kvadrati velikost 5, poiščemo zgoraj desni kot po sliki; in nato prištejemo
odkrijModro <- function(X){
  #n je velikost modrega kvadrata; iz tega lahko dobimo nato center, oziroma iskano vrednost
  #X je tenzor
  #X ima tri dimenzije; X[i,j,k] za vsak par pikslov (i,j)
  #X tretja dimenzija je X[,,i], i in 1,2,3
  velikost <- dim(X)
  for(i in 1:velikost[1]){
    for(j in 1:velikost[2]){
      if(X[i,j,1] == 0 && X[i,j,2] == 0 && X[i,j,3] > 0){
        levo <- i+2
        zgoraj <- j+2
        polozaj <- list(levo,zgoraj)
        return(polozaj)
      }
    }
  }
  return(vektor)
} 
