###########################################################################
# 1 Ansambli
###########################################################################

options(digits=15)
podatki <- read.csv("podatki1.csv", header = FALSE)
koristnosti <- read.csv("koristnosti1.csv", header = FALSE)



# 1.1 Kako podobna so si drevesa?

# Najprej razberemo število dreves in primerov
m <- nrow(podatki)
n <- ncol(podatki) - 1

# Zapišemo vektor korelacij, iz katerega bomo nato razbrali njegovo povpreèje
korelacije <- c()

for (i in 2:(n+1)) {
  a <- podatki[, i]
  povp_a <- mean(a)
  # Upoštevamo simetriènost korelacije: ro(i, j) = ro(j, i)
  for (j in 2:(n+1)) {
    if (i == j) {
      # Korelacije drevesa samega s seboj ne opazujemo
      next()
    }
    b <- podatki[, j]
    povp_b <- mean(b)
    
    # Izaèunamo vse potrebne vsote
    # prod <- 0
    # a2 <- 0
    # b2 <- 0
    # for (k in 1:m) {
    #   prod <- prod + (a[k] - povp_a) * (b[k] - povp_b)
    #   a2 <- a2 + (a[k] - povp_a)^2
    #   b2 <- b2 + (b[k] - povp_b)^2
    # }
    
    # Lahko tudi direktno z vektorskim raèunanjem (tj. brez zanke)
    prod <- sum((a - povp_a) * (b - povp_b))
    a2 <- sum((a - povp_a)^2)
    b2 <- sum((b - povp_b)^2)
    
    # Korelacijo izraèunamo po formuli iz navodil
    ro <- prod / (sqrt(a2) * sqrt(b2))
    
    # Dobljeno vrednost dodamo v vektor
    korelacije <- append(korelacije, ro)
  }
}

# Izpišemo konèni rezultat -> povpreèno vrednost
mean(korelacije)








# 1.2 Optimizacija uteži

library(limSolve)

validacijska <- podatki[1:(m/2), ]
testna <- podatki[(m/2+1):m, ]

# Najprej poišèemo optimalne uteži -> te so doloèene s predoloèenim sistemom 
# Pw = y, kjer je P matrika napovedi dreves (razsežnosti (m/2) x n), w vektor
# uteži in y vektor dejanskih vrednosti. Ker gre za predoloèen sistem, išèemo
# pa rešitev z najmanjšo srednjo kvadratièno napako, uporabimo funkcijo lsei 
# iz paketa limSolve, ki išèe rešitev z manjmanjšo kvadratno napako, tj. natanko
# to kar želimo. Ta funkcija vrne veè vrednosti, z $X pa dostopamo do dejanskih
# vrednosti uteži.
P <- as.matrix(validacijska[, -1]) # prvega stolpca dejanskih vrednosti ne potrebujemo
y <- as.vector(validacijska[, 1])
optimalne_utezi <- lsei(A = P, B = y, fulloutput = FALSE, verbose = FALSE)$X


# Funkcija, ki za podane uteži izraèuna napako gozda
izracunajNapako <- function(utezi) {
  prave_vrednosti <- testna[, 1]
  napovedi <- c()
  for (i in 1:nrow(testna)) {
    # Po formuli iz navodil izraèunamo napoved za vsak primer testne 
    # množice
    yi <- 0
    for (j in 2:(n+1)) {
      yi <- yi + utezi[j-1]*testna[i, j]
    }
    napovedi <- append(napovedi, yi)
  }
  
  napaka <- mean((prave_vrednosti - napovedi)^2)
  return(napaka)
}


# Izraèunajmo še iskane vrednosti
egOPT <- izracunajNapako(optimalne_utezi)
egSTD <- izracunajNapako(rep(1/n, n))
egSTD - egOPT







# 1.3 Koliko dreves zadošèa?

ustalitev <- function(drevesneKakovosti) {
  p <- nrow(drevesneKakovosti) # to nam pove število spremenljivk
  n <- ncol(drevesneKakovosti)
  
  povprecne_koristnosti <- rowMeans(drevesneKakovosti)
  
  # Najprej poišèemo vrstni red spremenljivk, èe upoštevamo vsa drevesa. Parameter
  # index.return nam poda stolpec ix, ki pove indekse urejenega seznama, kar je 
  # ravno vrednost, ki nas zanima.
  V <- sort(povprecne_koristnosti, index.return = TRUE)$ix
  
  for (n_vijuga in (n-1):1) {
    # Ogledamo si le prvih n_vijuga dreves in ponovimo isti postopek kot
    # prej.
    if (n_vijuga > 1) {
      povprecne <- rowMeans(drevesneKakovosti[, 1:n_vijuga])
    } else {
      # Ker v tem primeru ukaz rowMeans ne deluje pravilno!
      povprecne <- mean(drevesneKakovosti[, 1:n_vijuga])
    }
    V_vijuga <- sort(povprecne, index.return = TRUE)$ix
    
    # Èe je dobljen vrstni red enak konènemu, bi lahko upoštevali že eno
    # drevo manj in dobili enak rezultat, èe pa je vrstni red drugaèen od
    # konènega, moramo upoštevati (n_vijuga + 1) dreves
    if (all(V_vijuga == V) == FALSE) {
      # Torej vse vrednosti niso enake (oz. je drugaèna vsaj ena)
      return(n_vijuga + 1)
    }
  }
  return(1)
}

ustalitev(koristnosti)



