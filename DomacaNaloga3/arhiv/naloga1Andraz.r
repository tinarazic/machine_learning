###########################################################################
# 1 Ansambli
###########################################################################

options(digits=15)
podatki <- read.csv("podatki1.csv", header = FALSE)
koristnosti <- read.csv("koristnosti1.csv", header = FALSE)



# 1.1 Kako podobna so si drevesa?

# Najprej razberemo �tevilo dreves in primerov
m <- nrow(podatki)
n <- ncol(podatki) - 1

# Zapi�emo vektor korelacij, iz katerega bomo nato razbrali njegovo povpre�je
korelacije <- c()

for (i in 2:(n+1)) {
  a <- podatki[, i]
  povp_a <- mean(a)
  # Upo�tevamo simetri�nost korelacije: ro(i, j) = ro(j, i)
  for (j in 2:(n+1)) {
    if (i == j) {
      # Korelacije drevesa samega s seboj ne opazujemo
      next()
    }
    b <- podatki[, j]
    povp_b <- mean(b)
    
    # Iza�unamo vse potrebne vsote
    # prod <- 0
    # a2 <- 0
    # b2 <- 0
    # for (k in 1:m) {
    #   prod <- prod + (a[k] - povp_a) * (b[k] - povp_b)
    #   a2 <- a2 + (a[k] - povp_a)^2
    #   b2 <- b2 + (b[k] - povp_b)^2
    # }
    
    # Lahko tudi direktno z vektorskim ra�unanjem (tj. brez zanke)
    prod <- sum((a - povp_a) * (b - povp_b))
    a2 <- sum((a - povp_a)^2)
    b2 <- sum((b - povp_b)^2)
    
    # Korelacijo izra�unamo po formuli iz navodil
    ro <- prod / (sqrt(a2) * sqrt(b2))
    
    # Dobljeno vrednost dodamo v vektor
    korelacije <- append(korelacije, ro)
  }
}

# Izpi�emo kon�ni rezultat -> povpre�no vrednost
mean(korelacije)








# 1.2 Optimizacija ute�i

library(limSolve)

validacijska <- podatki[1:(m/2), ]
testna <- podatki[(m/2+1):m, ]

# Najprej poi��emo optimalne ute�i -> te so dolo�ene s predolo�enim sistemom 
# Pw = y, kjer je P matrika napovedi dreves (razse�nosti (m/2) x n), w vektor
# ute�i in y vektor dejanskih vrednosti. Ker gre za predolo�en sistem, i��emo
# pa re�itev z najmanj�o srednjo kvadrati�no napako, uporabimo funkcijo lsei 
# iz paketa limSolve, ki i��e re�itev z manjmanj�o kvadratno napako, tj. natanko
# to kar �elimo. Ta funkcija vrne ve� vrednosti, z $X pa dostopamo do dejanskih
# vrednosti ute�i.
P <- as.matrix(validacijska[, -1]) # prvega stolpca dejanskih vrednosti ne potrebujemo
y <- as.vector(validacijska[, 1])
optimalne_utezi <- lsei(A = P, B = y, fulloutput = FALSE, verbose = FALSE)$X


# Funkcija, ki za podane ute�i izra�una napako gozda
izracunajNapako <- function(utezi) {
  prave_vrednosti <- testna[, 1]
  napovedi <- c()
  for (i in 1:nrow(testna)) {
    # Po formuli iz navodil izra�unamo napoved za vsak primer testne 
    # mno�ice
    yi <- 0
    for (j in 2:(n+1)) {
      yi <- yi + utezi[j-1]*testna[i, j]
    }
    napovedi <- append(napovedi, yi)
  }
  
  napaka <- mean((prave_vrednosti - napovedi)^2)
  return(napaka)
}


# Izra�unajmo �e iskane vrednosti
egOPT <- izracunajNapako(optimalne_utezi)
egSTD <- izracunajNapako(rep(1/n, n))
egSTD - egOPT







# 1.3 Koliko dreves zado��a?

ustalitev <- function(drevesneKakovosti) {
  p <- nrow(drevesneKakovosti) # to nam pove �tevilo spremenljivk
  n <- ncol(drevesneKakovosti)
  
  povprecne_koristnosti <- rowMeans(drevesneKakovosti)
  
  # Najprej poi��emo vrstni red spremenljivk, �e upo�tevamo vsa drevesa. Parameter
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
    
    # �e je dobljen vrstni red enak kon�nemu, bi lahko upo�tevali �e eno
    # drevo manj in dobili enak rezultat, �e pa je vrstni red druga�en od
    # kon�nega, moramo upo�tevati (n_vijuga + 1) dreves
    if (all(V_vijuga == V) == FALSE) {
      # Torej vse vrednosti niso enake (oz. je druga�na vsaj ena)
      return(n_vijuga + 1)
    }
  }
  return(1)
}

ustalitev(koristnosti)



