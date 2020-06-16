###########################################################################
# 2 Nenadzorovano uèenje
###########################################################################

options(digits=15)



# 2.1 Vinogradniški vodje

# Predpostavka o splošni legi podatkov nam pove, da nikoli ne velja y1 = y2. Pri
# merjenju katerekoli izmed razdalj med dvema podatkoma se moramo tako najprej
# premakniti do y osi, nato spremeniti višino, sledi pa spet pomik po x osi. 

# Glede na definirano metriko razdalje lahko hitro ugotovimo, da lahko v vsoti 
# posebej obravnavamo x del in y del vsote (ker spremenljivki nikjer ne nastopata 
# skupaj).

# x del: Išèemo nek xc, pri katerem po sum(|xc| + |xi|) najmanjša, pri èemer seštevamo
#        po vseh i (xi so torej x komponente vseh podatkov). Oèitno je vsota |xi| enaka 
#        ne glede na izbiro xc, m*|xc| pa je najmanjša za xc = 0. To torej predstavlja
#        x komponento našega centroida.

# y del: Išèemo yc, kjer sum(|yc - yi|) doseže minimum, kjer seveda spet seštevamo po 
#        po vseh i (oz. yi predstavljajo y komponente danih podatkov). To želimo odvajati
#        po yc, ker pa absolutna vrednost ni odvedljiva, lahko vsoto razbijemo na dve,
#        pri èemer v prvi seštevamo (yi - yc) po vseh podatkih, za katere velja (yi > yc).
#        Druga vsota pa teèe po podatkih, kjer je (yi <= yc), seštevamo pa (yc - yi). Z
#        odvajanjem obeh vsot ugotovimo, da gre pri prvi za sum(-1), pri drugi pa sum(1) po
#        vseh prej omenjenih podatkih. Nièla odvoda je dosežena, ko seštevamo isto število
#        1 in -1. Za to moramo imeti sodo število podatkov (sicer je bodisi 1x veè 1 ali -1),
#        torej bo yc moral biti eden od yi. Da pa bo 1 in -1 enako, mora biti to yi na 
#        (floor(m/2) + 1)-vem mestu v urejenem vektorju yi (tj. sredinski yi).


source("funkcije2.R")
skupina <- naloziPodatke2()

centroid <- function(podatki) {
  m <- nrow(skupina)
  sredina <- floor(m/2) + 1 # ker je m liho število
  y <- sort(skupina$y)[sredina]
  x <- 0 # zaradi predpostavke o splošni legi
  
  return(c(x, y))
}

sum(centroid(skupina))












# 2.2 Kakovost razvršèanja v skupine

podatki22 <- read.csv("podatki22.csv")
source("ward.R")

# Pri razvršèanju prvega stolpca, ki deluje kot identifikator vrstice ne smemo upoštevati!
razvrstitev <- ward(as.matrix(podatki22[, 2:ncol(podatki22)]))

# Zadnje tri stolpce spremenimo v numeriène vrednosti (z ohranjanjem vrednosti), saj bomo
# to potrebovali pri raèunanju silhuet.
razvrstitev$skupina1 <- as.numeric(paste(razvrstitev$skupina1))
razvrstitev$skupina2 <- as.numeric(paste(razvrstitev$skupina2))
razvrstitev$skupina12 <- as.numeric(paste(razvrstitev$skupina12))


# Definirajmo funkcijo, ki sprejme neko delitev podatkov (tj. 
# tabelo podatkov, kjer je za vsako toèko navedena tudi skupina)
# in izraèuna silhueto
silhueta <- function(delitev) {
  st_stolpcev <- ncol(delitev)
  st_vrstic <- nrow(delitev)
  
  skupine <- delitev[, st_stolpcev] # skupine morajo biti podane v zadnjem stolpcu
  skupine <- unique(skupine)
  
  s <- 0
  for (podatek in 1:st_vrstic) {
    xi <- delitev[podatek, 2:(st_stolpcev-1)]
    prava_skupina <- delitev[podatek, st_stolpcev]
    moc_prave <- delitev[, st_stolpcev] == prava_skupina
    moc_prave <- sum(as.numeric(moc_prave))
    if (moc_prave == 1) {
      # Opazovan podatek leži v svoji skupini, torej je s(i) = 0, zato skupnemu
      # s ne prištejemo nièesar, torej se lahko kar premaknemo na opazovanje
      # naslednjega podatka.
      next
    }
    
    # Zapišemo neko tabelo b, katere prvi stolpec predstavlja skupino, drugi 
    # stolpec pripadajoè bi, tretji pa moè dane množice.
    ai <- 0
    b <- rep(0, length(skupine)-1)
    b <- as.matrix(data.frame(skupine[skupine != prava_skupina], b, b))
    
    for (podatek2 in 1:st_vrstic) {
      if (podatek == podatek2) {
        next
      }
      
      # Èe ne gre za isti podatek, si ogledamo razdaljo med podatkoma
      # ter to upoštevamo v utrezni kolièini, tj. ai, èe gre za isto
      # skupino oz. bi, èe gre za drugo skupino.
      xj <- delitev[podatek2, 2:(st_stolpcev-1)]
      nova_skupina <- delitev[podatek2, st_stolpcev]
      
      razdalja <- d(xi, xj)
      
      if (nova_skupina == prava_skupina) {
        # Podatka ležita v isti množici, zato raèunamo ai
        ai <- ai + razdalja
      } else {
        # Sicer moramo poveèati ustrezni bi in moè množice
        b[b[, 1] == nova_skupina, 2] <- b[b[, 1] == nova_skupina, 2] + razdalja
        b[b[, 1] == nova_skupina, 3] <- b[b[, 1] == nova_skupina, 3] + 1
      }
    }
    
    ai <- ai / (moc_prave - 1)
    bi <- min(b[, 2] / b[, 3]) # Upoštevamo še moè vsake od množic
    
    si <- (bi - ai) / max(ai, bi)
    s <- s + 1/st_vrstic * si
  }
  
  return(s)
}


# Za vsak k nato glede na delitve po uporabi Ward funkcije izraèunamo silhueto. Najprej moramo podatke
# preoblikovati v želeno obliko, kjer je podana tudi pripadajoèa skupina.
podatki <- podatki22
podatki$skupina <- 1:108
n <- nrow(podatki)

silhuete <- rep(0, n) # Ko je vsaj podatek v svoji skupini, je silhueta enaka 0 -> zadnji èlen
# Za 1 skupino silhueta ni definirana, zato lahko vrednost pustimo na 0.
for (k in 1:(nrow(razvrstitev)-1)) {
  # Najprej združimo ustrezni skupini v novo
  vrstica <- razvrstitev[k, ]
  nova <- vrstica$skupina12
  stara1 <- vrstica$skupina1
  stara2 <- vrstica$skupina2
  podatki[(podatki$skupina == stara1) | (podatki$skupina == stara2), "skupina"] <- nova
  
  s_k <- silhueta(as.matrix(podatki))
  
  # Dobljeno vrednost še zapišemo na ustrezno mesto
  silhuete[n-k] <- s_k
  print(k)
}



# Razberemo za kateri k je vrednost silhuete najveèja
k <- which.max(silhuete)
s <- silhuete[k]

k + s












# 2.3 Pekarstvo Edinburg

library(plyr)

# Funkcija read.csv2 število stolpcev sklepa po prvih 5 vrsticah, zato moramo sami definirati
# dovolj veliko število stolpcev, da so podatki res pravilno urejeni, torej da se število vrstic
# v csv datoteki ujema s številom vrstic v tabeli. Prav tako manjkajoèe podatke spremenimo v NA.
podatki23 <- read.csv2("podatki23.csv", header = FALSE, fill = TRUE, col.names = as.character(1:10),
                       na.strings = "")

# # Razberemo katere vrednosti se pojavijo v podatkih in kolikokrat
# vrednosti <- as.vector(as.matrix(podatki23))
# vrednosti <- vrednosti[is.na(vrednosti) != TRUE] # znebimo se praznih vrednosti
# vrednosti <- count(vrednosti)
# 
# # Izpišemo zgolj vektor vrednosti, ki se pojavijo vsaj 20x
# pogoste <- vrednosti[vrednosti$freq >= 20, ]$x
# 
# 
# # Za vse transakcije velikosti vsaj 3 si ogledamo, èe se vsi kupljeni izdelki
# # pojavijo v vektorju pogostih vrednosti ali ne.
# 
# # Najprej sfiltriramo transakcije z vsaj 3 izdelki
# prirejeni_podatki <- podatki23[is.na(podatki23$X3) == FALSE, ]
# 
# # Nato pa si za vsako od njih ogledamo, èe res vsebuje zgolj pogoste izdelke. Èe to drži, gre 
# # za takšno transkacijo kot jo želimo, zato se števec poveèa.
# ustrezne <- 0
# for (i in 1:nrow(prirejeni_podatki)) {
#   transakcija <- prirejeni_podatki[i, ]
#   vse_pogoste <- TRUE
#   for (j in 1:length(transakcija)) {
#     izdelek <- transakcija[j][[1]] # Ker je oèitno vsako polje tabele nek seznam
#     if (is.na(izdelek) == FALSE) {
#       # Zanimajo nas zgolj dejanski izdelki
#       if ((izdelek %in% pogoste) == FALSE) {
#         # Gre za izdelek, ki ni pogost
#         vse_pogoste <- FALSE
#       }
#     }
#   }
#   if (vse_pogoste) {
#     # Èe so vsi izdelki v transakciji pogosti, poveèamo števec
#     ustrezne <- ustrezne + 1
#   }
# }
# 
# ustrezne



####################################################################################

# Èe opazujemo zgolj transakcije, ki se v celotni ponovijo vsak 20x -> pravilna rešitev!

# Najprej vse vrstice uredimo po abecednem vrstem redu, zato da vrstni red v množici ne
# vpliva na konèni rezultat
for (i in 1:nrow(podatki23)) {
  vrstica <- podatki23[i, ]
  len <- length(vrstica)
  vrstica <- sort(vrstica)
  podatki23[i, ] <- c(sort(vrstica), rep(NA, len - length(vrstica))) # Z NA dopolnimo vrstico do prave dolžine
}

# Nato si ogledamo kolikokrat se doloèene transakcije pojavijo in izlušèimo tiste,
# ki se pojavijo vsaj 20x
pogoste <- count(podatki23)
pogoste <- pogoste[pogoste$freq >= 20, ]

# Sfiltriramo transakcije velikosti vsaj 3
pogoste <- pogoste[is.na(pogoste$X3) == FALSE, ]

# Vsoto pojavitev vseh pogostih množic velikosti vsaj 3 dobimo kot
sum(pogoste$freq)



