###########################################################################
# 2 Nenadzorovano u�enje
###########################################################################

options(digits=15)



# 2.1 Vinogradni�ki vodje

# Predpostavka o splo�ni legi podatkov nam pove, da nikoli ne velja y1 = y2. Pri
# merjenju katerekoli izmed razdalj med dvema podatkoma se moramo tako najprej
# premakniti do y osi, nato spremeniti vi�ino, sledi pa spet pomik po x osi. 

# Glede na definirano metriko razdalje lahko hitro ugotovimo, da lahko v vsoti 
# posebej obravnavamo x del in y del vsote (ker spremenljivki nikjer ne nastopata 
# skupaj).

# x del: I��emo nek xc, pri katerem po sum(|xc| + |xi|) najmanj�a, pri �emer se�tevamo
#        po vseh i (xi so torej x komponente vseh podatkov). O�itno je vsota |xi| enaka 
#        ne glede na izbiro xc, m*|xc| pa je najmanj�a za xc = 0. To torej predstavlja
#        x komponento na�ega centroida.

# y del: I��emo yc, kjer sum(|yc - yi|) dose�e minimum, kjer seveda spet se�tevamo po 
#        po vseh i (oz. yi predstavljajo y komponente danih podatkov). To �elimo odvajati
#        po yc, ker pa absolutna vrednost ni odvedljiva, lahko vsoto razbijemo na dve,
#        pri �emer v prvi se�tevamo (yi - yc) po vseh podatkih, za katere velja (yi > yc).
#        Druga vsota pa te�e po podatkih, kjer je (yi <= yc), se�tevamo pa (yc - yi). Z
#        odvajanjem obeh vsot ugotovimo, da gre pri prvi za sum(-1), pri drugi pa sum(1) po
#        vseh prej omenjenih podatkih. Ni�la odvoda je dose�ena, ko se�tevamo isto �tevilo
#        1 in -1. Za to moramo imeti sodo �tevilo podatkov (sicer je bodisi 1x ve� 1 ali -1),
#        torej bo yc moral biti eden od yi. Da pa bo 1 in -1 enako, mora biti to yi na 
#        (floor(m/2) + 1)-vem mestu v urejenem vektorju yi (tj. sredinski yi).


source("funkcije2.R")
skupina <- naloziPodatke2()

centroid <- function(podatki) {
  m <- nrow(skupina)
  sredina <- floor(m/2) + 1 # ker je m liho �tevilo
  y <- sort(skupina$y)[sredina]
  x <- 0 # zaradi predpostavke o splo�ni legi
  
  return(c(x, y))
}

sum(centroid(skupina))












# 2.2 Kakovost razvr��anja v skupine

podatki22 <- read.csv("podatki22.csv")
source("ward.R")

# Pri razvr��anju prvega stolpca, ki deluje kot identifikator vrstice ne smemo upo�tevati!
razvrstitev <- ward(as.matrix(podatki22[, 2:ncol(podatki22)]))

# Zadnje tri stolpce spremenimo v numeri�ne vrednosti (z ohranjanjem vrednosti), saj bomo
# to potrebovali pri ra�unanju silhuet.
razvrstitev$skupina1 <- as.numeric(paste(razvrstitev$skupina1))
razvrstitev$skupina2 <- as.numeric(paste(razvrstitev$skupina2))
razvrstitev$skupina12 <- as.numeric(paste(razvrstitev$skupina12))


# Definirajmo funkcijo, ki sprejme neko delitev podatkov (tj. 
# tabelo podatkov, kjer je za vsako to�ko navedena tudi skupina)
# in izra�una silhueto
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
      # Opazovan podatek le�i v svoji skupini, torej je s(i) = 0, zato skupnemu
      # s ne pri�tejemo ni�esar, torej se lahko kar premaknemo na opazovanje
      # naslednjega podatka.
      next
    }
    
    # Zapi�emo neko tabelo b, katere prvi stolpec predstavlja skupino, drugi 
    # stolpec pripadajo� bi, tretji pa mo� dane mno�ice.
    ai <- 0
    b <- rep(0, length(skupine)-1)
    b <- as.matrix(data.frame(skupine[skupine != prava_skupina], b, b))
    
    for (podatek2 in 1:st_vrstic) {
      if (podatek == podatek2) {
        next
      }
      
      # �e ne gre za isti podatek, si ogledamo razdaljo med podatkoma
      # ter to upo�tevamo v utrezni koli�ini, tj. ai, �e gre za isto
      # skupino oz. bi, �e gre za drugo skupino.
      xj <- delitev[podatek2, 2:(st_stolpcev-1)]
      nova_skupina <- delitev[podatek2, st_stolpcev]
      
      razdalja <- d(xi, xj)
      
      if (nova_skupina == prava_skupina) {
        # Podatka le�ita v isti mno�ici, zato ra�unamo ai
        ai <- ai + razdalja
      } else {
        # Sicer moramo pove�ati ustrezni bi in mo� mno�ice
        b[b[, 1] == nova_skupina, 2] <- b[b[, 1] == nova_skupina, 2] + razdalja
        b[b[, 1] == nova_skupina, 3] <- b[b[, 1] == nova_skupina, 3] + 1
      }
    }
    
    ai <- ai / (moc_prave - 1)
    bi <- min(b[, 2] / b[, 3]) # Upo�tevamo �e mo� vsake od mno�ic
    
    si <- (bi - ai) / max(ai, bi)
    s <- s + 1/st_vrstic * si
  }
  
  return(s)
}


# Za vsak k nato glede na delitve po uporabi Ward funkcije izra�unamo silhueto. Najprej moramo podatke
# preoblikovati v �eleno obliko, kjer je podana tudi pripadajo�a skupina.
podatki <- podatki22
podatki$skupina <- 1:108
n <- nrow(podatki)

silhuete <- rep(0, n) # Ko je vsaj podatek v svoji skupini, je silhueta enaka 0 -> zadnji �len
# Za 1 skupino silhueta ni definirana, zato lahko vrednost pustimo na 0.
for (k in 1:(nrow(razvrstitev)-1)) {
  # Najprej zdru�imo ustrezni skupini v novo
  vrstica <- razvrstitev[k, ]
  nova <- vrstica$skupina12
  stara1 <- vrstica$skupina1
  stara2 <- vrstica$skupina2
  podatki[(podatki$skupina == stara1) | (podatki$skupina == stara2), "skupina"] <- nova
  
  s_k <- silhueta(as.matrix(podatki))
  
  # Dobljeno vrednost �e zapi�emo na ustrezno mesto
  silhuete[n-k] <- s_k
  print(k)
}



# Razberemo za kateri k je vrednost silhuete najve�ja
k <- which.max(silhuete)
s <- silhuete[k]

k + s












# 2.3 Pekarstvo Edinburg

library(plyr)

# Funkcija read.csv2 �tevilo stolpcev sklepa po prvih 5 vrsticah, zato moramo sami definirati
# dovolj veliko �tevilo stolpcev, da so podatki res pravilno urejeni, torej da se �tevilo vrstic
# v csv datoteki ujema s �tevilom vrstic v tabeli. Prav tako manjkajo�e podatke spremenimo v NA.
podatki23 <- read.csv2("podatki23.csv", header = FALSE, fill = TRUE, col.names = as.character(1:10),
                       na.strings = "")

# # Razberemo katere vrednosti se pojavijo v podatkih in kolikokrat
# vrednosti <- as.vector(as.matrix(podatki23))
# vrednosti <- vrednosti[is.na(vrednosti) != TRUE] # znebimo se praznih vrednosti
# vrednosti <- count(vrednosti)
# 
# # Izpi�emo zgolj vektor vrednosti, ki se pojavijo vsaj 20x
# pogoste <- vrednosti[vrednosti$freq >= 20, ]$x
# 
# 
# # Za vse transakcije velikosti vsaj 3 si ogledamo, �e se vsi kupljeni izdelki
# # pojavijo v vektorju pogostih vrednosti ali ne.
# 
# # Najprej sfiltriramo transakcije z vsaj 3 izdelki
# prirejeni_podatki <- podatki23[is.na(podatki23$X3) == FALSE, ]
# 
# # Nato pa si za vsako od njih ogledamo, �e res vsebuje zgolj pogoste izdelke. �e to dr�i, gre 
# # za tak�no transkacijo kot jo �elimo, zato se �tevec pove�a.
# ustrezne <- 0
# for (i in 1:nrow(prirejeni_podatki)) {
#   transakcija <- prirejeni_podatki[i, ]
#   vse_pogoste <- TRUE
#   for (j in 1:length(transakcija)) {
#     izdelek <- transakcija[j][[1]] # Ker je o�itno vsako polje tabele nek seznam
#     if (is.na(izdelek) == FALSE) {
#       # Zanimajo nas zgolj dejanski izdelki
#       if ((izdelek %in% pogoste) == FALSE) {
#         # Gre za izdelek, ki ni pogost
#         vse_pogoste <- FALSE
#       }
#     }
#   }
#   if (vse_pogoste) {
#     # �e so vsi izdelki v transakciji pogosti, pove�amo �tevec
#     ustrezne <- ustrezne + 1
#   }
# }
# 
# ustrezne



####################################################################################

# �e opazujemo zgolj transakcije, ki se v celotni ponovijo vsak 20x -> pravilna re�itev!

# Najprej vse vrstice uredimo po abecednem vrstem redu, zato da vrstni red v mno�ici ne
# vpliva na kon�ni rezultat
for (i in 1:nrow(podatki23)) {
  vrstica <- podatki23[i, ]
  len <- length(vrstica)
  vrstica <- sort(vrstica)
  podatki23[i, ] <- c(sort(vrstica), rep(NA, len - length(vrstica))) # Z NA dopolnimo vrstico do prave dol�ine
}

# Nato si ogledamo kolikokrat se dolo�ene transakcije pojavijo in izlu��imo tiste,
# ki se pojavijo vsaj 20x
pogoste <- count(podatki23)
pogoste <- pogoste[pogoste$freq >= 20, ]

# Sfiltriramo transakcije velikosti vsaj 3
pogoste <- pogoste[is.na(pogoste$X3) == FALSE, ]

# Vsoto pojavitev vseh pogostih mno�ic velikosti vsaj 3 dobimo kot
sum(pogoste$freq)



