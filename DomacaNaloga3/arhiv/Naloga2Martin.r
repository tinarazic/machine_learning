# 2.2 Kakovost razvrščanja v skupine

podatki22 <- read.csv("podatki22.csv")
source("ward.R")

# Pri razvrščanju prvega stolpca, ki deluje kot identifikator vrstice ne smemo upoštevati!
razvrstitev <- ward(as.matrix(podatki22[, 2:ncol(podatki22)]))

# Zadnje tri stolpce spremenimo v numerične vrednosti (z ohranjanjem vrednosti), saj bomo
# to potrebovali pri računanju silhuet.
razvrstitev$skupina1 <- as.numeric(paste(razvrstitev$skupina1))
razvrstitev$skupina2 <- as.numeric(paste(razvrstitev$skupina2))
razvrstitev$skupina12 <- as.numeric(paste(razvrstitev$skupina12))


# Definirajmo funkcijo, ki sprejme neko delitev podatkov (tj. 
# tabelo podatkov, kjer je za vsako točko navedena tudi skupina)
# in izračuna silhueto
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
      # s ne prištejemo ničesar, torej se lahko kar premaknemo na opazovanje
      # naslednjega podatka.
      next
    }
    
    # Zapišemo neko tabelo b, katere prvi stolpec predstavlja skupino, drugi 
    # stolpec pripadajoč bi, tretji pa moč dane množice.
    ai <- 0
    b <- rep(0, length(skupine)-1)
    b <- as.matrix(data.frame(skupine[skupine != prava_skupina], b, b))
    
    for (podatek2 in 1:st_vrstic) {
      if (podatek == podatek2) {
        next
      }
      
      # Če ne gre za isti podatek, si ogledamo razdaljo med podatkoma
      # ter to upoštevamo v utrezni količini, tj. ai, če gre za isto
      # skupino oz. bi, če gre za drugo skupino.
      xj <- delitev[podatek2, 2:(st_stolpcev-1)]
      nova_skupina <- delitev[podatek2, st_stolpcev]
      
      razdalja <- d(xi, xj)
      
      if (nova_skupina == prava_skupina) {
        # Podatka ležita v isti množici, zato računamo ai
        ai <- ai + razdalja
      } else {
        # Sicer moramo povečati ustrezni bi in moč množice
        b[b[, 1] == nova_skupina, 2] <- b[b[, 1] == nova_skupina, 2] + razdalja
        b[b[, 1] == nova_skupina, 3] <- b[b[, 1] == nova_skupina, 3] + 1
      }
    }
    
    ai <- ai / (moc_prave - 1)
    bi <- min(b[, 2] / b[, 3]) # Upoštevamo še moč vsake od množic
    
    si <- (bi - ai) / max(ai, bi)
    s <- s + 1/st_vrstic * si
  }
  
  return(s)
}


# Za vsak k nato glede na delitve po uporabi Ward funkcije izračunamo silhueto. Najprej moramo podatke
# preoblikovati v želeno obliko, kjer je podana tudi pripadajoča skupina.
podatki <- podatki22
podatki$skupina <- 1:108
n <- nrow(podatki)

silhuete <- rep(0, n) # Ko je vsaj podatek v svoji skupini, je silhueta enaka 0 -> zadnji člen
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



# Razberemo za kateri k je vrednost silhuete največja
k <- which.max(silhuete)
s <- silhuete[k]

k + s
