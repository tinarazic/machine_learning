library(caret)
RNGkind(sample.kind = "Rejection")
set.seed(12321)


##########################################################################################################
# 1. Implementirajmo funkcijo narediPodatke(n, r, jeDvojiska), ki jo bomo kasneje uporabljali za
# ustvarjanje podaktovnih množic, primernih za osnovno različico metode podpornih vektorjev. Funkcija
# naj sprejme
# - število primerov n,
# - polmer r in 
# - logično vrednost jeDvojiska.
#
# Primeri (brez ciljne spremenljivke) živijo v R^2. Razdelili jih bomo v 4 krogle s središči 
# (+-1, +-1) in polmerom r.
# Generiramo jih tako:
# - naključno izberemo eno od središč,
# - naključno izberemo točko v pripadajoči krogli.
#
# Pomoč: točko v izbrani krogli K(s, r) s središčem v s dobiš tako, da najdeš točko v K(0, r)
# in jo premakneš za vektor s.
#
# Ciljna spremenljivka y je nominalna spremenljivka in naslednje vrednosti:
# - če jeDvojiska:
#      - možni vrednosti za y sta "0" in "1": y je "1" natanko tedaj, ko je x1 > 0
# - sicer:
#      - možne vrednosti za y označimo npr. z "00", "01", "10" in "11"; natanko primeri iz i-te krogle
#        imajo vrednost y_i.
#
#
# DN za slavo in čast:
# - kolikšen je največji polmer r, pri katerem so podatki še linearno ločljivi?
# - posploši funkcijo na m-dimenzionalne podatke (zgoraj je m = 2): tam imamo 2^m krogel in razredov 
###########################################################################################################

narediPodatke = function(n, m, r, jeDvojiska){
  x1 <- c()
  x2 <- c()
  y_data <- c()
  x <- c(1,-1)
  y <- c(1,-1)
  for (i in 1:n){
    x_sredisce = sample(x, 1)
    y_sredisce = sample(y, 1)
    t = 2*pi*runif(1)
    x1 <-c(x1,r*cos(t))
    x2 <- c(x2, r*sin(t))
    if (jeDvojiska) {
      if (x1 > 0) {y <- c(y, "1")} else {y <- c(y, "0")}}
    else {
      y <- c(y, sprintf("%s%s", abs(x_sredice), abs(y_sredisce))
    }
  }
  podatki <- cbind(x1, x2, y)
  return(podatki)
}


###########################################################################################################
# 2. Z uporabo funkcije narediPodatke ustvari linearno ločljive podatke s 1000 primeri, dvema vhodnima
# spremenljivkama in dvojiškim razredom y. Na njih poženi metodo podpornih vektorjev. Ustrezna vrednost
# argumenta method je 'svmLinear'.
# Ker so podatki linearno ločljivi, optimizacija parametra C in potrebna, zato lahko uporabiš
# tuneGrid=data.frame(C=Inf) in trControl=trainControl(method='none').
###########################################################################################################
podatki1 = narediPodatke(1000,2,1,TRUE)
model1 = train(podatki1, method = "svmLinear",tuneGrid=data.frame(C=Inf), trControl=trainControl(method='none'))

###########################################################################################################
# 3. Narišite nivojnice SVM modela: zelo primerna je knjižnica kernlab, ker jih zna narisati
# za splošna jedra. Klic plot(svmModel, data=podatki) zadošča! Pazite le na to, da podatke le vhodni
# del podatkov in to kot matriko (as.matrix(...)).
###########################################################################################################
library(kernlab)

plot(NULL, data=NULL)
# K ničla tam kjer je odločitvena meja


###########################################################################################################
# 4. Zapustimo laboratorijske pogoje in naredimo podatke linearno neločljive.
# To dosežeš bodisi z dodajanjem šuma (naključno spremeni vrednost y nekaterim primerov) ali pa
# prekomerno povečaj polmer krogel. Vklopi optimizacijo parametra C!
# Nariši nivojnice.
###########################################################################################################
cji = c(0.01, 0.1, 1, 10.0, 100.0, 1000.0, 1000000.0)



###########################################################################################################
# 5. Poišči optimalno vrednost parametra C na podatkih titanic: funkcija z vaj 4, ki uredi vse potrebno,
# je že podana. Prostor parametrov preišči tako:
#
# n = 6
# kandidati, q = geometrijskoZaporedje(10^-2, 10^2, n)
# dokler max(kandidati) / min(kandidati) > toleranca:
#      najdi optimalni c med kandidati
#      če je pripadajoča točnost največja do zdaj:
#          posodobi do zdaj najboljši c
#      kandidati, q = geometrijskoZaporedje(optimalni/q, optimalni * q, n)
#
# Nekaj opomb:
# - Toleranca mora biti, ker delimo max in min, nekaj nad 1, npr. 1.01
# - Funkcija geometrijskoZaporedje(a, b, n) naj vrne števila
#   a = a * q^0, a * q, a * q^2, ..., b = a * q^(n - 1). Pri a = 0.01, b = 100 in n = 5 tako dobimo
#   kandidati = c(0.01, 0.1, 1.0, 10.0, 100.0) in q = 10.0.
# - Optimalni C in pripadajoča točnost za model = train(...) se skrivata v model$results.
#   Do ustreznega stolpca dostopamo z model$results[["ime"]].
# - Zgornjo psevdokodo lahko še izboljšaš, tako da nobene vrednosti parametra C ne bomo ocenili večkrat.
#   (trenutno se to dogodi v točkah na robu)
# 
###########################################################################################################
geometrijskoZaporedje = function(a, b, n){
    NULL
}


dobiTitanic = function(){
    # ponovimo del vaj 4
    library(earth)
    data(etitanic)
    etitanic$survived[etitanic$survived==1] = "P"
    etitanic$survived[etitanic$survived==0] = "N"
    etitanic$survived = as.factor(etitanic$survived)
    return(etitanic)
}


titanic = dobiTitanic()
particija = createFolds(titanic$survived, k = 10, returnTrain=TRUE)  # uporabi v trainControl
optimalnaTocnost = 0.0
optimalniC = -1



while (kandidati[n] / kandidati[1] > toleranca){
    break
}
print(sprintf("Končni optimalni C (in točnost): %.12f (%.6f)", optimalniC, optimalnaTocnost))
