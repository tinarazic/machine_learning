library(caret)
RNGkind(sample.kind = "Rejection")
set.seed(12321)


trenutniCas = function(){
    as.numeric(as.POSIXct(Sys.time()))
}



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
#      - možni vrednosti za y sta "0" in "1": y je "1" natanko tedaj, ko je prva koordinata
#        pripadajočega središča pozitivna
# - sicer:
#      - možne vrednosti za y označimo npr. z "00", "01", "10" in "11"; natanko primeri iz i-te krogle
#        imajo vrednost y_i.
#
#
# DN za slavo in čast:
# - kolikšen je največji polmer r, pri katerem so podatki še linearno ločljivi?
# - posploši funkcijo na m-dimenzionalne podatke (zgoraj je m = 2): tam imamo 2^m krogel in razredov 
###########################################################################################################

narediPodatke = function(n, m, r, jeDvojiska, stopnjaSuma){
    xs = matrix(nrow = n, ncol = m)
    # hranimo še izbrana središča
    sredisca = matrix(nrow = n, ncol = m)
    # definiramo xs
    for (i in 1:n){
        # izberi sredisce
        sredisce = sign(runif(m) - 0.5)
        # izberi tocko v krogli (to je za visoke dimenzije malo potratno):
        # 1) najdi tocko v hiperkocki [-r, r]^m
        # 2) preveri, ali je njena norma <= r
        # 3) ce je, ustrezno premaknemo
        while(TRUE){
            primer = 2 * r * (runif(m) - 0.5)
            if (norm(primer, type="2") <= r){
                xs[i, ] = sredisce + primer
                sredisca[i, ] = sredisce
                break
            }
        }
    }
    # definiramo še y
    y = rep("0", n)
    if (jeDvojiska){
        y[sredisca[,1] > 0] = "1"
        for (i in 1:n){
            # dodamo šum, če je treba
            if (runif(1) < stopnjaSuma){
                if (y[i] == "0"){
                    y[i] = "1"
                } else {
                    y[i] = "0"
                }
            }
        }
        y = as.factor(y)
    } else {
        M = 2^m
        # če m poznamo, npr. m = 2, potem lahko preprosto napišemo
        # if (sredisca[i, 1] > 0){
        #   if (sredisca[i, 2] > 0){
        #     y[i] = "11"
        #   } else {
        #     y[i] = "10"
        #   }
        # } else {
        #   if (sredisca[i, 2] > 0){
        #     y[i] = "01"
        #   } else {
        #     y[i] = "00"
        #   }
        # V sploščnem lahko enostavno zlepimo predznake v niz
        for (i in 1:n){
            razred = sredisca[i, ]  # -1 ali 1
            # malo suma za naslednje naloge
            if (runif(1) < stopnjaSuma){
                drugi = n + 1 - i 
                # morda je pripadajoče središče isto, ampak
                # - večinoma ni
                # - lahko izračunamo verjetnost neuspeha in ustrezno popravimo sum na začetku
                razred = sredisca[drugi, ]
            }
            razred[razred < 0] = 0
            y[i] = paste(as.character(razred), collapse = "")
        }
    }
    y = as.factor(y)
    podatki = data.frame(xs)
    podatki$y = y
    imenaStolpcev = lapply(1:(m + 1), function(i){sprintf("x%d", i)})
    imenaStolpcev[m + 1] = "y"
    colnames(podatki) = imenaStolpcev
    return(podatki)
}


###########################################################################################################
# 2. Z uporabo funkcije narediPodatke ustvari linearno ločljive podatke s 1000 primeri, dvema vhodnima
# spremenljivkama in dvojiškim razredom y. Na njih poženi metodo podpornih vektorjev. Ustrezna vrednost
# argumenta method je 'svmLinear'.
# Ker so podatki linearno ločljivi, optimizacija parametra C in potrebna, zato lahko uporabiš
# tuneGrid=data.frame(C=Inf) in trControl=trainControl(method='none').
###########################################################################################################
podatki1 = narediPodatke(1000, 2, 0.5, TRUE, 0)
t0 = trenutniCas()
model1 = train(y ~ ., 
               data=podatki1,
               method='svmLinear',
               tuneGrid=data.frame(C=Inf),
               trControl=trainControl(method='none'))
t1 = trenutniCas()
print(sprintf("Za model1 smo potrebovali %.2f sekund", t1 - t0))
###########################################################################################################
# 3. Narišite nivojnice SVM modela: zelo primerna je knjižnica kernlab, ker jih zna narisati
# za splošna jedra. Klic plot(svmModel, data=podatki) zadošča! Pazite le na to, da podatke le vhodni
# del podatkov in to kot matriko (as.matrix(...)).
###########################################################################################################
library(kernlab)
t0 = trenutniCas()
plot(model1$finalModel, data=as.matrix(podatki1[names(podatki1)!='y']))
t1 = trenutniCas()
print(sprintf("Za napovedovanje (risanje) pa: %.2f", t1 - t0))

###########################################################################################################
# 4. Zapustimo laboratorijske pogoje in naredimo podatke linearno neločljive.
# To dosežeš bodisi z dodajanjem šuma (naključno spremeni vrednost y nekaterim primerov) ali pa
# prekomerno povečaj polmer krogel. Vklopi optimizacijo parametra C!
# Nariši nivojnice.
###########################################################################################################
cji = c(0.01, 0.1, 1, 10.0, 100.0, 1000.0, 1000000.0)

narediNarisi = function(p, imeModela){
    t0 = trenutniCas()
    model = train(y ~ ., 
                  data=p,
                  method='svmLinear',
                  tuneGrid = data.frame(C = cji),
                  trControl=trainControl(method='cv'))
    t1 = trenutniCas()
    plot(model$finalModel, data=as.matrix(p[names(p)!='y']))
    t2 = trenutniCas()
    print(sprintf("%s: gradnja: %.2f, risanje (napovedi): %.2f [sekunde]", imeModela, t1 - t0, t2 - t1))
    return(model)
}

# prevelik polmer
podatki2 = narediPodatke(1000, 2, 1.5, TRUE, 0)
model2 = narediNarisi(podatki2, "model2")


# dodajmo šum
podatki3 = narediPodatke(1000, 2, 0.5, TRUE, 0.2)
model3 = narediNarisi(podatki3, "model3")

# oboje
podatki4 = narediPodatke(1000, 2, 1.5, TRUE, 0.1)
model4 = narediNarisi(podatki4, "model4")

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
    q = (b / a) ** (1 / (n - 1))
    stopnje = 0:(n - 1)
    kandidati = a * q ** stopnje
    return(list(kandidati, q))
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

lepsiPrikazKandidatov = function(kandidati){
    odgovor = lapply(kandidati, function(kandidat){sprintf("%.3e", kandidat)})
    odgovor = paste(odgovor, collapse = ", ")
}

titanic = dobiTitanic()
particija = createFolds(titanic$survived, k = 10, returnTrain=TRUE)
n = 8
odgovor = geometrijskoZaporedje(0.001, 1000.0, n)
kandidati = odgovor[[1]]
q = odgovor[[2]]
toleranca = 1.01
optimalnaTocnost = 0.0
optimalniC = -1
t0 = trenutniCas()
while (kandidati[n] / kandidati[1] > toleranca){
    print(sprintf("Trenutni kandidati:    %s", lepsiPrikazKandidatov(kandidati)))
    # naučimo se
    modelT = train(survived ~ ., 
                   data=titanic,
                   method='svmLinear',
                   tuneGrid = data.frame(C = kandidati),
                   trControl=trainControl(method='cv', index=particija))
    print(sprintf("Pripadajoče točnosti:  %s", lepsiPrikazKandidatov(modelT$results[["Accuracy"]])))
    # najdimo opt vrednost med trenutnimi
    iOpt = which.max(modelT$results[["Accuracy"]])
    tocnost = modelT$results[["Accuracy"]][iOpt]
    C = modelT$results[["C"]][iOpt]
    if (tocnost > optimalnaTocnost){
        optimalniC = C
        optimalnaTocnost = tocnost
        print(sprintf("Novi optimalni C (in točnost): %.12f (%.6f)", optimalniC, optimalnaTocnost))
    }
    # izračunamo naslednje kandidate
    odgovor = geometrijskoZaporedje(optimalniC / q, optimalniC * q, n)
    kandidati = odgovor[[1]]
    q = odgovor[[2]]
}
t1 = trenutniCas()
print(sprintf("Končni C (in točnost): %.12f (%.6f)", optimalniC, optimalnaTocnost))
print(sprintf("Potreben čas: %.2f sekund", t1 - t0))
