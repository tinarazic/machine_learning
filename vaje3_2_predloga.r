library(caret)
data(iris)

kji = c(1:30)
trainCtrl = trainControl(method='boot', savePredictions=TRUE, number=4);
tuneGrd = data.frame(k=kji);

# savePredictions=TRUE shrsni vsepredikcije
# dobimo napovedi, za tiste ki niso bili vključeni v učenje modela 
model = train(Species~., data=iris, method='knn', trControl=trainCtrl, tuneGrid=tuneGrd)

# Vsi potrebni podatki so na voljo v model$pred,
# kjer imamo za vsak korak vzorčenja podane napovedi vsakega modela
# na vseh podatkih, ki niso bili izbrani v učno množico


############
# BS
############

bs_osnovno = function(model){
    # ERR_BS = povrecje po vzorcenjih(napaka na primerih, ki niso bili izbrani)
    # To moramo izracunati za vsak k ...
    # Izracunamo najprej povprecje pri danem k in vzorcenju:
    tmp = aggregate(model$pred$pred==model$pred$obs,
                    list(model$pred$k, model$pred$Resample),
                    mean)
    # ERR_BS za dani k (ki je postal Group.1) dobimo s povprecenjem novonastalega stolpca x:
    ERR_BS = aggregate(tmp$x, list(tmp$Group.1), mean)    
    # Če želimo še standardno deviacijo:
    # ERR_BS = aggregate(tmp$x, list(tmp$Group.1), function(x){c(mean(x), sd(x))})
    # ERR_BS = data.frame(as.matrix(ERR_BS))
    return(ERR_BS[, 2])
}

bs1 = function(model){
    # ERR_BS1 =
    # 1) za vsak primer izračunaj povprečno napako tistih modelov,
    #    ki niso uporabili tega primera pri učenju
    # 2) izračunaj povprečje vrednosti, ki jih dobiš pri 1)
    

    return(ERR_BS1[, 2])
}


bs632 = function(podatki, kji, bs1){
    # P(primer ni izbran pri zankanju) ---> 1 / e (= 0.368)
    # P(primer izbran) ---> 1 - 1 / e (= 0.632)
    podatki = iris
    n_m = dim(podatki)
    n = n_m[1]
    m = n_m[2]
    trainc = trainControl(method='cv',
                          index = list(fold1=1:n),
                          indexOut = list(fold1=1:n))  # trik, kako do vseh rezultatov: 1 fold  ...
    
    return(ERR_BS632)
}

