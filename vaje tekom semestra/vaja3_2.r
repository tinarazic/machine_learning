options(encoding = "UTF-8")
library(caret)
data(iris)

set.seed(12)
kji = c(1:30)
trainCtrl = trainControl(method='boot', savePredictions=TRUE, number=10);
tuneGrd = data.frame(k=kji);

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
    
    # Za vsak primer in k:
    tmp = aggregate(model$pred$pred==model$pred$obs,
                    list(model$pred$k, model$pred$rowIndex),
                    mean)
    # Za vsak k:
    ERR_BS1 = aggregate(tmp$x, list(tmp$Group.1), mean)
    return(ERR_BS1[, 2])
}

bs1_data_table = function(model){
    # Alternativa funkciji aggregate je uporaba paketa data.table
    # Prednost uporabe tega paketa se pokaže predvsem ko želimo iz podatkov izluščiti več
    # kot le povprečno točnost, saj lahko izračunamo marsikaj kar s stolpci:
    # poglej si https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
    library(data.table)
    
    # izracunajmo točnost in njen standardni odklon po vzorčenjih
    DT = data.table(model$pred)
    tmp = DT[, mean(pred==obs), by=.(k, Resample)]  # pazi na piko
    ERR_BS_datatable = tmp[, .(mean(V1), sd(V1)), by=.(k)]
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
    # formula: predpostavimo, da je ciljna spremenljivka zadnja
    f = as.formula(sprintf("%s ~ .", colnames(podatki)[m]))
    knnModelTrain = train(f, data=podatki, method='knn',
                         tuneGrid=data.frame(k=kji), trControl=trainc)
    ERR_train = knnModelTrain$results[,"Accuracy"]
    ERR_BS632 = exp(-1) * ERR_train + (1-exp(-1)) * bs1
    return(ERR_BS632)
}


eOsnovno = bs_osnovno(model)
eOsnovnoDT = bs1_data_table(model)
e1 = bs1(model)
e632 = bs632(iris, kji, e1)

mini = min(eOsnovno, e1, e632) * 0.95
maxi = max(eOsnovno, e1, e632) * 1.05

plot(kji, eOsnovno, col="red", type="o", ylim=c(mini, maxi), xlab = "k", ylab = "točnost")
lines(kji, e1, col="blue", type="o")
lines(kji, e632, col="green", type="o")

legend(x = "topright",
       legend = c("eOsnovno", "e1", "e632"),
       col = c("red", "blue", "green"), lty = 1, lwd = 1)

####################################
# še regresijski del
####################################

bs_osnovno_reg = function(model){
    tmp = aggregate((model$pred$pred - model$pred$obs)^2,
                    list(model$pred$Resample),
                    mean)
    ERR_BS = sqrt(mean(tmp$x))
}

bs1_reg = function(model){
    tmp = aggregate((model$pred$pred - model$pred$obs)^2,
                    list(model$pred$rowIndex),
                    mean)
    ERR_BS1 = sqrt(mean(tmp$x))
}

bs632_reg = function(podatki, kji, bs1){
    # P(primer ni izbran pri zankanju) ---> 1 / e (= 0.368)
    # P(primer izbran) ---> 1 - 1 / e (= 0.632)
    n_m = dim(podatki)
    n = n_m[1]
    m = n_m[2]
    trainc = trainControl(method='cv',
                          index = list(fold1=1:n),
                          indexOut = list(fold1=1:n))
    f = as.formula(sprintf("%s ~ .", colnames(podatki)[m]))
    knnModelTrain = train(f, data=podatki, method='lm', trControl=trainc)
    ERR_train = knnModelTrain$results[,"RMSE"]
    ERR_BS632 = exp(-1) * ERR_train + (1-exp(-1)) * bs1
    return(ERR_BS632)
}


source("pomozno.R")
podatki = narediPodatke(20000, 5)
dodatni = 5000:20000
podatkiNovi = podatki[dodatni,]
podatki = podatki[-dodatni,]

trainCtrl = trainControl(method='boot', savePredictions=TRUE, number=10)
model = train(y ~ ., data = podatki, method='lm', trControl=trainCtrl)


eOsnovno = bs_osnovno_reg(model)
e1 = bs1_reg(model)
e632 = bs632_reg(podatki, kji, e1)

e_prava = RMSE(predict(model, newdata = podatkiNovi), podatkiNovi$y)
print(sprintf("eOsnovno, e1, e632: %.5f %.5f %.5f", eOsnovno, e1, e632))
napake = c("osnovna", "bs1", "bs632")
najblizja = which.min(abs(e_prava - c(eOsnovno, e1, e632)))
print(sprintf("Najbližje pravi napaki (%.5f) je %s", e_prava, napake[najblizja]))
