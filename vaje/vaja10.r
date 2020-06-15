###############################################################################################
# 0. Naključni gozd lahko zgradimo tako, da uporabimo train in method='rf':
#    train(trainX, trainY, method='rf', tuneGrid = data.frame(mtry = c(...)), drugi parametri)
#
#    S caretom lahko torej optimiziramo samo parameter mtry (število spremenljivk, ki jih
#    obravnavamo v vsakem vozlišču). Drugi parametri vključujejo:
#    - ntree: število dreves
#    - nodesize: najmanjša dovoljena velikost lista
#    - maxnodes: največje število listov
#    - importance: ali naj izračunamo kakovosti spremenljivk?
#    - ...
#    Za preostale si lahko ogledate dokumentacijo fukncije randomForest, ki je drugi način za
#    izgradnjo gozda (a brez tuneGrida ipd.)
###############################################################################################

library(caret)
library(randomForest)

n = 5000
m = 9
X = matrix(rnorm(m * n), ncol=m)  # n x m matrika
Y = 1 + X[, 1] - 2 * X[, 2] + (X[, 4] * X[, 5] - X[, 6] + X[, 9]) / 3 + rnorm(n)

plus = Y > mean(Y)
Y[plus] = 1
Y[!plus] = 0
Y = as.factor(Y)

data = data.frame(X, Y)
trainX = data[, names(data)!='Y']
trainY = data$Y


rfModel = train(trainX, trainY,
                method='rf',
                trControl = trainControl(method='cv', number=10),
                tuneGrid = data.frame(mtry = c(2, 4)),
                ntree = 10
)



####################################################################################################
# 1. Na zgornjih podatkih (nekaj spremenljivk je očitno povsem nepomembnih)
#    poženi metodo naključnih gozdov. Nariši graf, ki prikazuje, kako se učna, testna in OOB točnost
#    metode spreminjajo v odvisnosti od števila spremenljivk v drevesih gozda.
#    Katera vrednost mtry je najboljša? Oceni jo z 10-kratnim prečnim preverjanjem.
#
#    Testno točnost najdete v rfModel$results, učno morate izračunati, OOB pa lahko hitro dobite
#    iz rfModel$finalModel$err.rate
####################################################################################################
tc = trainControl(method='cv', number=10, index=createFolds(data$Y))
tocnosti = data.frame(n=1:m, testna=rep(0, m), OOB=rep(0, m), ucna=rep(0, m))
for(mtry in 1:m){
    print(mtry)
    rfModel = train(trainX, trainY, method='rf',
                    tuneGrid = data.frame(mtry=mtry),
                    trControl = tc,
                    ntree=1000)  # da skonvergira
    tocnosti$testna[mtry] = rfModel$results$Accuracy
    tocnosti$ucna[mtry] = mean(predict(rfModel, trainX) == trainY)
    # i-ta vrstica za i dreves zadnja vrstica
    tocnosti$OOB[mtry] = 1 - rfModel$finalModel$err.rate[1000, 1]
}

plot(tocnosti$n, tocnosti$testna, type='l', col='red', ylim=c(0.5, 1.05),
     xlab="mtry", ylab="točnost")
lines(tocnosti$OOB, col='blue')
lines(tocnosti$ucna, col='black')
legend("bottomright", inset = 0.1,
       legend=c("testna", "OOB", "učna"),
       col=c("red", "blue", "black"), lty=1)


###################################################################################################
# 2. Jasno je: več dreves vodi k boljšim rezultatom. Nariši graf, ki pokaže, kako se
#    zgornje napake spreminjajo s številom dreves v gozdu. Po koliko drevesih začne kakovost
#    gozda "stagnirati"? Narišite točnosti vseh modelov, ko gre število dreves od 2 do 1000.
#
#    Da se izognete tisočim klicem predict(model, učna), naredite le en model iz tisoč dreves
#    s klicem rfModel = randomForest(trainX, trainY, ntree=1000, drugi parametri) in nato
#    povprečite napovedi posameznih modelov, ki jih dobite s klicem
#    predict(rfModel, trainX, predict.all=TRUE).
###################################################################################################

ucniIndeksi = createDataPartition(data$Y, p=0.8, list=FALSE)
trainX = data[ucniIndeksi, names(data)!='Y']
trainY = data$Y[ucniIndeksi]
testX = data[-ucniIndeksi, names(data)!='Y']
testY = data$Y[-ucniIndeksi]

ntree = 1000
rfModel = randomForest(trainX, trainY, importance=TRUE, xtest=testX, 
                        ytest = testY, keep.forest=TRUE, keep.inbag=TRUE, ntree=ntree)


tocnosti = data.frame(n=1:ntree, testna=rep(0, ntree), OOB=rep(0, ntree), ucna=rep(0, ntree))

tocnosti$testna = 1 - rfModel$test$err.rate[,'Test']
tocnosti$OOB = 1 - rfModel$err.rate[, 'OOB']

napovedi = predict(rfModel, trainX, predict.all=TRUE)
delneVsote = rep(0, length(ucniIndeksi))
for(i in 1:ntree){
    delneVsote = delneVsote + as.numeric(napovedi$individual[, i] == '1')
    agregirane = rep('0', length(ucniIndeksi))
    agregirane[delneVsote / i > 0.5] = '1'
    tocnosti$ucna[i] = mean(agregirane == trainY)
}

plot(tocnosti$n, tocnosti$testna, type='l', col='red', ylim=c(0.5, 1.05),
     xlab="število dreves", ylab="točnost")
lines(tocnosti$OOB, col='blue')
lines(tocnosti$ucna, col='black')
legend("bottomright", inset = 0.1,
       legend=c("testna", "OOB", "učna"),
       col=c("red", "blue", "black"), lty=1)



###################################################################################################
# 3.Nariši graf, ki prikazuje pomembnost posameznih spremenljivk.
#   Katere spremenljivke metoda naključnih gozdov oceni najslabše?
#   Katere bi morala oceniti najslabše?
#
#   Pomagaš si lahko s funkcijo varImpPlot(rfModel).
###################################################################################################
varImpPlot(rfModel)  # točnost, Gini = 1 - \sum_i p^2(y_i)

# x < fi:  Hevristika(x, fi) = Gini(D) - (|D_l|/|D| Gini(D_l) + |D_d| / |D| Gini(D_d))

###################################################################################################
# 4. Preiskovanje dreves. Če dobimo rfModel = randomForest(...), lahko posamezna drevesa dobimo
#    s klicem drevo =  getTree(rfModel, indeks). Drevesa so izrazito rekurzivna struktura, zato
#    se jih bomo naučili preiskovati na primeru naslednjega računanja kakovosti spremenljivk:
#
#    Kakovost spremenljivke x v danem notranjem vozlišču je enaka
#    - 0, če se x ne pojavi v testu tega vozlišča
#    - številu primerov iz učne množice, ki dosežejo to vozlišče.
#
#    Kakovost spremenljivke x za dano drevo je enaka vsoti vozliščnih kakovosti x,
#    ki teče po notranjih vozliščih drevesa.
#
#    Kakovost spremenljivke x za gozd je enaka vsoti drevesnih kakovosti x.
#
#    Ko to izračunamo za vse x, rezultate še normaliziramo, tako da kakovosti delimo z
#    največjo med njimi.
#
#    Oglej si, kako je videti getTree(rfModel, indeks) in napiši funkcijo
#    kakovosti(rfModel, učni podatki), ki izračuna kakovosti po zgornji definiciji.
#    Nekaj misli:
#    - upoštevaj, so testi v drevesih oblike x <= prag
#    - najbrž se bo treba rekurzivno spustiti po drevesu in spotoma voditi število primerov
#      (oz. kar primere same), ki dospejo do danega vozlišča.
##################################################################################################

ntree = 1000
rfModel = randomForest(trainX, trainY, importance=TRUE, xtest=testX, 
                        ytest = testY, keep.forest=TRUE, keep.inbag=TRUE, ntree=ntree)


kvaliteteZaDrevo = function(tree, data, dataCounts, vozlisce, kvaliteteSpremenljivk){
    if(tree[vozlisce, 'status'] == 1){
        # če je notranje

        splitVar = tree[vozlisce, 'split var']
        kvaliteteSpremenljivk[splitVar] = kvaliteteSpremenljivk[splitVar] + sum(dataCounts)
        
        splitPoint = tree[vozlisce, 'split point']
        leftDaughter = tree[vozlisce, 'left daughter']
        rightDaughter = tree[vozlisce, 'right daughter']
        # testi so oblike x < prag
        
        # gremo v levo
        dataCountsLeft = dataCounts
        dataCountsLeft[data[, splitVar] > splitPoint] = 0
        kvaliteteSpremenljivk = kvaliteteZaDrevo(tree, data, dataCountsLeft, leftDaughter, kvaliteteSpremenljivk)
        # gremo v desno
        dataCountsRight = dataCounts
        dataCountsRight[data[, splitVar] < splitPoint] = 0
        kvaliteteSpremenljivk = kvaliteteZaDrevo(tree, data, dataCountsRight, rightDaughter, kvaliteteSpremenljivk)
    }
    kvaliteteSpremenljivk
}


# najprej za vsako drevo posebej
kvalitete = rep(0, m)
for(i in 1:ntree){
    tree = getTree(rfModel, i)
    kvaliteteDrevo = rep(0, m)
    kvaliteteDrevo = kvaliteteZaDrevo(tree, data[ucniIndeksi, ], rfModel$inbag[, i], 1, kvaliteteDrevo)
    kvalitete = kvalitete + kvaliteteDrevo
}
kvalitete = kvalitete / max(kvalitete)
dim(kvalitete) = c(length(kvalitete), 1)
rownames(kvalitete) = lapply(1:9, function(i){sprintf("x%d", i)})
vrstniRed = order(kvalitete, decreasing = TRUE)
print(kvalitete[vrstniRed,])

# Druge kvalitete še pridejo ...