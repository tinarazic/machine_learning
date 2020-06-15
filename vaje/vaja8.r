library(caret)
library(mxnet)
library(mlbench)

current_working_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

source('plotAreas.R')
# Opozorilo: če boste pognali datoteko s Source, potem se grafi ne prikažejo
# Lahko ali a) vse označite in pritisnete Run, ali b) spremenite plotAreas(...) v
# p = plotAreas(...)
# plot(p)

linearnoLocljivi = function(n, seme = 123, sd = 1.0){
    # Naredimo linearno ločljive podatke, ki so razdeljeni na
    # - pozitivni (y = 1) in
    # - negativni del (y = -1)
    # Obe vhodni spremenljivki sta porazdeljeni neodvisno in normalno N(2y, sd)
    # Če se zgodi, da niso linearno ločljivi, zamenjaj seme ali zmanjšaj sd.
    pozitivni = round(n / 2)
    negativni = n - pozitivni
    set.seed(seme)
    x1 = c(rnorm(pozitivni, mean=2), rnorm(negativni, mean=-2))
    x2 = c(rnorm(pozitivni, mean=2), rnorm(negativni, mean=-2))
    y = c(rep(TRUE, pozitivni), rep(FALSE, negativni))
    y = as.factor(as.logical(y))
    return(data.frame(x1=x1, x2=x2, y=y))
}

radialnoLocljivi = function(n, seme = 123, sd = 0.3){
    # Naredimo nekaj takega:
    #
    #       - - - -
    #     -        -
    #    -  + + +   -
    #   -  +     +  -
    #    -  + + +  -
    #     -       -
    #      - - - -
    # 
    pozitivni = round(n / 2)
    negativni = n - pozitivni
    sd = 0.3
    # polmeri + in - primerov so v povprečju 2 in 4
    r = c(rnorm(pozitivni, mean=2, sd=sd), rnorm(negativni, mean=4, sd=sd))
    # primeri so enakomerno porazdeljeni v vseh smereh
    theta = 2 * pi * runif(n)
    # kartezične
    x1 = r * cos(theta)
    x2 = r * sin(theta)
    y = c(rep(TRUE, pozitivni), rep(FALSE, negativni))
    y = as.factor(as.logical(y))
    return(data.frame(x1 = x1, x2 = x2, y = y))
}

##################################################################################################
# 1. Na 200 linearno ločljivih primerih natreniraj mrežo z 2 vhodnima in enim izhodnim nevronom in
#    brez skritih plasti, tj. enostavni perceptron. Uporabi reLU aktivacijo. Uporabiš lahko
#    funkcijo train s paremtrom method='mxnet'. Mreži lahko podaš naslednje parametre:
#    - layer1, layer2 in layer3: števila nevronov v 1., 2. in 3. skritem sloju
#    - learning.rate in momentum, ki skupaj določata posodobitveno pravilo uteži:
#      D_t = momentum * D_{t - 1} + (1 - momentum) * L'(W, x, y)
#      W   = W  - learning.rate D_t
#      kjer je t indeks iteracije in L'(W, x, y) diferencial funkcije izgube (loss function)
#      glede za trenutne uteži W in podatke x, y.
#    - activation: ime aktivacijske funkcije (npr. 'tanh', 'relu' ipd.)
#
#    Kako narediti mrežo z npr. 4 skritimi sloji, se bomo naučili kasneje.
##################################################################################################
podatki1 = linearnoLocljivi(200)
nnModel = train(y ~ ., data = podatki1,
                method='mxnet',
                tuneGrid=expand.grid(layer1=0, layer2=0, layer3=0,
                                     learning.rate=0.1, momentum=0, dropout=0,
                                     activation='relu'),
                trControl = trainControl(method = "none")  # trainControl('cv', number=10)
                )

##################################################################################################
# 2. Na istih podatkih se nauči še linearni SVM-model.
#    - V čem sta si odločitveni meji podobni?
#    - V čem se razlikujeta?
#
#   Uporabiš lahko funkcijo plotAreas(model, podatki). Za hitrejše (a manj natančno risanje lahko
#   ustrezno zmanjšate vrednost argumenta gridNum).
##################################################################################################
svmModel = train(y ~ ., data = podatki1,
                 method='svmLinear',
                 tuneGrid=expand.grid(C=Inf),
                 trControl = trainControl(method = "none")  # trainControl('cv', number=10))
                 )
source('plotAreas.R')
plotAreas(nnModel, podatki1, naslov = "Mreža")
plotAreas(svmModel, podatki1, naslov = "SVM")

##################################################################################################
# 3. Ponovi točki 1 in 2 na radialno ločljivih podatkih, s tem da
#    - za SVM namesto linearnih uporabi kakšna bolj smiselna jedra,
#    - se pri mrežah poigraj s širino skritih slojev. Poizkusi z 0 in čim večjim.
##################################################################################################
podatki2 = radialnoLocljivi(200)

radialniNNModel = function(sirina){
    nnModel = train(y ~ ., data = podatki2,
                    method='mxnet',
                    tuneGrid=expand.grid(layer1=sirina, layer2=sirina, layer3=sirina,
                                         learning.rate=0.1, momentum=1, dropout=0,
                                         activation='relu'),
                    trControl = trainControl(method = "none") # trainControl('cv', number=10)
                    )
}
nn0 = radialniNNModel(0)
nn5 = radialniNNModel(5)
nn10 = radialniNNModel(10)
nn50 = radialniNNModel(50)
nn100 = radialniNNModel(100)
plotAreas(nn0, podatki2, gridNum=250, naslov = "0 skritih nevronov")
plotAreas(nn5, podatki2, gridNum=250, naslov = "5 skritih nevronov")
plotAreas(nn10, podatki2, gridNum=250, naslov = "10 skritih nevronov")
plotAreas(nn50, podatki2, gridNum=250, naslov = "50 skritih nevronov")
plotAreas(nn100, podatki2, gridNum=250, naslov = "100 skritih nevronov")
svmModel2 = train(y ~ ., data = podatki2,
                  method='svmRadial',
                  trControl = trainControl(method = "none")  # trainControl('cv', number=10))
)
plotAreas(svmModel2, podatki2, gridNum = 250, naslov = "svm rad")


##################################################################################################
# Kako sestaviti mrežo 'na roke'? Knjižnica mx nam ponuja kar nekaj gradnikov. Danes bomo spoznali
# - polnopovezane sloje
# - aktivacijske funkcije
# - soft-max
#
# Pri gradnji se moramo zavedati sledečega: Vse je nov sloj:
# - dodaj polnopovezan sloj = dodaj nov sloj
# - definiraj aktivacijsko funkcijo = dodaj nov sloj
# - uporabi soft-max = dodaj nov sloj
# ...
# Razen pri vhodnem zato definiramo noviSloj(prejšnji, parametri)
#
# Kot model funkciji za učenje podamo kar zadnji sloj.
##################################################################################################
# vhodni sloj
data = mx.symbol.Variable("data")
n_hidden = 100
# Naslednji sloj (skriti): polnopovezan, 10 nevronov, tanh aktivacija
fc1 = mx.symbol.FullyConnected(data, num_hidden=n_hidden)  # polnopovezan sloj
act1 = mx.symbol.Activation(fc1, act.type='tanh')    # aktivacijski "sloj"
# Naslednji sloj (skriti): polnopovezan, 10 nevronov, tanh aktivacija
fc2 = mx.symbol.FullyConnected(act1, num_hidden=n_hidden)   # polnopovezan sloj
act2 = mx.symbol.Activation(fc2, act.type='relu')    # aktivacijski "sloj"
# Naslednji sloj (skriti): polnopovezan, 10 nevronov, tanh aktivacija
fc3 = mx.symbol.FullyConnected(act2, num_hidden=n_hidden)   # polnopovezan sloj
act3 = mx.symbol.Activation(fc3, act.type='relu')    # aktivacijski "sloj"

# Naslednji sloj (izhodni): polnopovezan, dva nevrona
fc4 = mx.symbol.FullyConnected(act3, num_hidden=2)   # polnopovezan sloj
sfo = mx.symbol.SoftmaxOutput(fc4)                   # soft-max "sloj"

# Podatke je treba podati kot matrike
podatki3 = radialnoLocljivi(200)
trainIndex = createDataPartition(podatki3$y, p=0.8, list=FALSE)
podatki3$y = as.numeric(podatki3$y) - 1
trainX = data.matrix(podatki3[trainIndex, 1:2])
trainY = podatki3$y[trainIndex]
testX = data.matrix(podatki3[-trainIndex, 1:2])
testY = podatki3$y[-trainIndex]

nnModelNaRoke = mx.model.FeedForward.create(
    sfo, X=trainX, y=trainY,
    eval.data=list(data=testX, label=testY),
    ctx=mx.cpu(), num.round=300, array.batch.size=30,
    learning.rate=0.1, momentum=0.9,
    eval.metric = mx.metric.accuracy
)

napovedi = predict(nnModelNaRoke, testX)
# dodatno podamo še Y, saj ga 2. argument ne vsebuje
plotAreas(nnModelNaRoke, testX, naslov ="Mreža na roke", Y = testY)
##################################################################################################
# 4. Na podatkih Sonar natreniraj mrežo z enim skritim slojem tako, da
#    - zahtevaš, da gredo podatki skoznjo vsaj 10x
#    - če se v petih zaporendih iteracijah napaka ne izboljša vsaj za min.diff,
#      se treniranje ustavi
#    - po 200 iteracijah se treniranje ustavi.
#    Pri implementaciji zgonjega ustavljanja bo koristen argument epoch.end.callback funkcije
#    mx.model.FeedForward.create. Funkcija, podana tu, se pokliče po koncu vsake iteracije.
#    Delno je že implementirana.
##################################################################################################
data(Sonar)
Sonar$Class = as.numeric(Sonar$Class)-1

# To je funkcija, ki bo vrnila ustrezno funkcijo
mx.callback.early.stop = function(min.steps=NULL, min.diff=NULL, bad.steps=NULL) {
    function(iteration, nbatch, env, verbose) {
        # v spremenljivko lahko kaj shraniš, npr. število "slabih korakov"
        # Funkcija naj vrne TRUE <=> lahko še nadaljujemo
        if (!is.null(env$metric)) {
            if (!is.null(min.steps) && !is.null(min.diff) &&!is.null(bad.steps)) {
                result <- env$metric$get(env$train.metric)
                # za izpis
                cat(paste(result$value, '\n', env$previous.result, '\n', iteration, '\n'))
                # ustrezen pogoj
                if (iteration > min.steps && result$value - env$previous.result < min.diff) {
                    env$bad.steps <- env$bad.steps + 1
                    if (env$bad.steps >= bad.steps){
                        return(FALSE)
                    }
                }
                else{
                    env$bad.steps <- 0
                }
                env$previous.result <- result$value
            }
        }
        return(TRUE)
    }
}




mx.set.seed(0)

data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, num_hidden=10)
act1 <- mx.symbol.Activation(fc1, act.type='tanh')
fc2 <- mx.symbol.FullyConnected(act1, num_hidden=2)
sfo <- mx.symbol.SoftmaxOutput(fc2)
mx.set.seed(0)

trainIndex <- createDataPartition(Sonar$Class, p=0.8, list=FALSE)
# trainIndex <- c(1:50, 100:150)
trainX <- data.matrix(Sonar[trainIndex, names(Sonar)!='Class'])
trainY <- Sonar$Class[trainIndex]
testX <- data.matrix(Sonar[-trainIndex, names(Sonar)!='Class'])
testY <- Sonar$Class[-trainIndex]

model <- mx.model.FeedForward.create(
    sfo, X=trainX, y=trainY,
    eval.data=list(data=testX, label=testY),
    ctx=mx.cpu(), num.round=200, array.batch.size=41,
    learning.rate=0.07, momentum=0.9,
    # batch.end.callback = mx.callback.log.train.metric(2),
    epoch.end.callback = mx.callback.early.stop(min.steps = 10, min.diff = 0.001, bad.steps = 5),
    eval.metric = mx.metric.accuracy
)

predictions = predict(model, testX)
tocnost = sum((max.col(t(predictions)) - 1)==testY) / length(testY)

data(Sonar)
svmModel = train(trainX, as.factor(trainY), method='svmLinear', tuneGrid=data.frame(C=10^(-5:5)))
predictions2 = predict(svmModel, testX)
tocnostSVM = sum(predictions2 == testY) / length(testY)
print(sprintf("Točnosti: linearni podporni vektorj: %.3f; mreža: %.3f", tocnostSVM, tocnost))
