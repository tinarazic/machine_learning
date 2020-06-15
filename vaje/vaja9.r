library(caret)
library(mxnet)

current_working_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

source("preberiUtezi.R")

sprotniRisalec = function(iteration, nbatch, env, verbose) {
    shraniFilter(env$model, filterDatoteka)
    prikaziFilter(filterDatoteka)
    return(TRUE)
}



# s prejšnjih vaj, a nekoliko drugačna
mx.callback.early.stop = function(min.steps=NULL, min.diff=NULL, bad.steps=NULL) {
    function(iteration, nbatch, env, verbose) {
        shraniFilter(env$model, filterDatoteka)
        prikaziFilter(filterDatoteka)
        if (!is.null(env$metric)) {
            if (!is.null(min.steps) && !is.null(min.diff) &&!is.null(bad.steps)) {
                result = env$metric$get(env$train.metric)
                if (result$value > 0.999){
                    return(FALSE)
                }
                if (!is.null(env$previous.result) && env$previous.result-result$value < min.diff) {
                    env$bad.steps <- env$bad.steps + 1
                    if (iteration > min.steps && env$bad.steps >= bad.steps){
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


naloziStevke = function(izbraneStevke=0:9, pozitivniRazred=NULL){
    set.seed(123)
    podatki = read.csv('train.csv')
    x = podatki[names(podatki)!='label'] / 255  # normalizacija
    y = podatki$label
    # obdržimo le izbrane števke
    okIndeksi = rep(FALSE, length(y))
    for (stevka in izbraneStevke){
        okIndeksi = okIndeksi | y == stevka
    }
    x = x[okIndeksi, ]
    y = y[okIndeksi]
    # po potrebi preobrazimo v dvojiski problem
    if(!is.null(pozitivniRazred)){
        pozitivni = y == pozitivniRazred
        y = rep(0, length(y))
        y[pozitivni] = 1
    }
    # razbijemo na dva dela
    ucniIndeksi = createDataPartition(y, p=0.8, list=FALSE)
    xUcni = as.matrix(x[ucniIndeksi, ])
    yUcni = y[ucniIndeksi]
    xTestni = as.matrix(x[-ucniIndeksi, ])
    yTestni = y[-ucniIndeksi]
    return(list(xUcni = xUcni, yUcni = yUcni, xTestni = xTestni, yTestni = yTestni))
}

prikaziSliko = function(stevka){
    n = dim(stevka)[2]
    image(stevka[,n:1])
}

dolociY = function(besedas, crka){
    y = rep(1.0, length(besedas))
    for (i in 1:length(besedas)){
        y[i] = as.numeric(grepl(crka, besedas[i]))
    }
    return(y)
}

naloziBesede = function(crka){
    stolpci = 167
    vrstice = 30
    piklsi = vrstice * stolpci
    tabela = read.csv("besede.csv", fileEncoding="UTF-8")
    vrsticeTabela = dim(tabela)[1]
    stolpciTabela = dim(tabela)[2]
    if (stolpciTabela != piklsi + 1){
        print("Nekaj je narobe")
    }
    besede = levels(tabela[, 1])
    xs = as.matrix(tabela[, 2:stolpciTabela])
    # iteracija po matriki poteka po stolpcih
    xs = t(xs)
    # dim slike x kanali x število primerov
    dim(xs) = c(stolpci, vrstice, 1, vrsticeTabela)
    # določimo še y
    y = dolociY(besede, crka)
    
    trainIndex = createDataPartition(y, p=0.8, list=FALSE)
    dim(trainIndex) = length(trainIndex)
    trainX = xs[,,,trainIndex]
    trainY = y[trainIndex]
    testX = xs[,,,-trainIndex]
    testY = y[-trainIndex]
    
    y_x_examples = dim(trainX)
    dim(trainX) = c(y_x_examples[1], y_x_examples[2], 1, y_x_examples[3])
    y_x_examples = dim(testX)
    dim(testX) = c(y_x_examples[1], y_x_examples[2], 1, y_x_examples[3])
    return(list(xUcni = trainX, yUcni = trainY, xTest = testX, yTest = testY))
}



#############################################################################################################
# 1. Če so podatki "lepi", lahko napovedi konvolucijskih mrež razložimo. Zato se bomo osredotočili na števke
#    0, 1 in 3 ter naredili model, ki loči med izbrano (npr. 3) in preostalima dvema.
#
#    Pri tej nalogi bomo spoznali naslednje novosti:
#    - konvolucijski sloj mx.symbol.Convolution, ki ima poleg parametra data še parametre
#      * kernel: dimenzije filtra, npr. c(5, 5) (y krat x)
#      * stride: korak za vsako od dimenzij, npr. c(1, 1)
#      * pad: širina obrobe za vsako dimenzij, npr. c(0, 0); pozor: če je c(1, 2),
#        smo vhod razširili za 2 in 4
#      * num.filter: število filtrov
#      * ...
#    - zbiralni / akumulacijski sloj  mx.symbol.Pooling, ki ima poleg parametra data še parametre
#      * pool_type (ali pool.type): način zbiranja, npr. "max"
#      * kernel: podobno kot zgoraj
#      * stride: podobno kot zgoraj
#      * pad: podobno kot zgoraj
#      * ...
#    - sploščitveni sloj mx.symbol.Flatten (oz. .flatten), ki je praktično brez parametrov
#      (izjema je seveda data) in splošči trenutno matriko/tenzor v 1D vektor.
#
#   Dokumentacija je nekoliko nedosledna (npr. http://beta.mxnet.io/r/api/mx.symbol.Convolution.html)
#   glede razsežnosti podatkov. Mi jih bomo podali kot
#   stolpci (oz. širina) x vrstice (oz. višina) x kanali x primeri
#
#   Ker se bomo ukvarjali s črno-belimi slikami, bo kanali = 1 (sicer pa je pogosto kanali = 3).
#   Vrsti red prvih dveh dimenzij ni zares pomemben.
#
#   Podatki so že pripravljeni (normalizirani! in ustreznih razsežnosti), vaša naloga pa je,
#   da naredite preprosto konvolucijsko mrežo:
#   - po vhodnem sloju naj bo konvolucijski. S klici prikaziSliko(xUcni[,,,iStevka]) se lahko prepričate,
#     da so vse števke približno enako velike, lepo centrirane in segajo "čez vso" sliko,
#     zato bomo naredili filter velikosti 27 x 27.
#   - konvolucijskemu sloju naj sledi aktivacijski. Premislite kakšna je njegova dimenzija.
#     Če vam je dolgčas, lahko izpeljete še splošno formulo.
#   - aktivacijskemu naj sledi zbiralni: parameter kernel nastavite tako, da bo na izhodu tega sloja
#     en sam nevron (tj. sloj bo oblike 1 x 1 x 1)
#   - sploščite prejšnji sloj v 1D vektor (dolžine 1)
#   - dodajte še polnopovezani sloj z dvema izhodnima nevronoma, ki ga opremite še s softmaxom.
#
#   Izberi ustrezne parametre za optimizacijo uteži ter na koncu nariši filter, ki ga dobiš.
#   Je podoben števki, ki predstavlja pozitivni razred?
#############################################################################################################

podatki = naloziStevke(izbraneStevke = 0:9, pozitivniRazred = 4)
xUcni = t(podatki$xUcni)
xTest = t(podatki$xTestni)
# števke so dimenzij 28 x 28
dim(xUcni) = c(28, 28, 1, ncol(xUcni))
dim(xTest) = c(28, 28, 1, ncol(xTest))
# 10. stevka
# prikaziSliko(xUcni[,,,6])

yUcni = podatki$yUcni
yTest = podatki$yTestni

# Preprosta konvolucijska mreža:
# slika števke --> konvolucija --> 2 x 2 --> 1 x 1 --> izhod

data = mx.symbol.Variable('data')  # vhodni podatki

# konvolucija: ker so slike razsežnosti 28 x 28, bomo dobili (1 filter) x 2 x 2 izhod
conv1 = mx.symbol.Convolution(data = data, kernel = c(27, 27), num.filter = 1)
# aktivacija rezultatov filtrov
act1 = mx.symbol.Activation(data = conv1, act_type = "tanh")
# za vsak prejšnji filter združimo 2 x 2 kvadrate --> dobimo 1 x 1 x 1
pool1 = mx.symbol.Pooling(data = act1, pool_type = "max", kernel = c(2, 2), stride = c(1, 1))
# sploščimo to v vektor
flatten = mx.symbol.Flatten(data = pool1)
# še izhodna plast
fc1 = mx.symbol.FullyConnected(data=flatten, num_hidden=2)
cnn = mx.symbol.SoftmaxOutput(data = fc1)

filterDatoteka = "filter.csv"
model = mx.model.FeedForward.create(
    cnn,
    X = xUcni,
    y = yUcni,
    eval.data=list(data=xTest, label=yTest),
    num.round = 100,
    array.batch.size = 100,
    learning.rate = 0.001,
    momentum = 0.9,
    eval.metric = mx.metric.accuracy,
    epoch.end.callback = sprotniRisalec
)
# shranimo konvolucijski filter in ga pokažemo
shraniFilter(model, "filterZa0proti13.csv")
prikaziFilter("filterZa0proti13.csv")


#########################################################################################################
# 2. Naredi "navadno" polnopovezano mrežo, ki loči med vsemi desetimi števkami. Podatkov ni treba
#    preoblikovati v slike razsežnosti 28 x 28, saj so si v polnopovezanih slojih vsi vhodni nevroni
#    enako blizu. Mreža naj ima poleg vhodnega in izhodnega še dva skrita sloja. Njuno širino določi,
#    kakor ti drago. Prav tako parametre učenja. Vzrajamo le pri tem, da tokrat naučimo uporabe parametra
#
#    initializer=mx.init.uniform(meja)
#  
#    ki začetne vrednosti uteži enakomerno razporedi na [-meja, meja]. Meje reda 0.01 bodo v redu.
#
#    Izračunaj točnost dobljene mreže na testnih podatkih.
#########################################################################################################

podatki = naloziStevke()
xUcni = podatki$xUcni
xTest = podatki$xTestni
yUcni = podatki$yUcni
yTest = podatki$yTestni


data = mx.symbol.Variable("data")
fc1 = mx.symbol.FullyConnected(data, num_hidden=128)
act1 = mx.symbol.Activation(fc1, act_type="relu")
fc2 = mx.symbol.FullyConnected(act1, num_hidden=64)
act2 = mx.symbol.Activation(fc2, act_type="relu")
fc3 = mx.symbol.FullyConnected(act2, num_hidden=10)
softmax = mx.symbol.SoftmaxOutput(fc3)

mx.set.seed(0)
fcNNModel = mx.model.FeedForward.create(
    softmax, X=xUcni, y=yUcni,
    eval.data = list(data = xTest, label = yTest),
    num.round=10, array.batch.size=100,
    learning.rate=0.07, momentum=0.9,
    eval.metric=mx.metric.accuracy,
    initializer=mx.init.uniform(0.01)
)

napovediFCNN = predict(fcNNModel, xTest)
yFCNN = max.col(t(napovediFCNN)) - 1  # to so ravno števke
accFCNN = mean(yFCNN == yTest)

####################################################################################################
# 3. Naredi še konvolucijsko mrežo. Zanjo naj velja:
#    - naj ima vsaj dva konvolucijska sloja
#    - vsakemu konvolucijskemu sledi najprej aktivacijski, nato pa zbiralni
#    - za skupinama/i zgornjih naj pride sploščitveni
#    - dodaj še kakšen polnopovezan sloj
#    - na izhodnem uporabi softmax
#
#    Izračunaj točnost mreže tudi na teh podatkih in jo primerjaj s prejšnjo.
#####################################################################################################
podatki = naloziStevke()
xUcni = t(podatki$xUcni)
xTest = t(podatki$xTestni)
dim(xUcni) = c(28, 28, 1, ncol(xUcni))
dim(xTest) = c(28, 28, 1, ncol(xTest))
yUcni = podatki$yUcni
yTest = podatki$yTestni

# naredimo malce bolj divjo mrežo kot prej
data = mx.symbol.Variable('data')

conv1 = mx.symbol.Convolution(data = data, kernel = c(5, 5), num.filter = 5)
act_conv1 = mx.symbol.Activation(data = conv1, act_type = "tanh")
pool1 = mx.symbol.Pooling(data = act_conv1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

conv2 = mx.symbol.Convolution(data = pool1, kernel = c(4, 4), num.filter = 5)
act_conv2 = mx.symbol.Activation(data = conv2, act_type = "tanh")
pool2 = mx.symbol.Pooling(data=act_conv2, pool_type = "max", kernel = c(2, 2), stride = c(1, 1))

flatten = mx.symbol.Flatten(data = pool2)
fc1 = mx.symbol.FullyConnected(data = flatten, num_hidden = 100)
act_fc1 = mx.symbol.Activation(data = fc1, act_type="tanh")

fc2 = mx.symbol.FullyConnected(data=act_fc1, num_hidden=10)
cnn = mx.symbol.SoftmaxOutput(data = fc2)

CNNModel = mx.model.FeedForward.create(
    cnn,
    X = xUcni,
    y = yUcni,
    eval.data = list(data = xTest, label = yTest),
    num.round = 10,
    array.batch.size = 100,
    learning.rate = 0.07,
    momentum = 0.9,
    initializer=mx.init.uniform(0.01),
    eval.metric = mx.metric.accuracy)

napovediCNN = predict(CNNModel, xTest)
yCNN = max.col(t(napovediCNN)) - 1  # to so ravno števke

accCNN = mean(yCNN == yTest)

###############################################################################################
# 4. Analizirjamo rezultate. Kateri razred napovemo najslabše / najboljše?
#    Med katerima razredoma najtežje ločimo? Se to sklada z intuicijo?
#   
#    V pomoč vam je lahko funkcija matrikaNapak.
###############################################################################################

matrikaNapak = function(yPravi, yNapoved){
    # matrika[i, j]: število primerov razreda i, ki so bili napovedani kot j
    matrika = matrix(0, 10, 10)
    n = length(yPravi)    
    for (primer in 1:n){
        i = yPravi[primer] + 1
        j = yNapoved[primer] + 1
        matrika[i, j] = matrika[i, j] + 1
    }
    colnames(matrika) = 0:9
    rownames(matrika) = 0:9
    print(matrika)
}


matrikaNapak(yTest, yFCNN)
matrikaNapak(yTest, yCNN)


################################################################################################################
# 5. Lotimo se še razpoznavanja črk v besedi. Prejšnji problem je bil nekoliko lažji, saj smo imeli na vsaki
#    sliki le en objekt. Tokrat jih imamo več: v datoteki besede.csv je približno 2050 izbranih besed iz SSKJ,
#    ki se začno na m (prekratke, predolge in še kakšne so bile odstranjene). Predstavljene so kot slike (oglej
#    si kakšno), cilj pa je naučiti, katere izmed njih vsebujejo črko e.
#
#    Slike so velikosti 167 x 30 (začetna velikost 500 x 150 je bila okleščena, kolikor se je dalo). Mreža naj bo
#    videti tako:
#    - konvolucijski sloj
#    - aktivacijski sloj
#    - zbiralni sloj: po tem naj bodo podatki velikosti 1 x 1 x 1
#    - sploščitveni sloj
#    - izhodni sloj
#
#    Na izhodnem sloju lahko postopate kot po navadi (mx.symbol.SoftmaxOutput(...)) ali pa preizkusite še drugo
#    možnost: mx.symbol.LinearRegressionOutput(data=flatten). V tem primeru bo na izhodu samo en nevron.
#
#################################################################################################################

crka = "i"
podatki = naloziBesede(crka)
xUcni = podatki$xUcni
xTest = podatki$xTest
yUcni = podatki$yUcni
yTest = podatki$yTest

prikaziSliko(xUcni[,,,12])


data = mx.symbol.Variable('data')
# Velikost filtra nastavimo približno na eno črko - to ocenimo s pomočjo slik
conv1 = mx.symbol.Convolution(data = data, kernel = c(20, 15), num.filter = 1)
act_conv1 = mx.symbol.Activation(data = conv1, act_type = "tanh")
# Slike: 167 x 30, torej je vhod v spodnji sloj dimenzije ... 148 = 167 - 20 + 1 in 16 = 30 - 15 + 1;
# če želimo kar takoj 1 x 1 x 1 (radi bi, da filter "naredi vse")
pool1 = mx.symbol.Pooling(data = act_conv1, pool_type = "max", kernel = c(148, 16), stride = c(1, 1))
flatten = mx.symbol.Flatten(data = pool1)

najBoKlasifikacija = FALSE
if (najBoKlasifikacija){
    fc1 = mx.symbol.FullyConnected(data = flatten, num_hidden = 2)
    NN_model = mx.symbol.SoftmaxOutput(data = fc1)    
} else{
    NN_model = mx.symbol.LinearRegressionOutput(data = flatten)
}
# Pozor: primerni parametri, se izkaže, so malo drugačni pri regresijskem in klasifikacijskem pristopu
# Poleg tega je treba spremeniti še metriko ter naš callback (ker je pri točnosti skala malce drugačna kot pri
# MSE in ker je pri točnosti več = bolje, kar ni res pri MSE)
filterDatoteka = sprintf("%sx.csv", crka)
model = mx.model.FeedForward.create(
    NN_model,
    X = xUcni,
    y = yUcni,
    eval.data=list(data=xTest, label=yTest),
    num.round = 200,
    array.batch.size = 40,
    learning.rate = 0.005,
    momentum = 0.9,
    eval.metric = mx.metric.mse, # mx.metric.accuracy,
    epoch.end.callback = mx.callback.early.stop(min.steps = 10, min.diff = 0.00001, bad.steps = 5)
)
crka = "c"
filterDatoteka = sprintf("%sx.csv", crka)
prikaziFilter(filterDatoteka)

shraniFilter(model, filterDatoteka)
