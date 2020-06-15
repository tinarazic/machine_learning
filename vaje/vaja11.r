current_working_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)


library(caret)
library(rpart)

#########################################################################################
# 0.1 Naredimo podatke
#########################################################################################

narediY = function(x){
    y = x[, c(1, 2, 3)] %*% c(1, 2, 3)
    y = y + sqrt(abs(x[,4] * x[,5])) + sqrt(abs(x[,5] * x[,6]))
    y = y + rnorm(length(y))
}

set.seed(123)
m = 5000
n = 10
xs = matrix(rnorm(m * n), nrow = m, ncol = n)
y = narediY(xs)

nUcni = 1:round(0.75 * m)
xUcni = xs[nUcni,]
yUcni = y[nUcni]
xTestni = xs[-nUcni,]
yTestni = y[-nUcni]

mValid = round(m * 0.3)
xValid = matrix(rnorm(mValid * n), nrow = mValid, ncol = n)
yValid = narediY(xValid)


#########################################################################################
# 0.2 Vrečenje (ne le dreves): V splošnem (in tudi za caret to velja) je vrečenje družina
#     modelov, ki je določena z
#     - osnovnim modelom (in njegovimi parametri)
#     - številom vreč
#     - načinom združevanja napovedi osnovnih modelov
#
#     Ogledali si ga bomo na primeru regresije.
#
#     V caretu dobimo vrečenje s klicem
#     bag(xs, y, bagControl = bagControl(parametri), B = 20, ...),
#     pri čemer so xs vhodni in y izhodni podatki, B število vreč (bags), ... pa se podajo
#     osnovnemu modelu pred učenjem. Parameter bagControl j tisto, kjer zares povemo, kaj
#     želimo, in potrebuje naslednje tri stvari:
#     - fit: function(x, y, ...) --> osnovni model: x in y so vhodni in izhodni podatki
#     - predict: function(model, x) --> napovedi: model smo dobili s fit, x pa so vhodni
#                podatki, za katere želimo napovedi
#     - aggregate: function(x, type) --> napovedi: x je seznam dolžine B. Vsak njegov
#                  element je vektor napovedi pripadajočega modela. Parameter type je 
#                  pomemben pri klasifikaciji, ko modeli napovedujejo bodisi verjetnosti
#                  (vsak osnovni model napove npr. (0.3, 0.5, 0.2)) bodisi en razred
#                  (vsak osnovni model (efektivno) napove npr. (0, 1, 0)). Pri regresiji
#                  ga bomo ignorirali.
#                  
#     Ker paralelizacije niso napisali povsem pravilno, bomo dodali še četrtega:
#     allowParallel = FALSE
#
#     Za nadaljnje podrobnosti sledite povezavi https://rdrr.io/cran/caret/man/bag.html
#########################################################################################

modelVrecenja = function(x, y, bct, vrece){
    # Poskrbimo, da bodo imeli modeli iste vrece ...
    # x: vhodni podatki
    # y: izhodni
    # bct: bagControl objekt
    # vrece: število vreč
    set.seed(123)
    bag(x, y, bagControl = bct, B = vrece)
}

pokaziIzboljsanje = function(oob1, testVrecenje, imeModela){
    q = testVrecenje / oob1
    print(sprintf("mse(vrečenje) / mse(model) za %s: %.3e (= %.3e / %.3e)", imeModela, q, testVrecenje, oob1))
}

#########################################################################################
# 1. Neodvisno od samega osnovnega modela bomo napoved ansambla definirali kot povprečje
#    napovedi podmodelov. Zapiši funkcijo mojAgregator(x, type), ki bo primerna za
#    za uporabo v bagControl(aggregator= mojAgregator, ...).
#
#   Izračunati je treba torej povprečja vrstic v matriki x, type pa lahko ignoriramo.
#########################################################################################
mojAgregator = function(x, type){
    nModelov = length(x)
    nPrimerov = length(x[[1]])
    m = matrix(0, nrow = nPrimerov, ncol = nModelov)
    for(i in 1:nModelov){
        m[, i] = x[[i]]
    }
    rowMeans(m)
}


################################################################################################
# 2. Izvedi vrečenje drevesnih modelov na gornjih podatkih. Definirati je treba še
#    mojDrevoFit in mojDrevoPredict, potem pa vrečenje že lahko dobiš s klicem
#    modelVrecenja(xUcni, yUcni,
#                  bagControl(fit = mojDrevoFit,
#                             predict = mojDrevoPredict,
#                             aggregator = mojAgregator,
#                             allowParallel = FALSE
#                  ),
#                  število vreč
#    )
#
#    V metodi fit (function(x, y, ...)) lahko uporabiš
#
#    rpart(y~., data = data.frame(x, y = y), control=rpart.control(cp=0, xval=0, minsplit=0, minbucket=1))
#
#    saj si načeloma želimo globokih dreves.
#
#    Opozorilo: lokalno smo združili x in y v eno samo spremenljivko data, saj rpart potrebuje
#    formulo (podobno bo veljalo tudi za druge naloge), medtem ko dobimo x in y ločeno.
#
#    V metodi predict (function(model, x)) postopaj podobno, le da si vrednosti y tokrat izmisliš
#    (npr. same ničle).
#################################################################################################
mojDREVOfit = function(x, y, ...){
    # naredimo globoko drevo ...
    rpart(y~., data = data.frame(x, y = y), control=rpart.control(cp=0, xval=0, minsplit=0, minbucket=1))
}

mojPredict = function(model, x){
    # isti za vse
    yFake = matrix(0, nrow = nrow(x), ncol = 1)
    dataFake = data.frame(x, y = yFake)
    predict(model, dataFake)
}


bagCTDrevo = bagControl(fit = mojDREVOfit, predict = mojPredict, aggregate = mojAgregator,
                        allowParallel = FALSE)

vrecenjeDreves = modelVrecenja(xUcni, yUcni, bagCTDrevo, 10)

#################################################################################################
# 3. Izračunaj povprečno OOB napako posameznih modelov z uporabo funkcije oobMSE. Primerjaj to
#    z napako vrečenja na testnih podatkih. Koliko je relativno izboljšanje, tj.
#    MSE(vrečenje) / MSE(oob drevo)? (podatkov je dovolj, zato sta številki primerljivi)
#    Izračun testnega MSE si lahko implementiraš kot funkcijo, saj jo bomo računali večkrat.
#################################################################################################
oobMSE = function(bagModel){
    oob = bagModel$fits
    # oob: seznam (list); vsak njegov element vsebuje element oob,
    # ki je tabela s stolpci pred, obs (in key)
    n = length(oob)
    mse = rep(-1, n)
    for(i in 1:n){
        mse[i] = mean((oob[[i]]$oob$pred - oob[[i]]$oob$obs)^2)
    }
    mean(mse)
}


testMSE = function(model){
    mean((yTestni - predict(model, xTestni))^2)
}

oobDrevesa = oobMSE(vrecenjeDreves)
testDrevesa = testMSE(vrecenjeDreves)
pokaziIzboljsanje(oobDrevesa, testDrevesa, "vrecenjeDreves")
################################################################################################
# 4. Izvedi vrečenje linearnih modelov na istih podatkih. Definirati je treba še
#    mojLMFit in mojLMPredict, potem pa model že lahko dobiš s klicem kot prej.
#
#    V metodi fit lahko uporabiš lm(y ~ ., data, ...), spet pa bo treba tako pri fit kot pri predict
#    združiti x in y v eno samo spremenljivko ...
#################################################################################################
mojLMfit = function(x, y, ...){
    lm(y ~ ., data = data.frame(x, y = y), ...)
}

bagCTLM = bagControl(fit = mojLMfit, predict = mojPredict, aggregate = mojAgregator,
                     allowParallel = FALSE)
vrecenjeLM = modelVrecenja(xUcni, yUcni, bagCTLM, 10)


#################################################################################################
# 5. Izračunaj povprečno OOB napako posameznih linearnih modelov. Primerjaj to
#    z napako vrečenja na testnih podatkih. Koliko je relativno izboljšanje, tj.
#    MSE(vrečenje) / MSE(oob LM)? (podatkov je dovolj, zato sta številki primerljivi)
#################################################################################################
oobLM = oobMSE(vrecenjeLM)
testLM = testMSE(vrecenjeLM)
pokaziIzboljsanje(oobLM, testLM, "vrecenjeLM")


################################################################################################
# 6. Izvedi vrečenje modelov podpornih vektorjev na istih podatkih. Spet je treba definirati
#    isti dve metodi.
#
#    V metodi fit lahko uporabiš train(y ~ ., data = data.frame(x, y = y), method="svmRadial", ...),
#    saj želimo, da parametre podpornih vektorjev vsaj malo optimiziramo. Prav tako bo treba x
#    in y tako pri fit kot pri predict združiti v eno samo spremenljivko.
#################################################################################################
mojSVMfit = function(x, y, ...){
    # print("fitam svm")
    # spotoma se optimiziramo parametre  ...
    train(y ~ ., data = data.frame(x, y = y), method="svmRadial",
          trControl = trainControl(method="cv", number=4), ...)
}

bagCTSVM = bagControl(fit = mojSVMfit, predict = mojPredict, aggregate = mojAgregator,
                      allowParallel = FALSE)
vrecenjeSVM = modelVrecenja(xUcni, yUcni, bagCTSVM, 10)

#################################################################################################
# 7. Izračunaj povprečno OOB napako posameznih modelov podpornih vektorjev. Primerjaj to
#    z napako vrečenja na testnih podatkih. Koliko je relativno izboljšanje, tj.
#    MSE(vrečenje) / MSE(oob model)? (podatkov je dovolj, zato sta številki primerljivi)
#################################################################################################
oobSVM = oobMSE(vrecenjeSVM)
testSVM = testMSE(vrecenjeSVM)
pokaziIzboljsanje(oobSVM, testSVM, "vrecenjeSVM")

#################################################################################################
# 8. Nehomogeni ansambli. Izračunaj še testno MSE za napovedi nehomogenega ansambla,
#    ki jih dobiš kot povrečje napovedi vrecenja dreves, vrecenja LM in vrecenja SVM.
#    Je ta model še boljši kot prejšnji trije?
#################################################################################################


heterogen = function(drevesa, linearni, svmji, w = rep(1/3, 3), optimist = FALSE, metoda = "lm"){
    # prvi trije argumenti: napovedi na testni množici pripadajočih vrečenj
    # w: NULL (optimizacija uteži) ali vektor dolžine tri za uteženo povprečje napovedi
    # optimist: uporabi testno množico za optimizacijo (optimistična ocena napake!)
    # metoda: podamo v method argument funkcije train za zduževanje napovedi; v primeru lm je to
    #         kar uteženo povprečje

    if (is.null(w)){
        # optimizirajmo uteži/način združevanja napovedi
        print("Optimizacija združevanja napovedi ..")
        if (!optimist){
            print("  .. preko validacijske množice")
            napDrevesaValid = predict(vrecenjeDreves, xValid)
            napLMValid = predict(vrecenjeLM, xValid)
            napSVMValid = predict(vrecenjeSVM, xValid)
            podatki = data.frame(x1 = napDrevesaValid, x2 = napLMValid, x3 = napSVMValid, y = yValid)
        } else{
            # tega se NE počne, saj vodi v prekomerno prileganje: potrebovali bi
            # ločene podatke za validacijo - tu se poigrajmo s tem zgolj zato, da vidimo,
            # kako daleč lahko pridemo
            print("  .. preko testne množice (OCENA OPTIMISTIČNA!)")
            podatki = data.frame(x1 = drevesa, x2 = linearni, x3 = svmji, y = yTestni)
        }
        # združimo troje napovedi v končne
        if (metoda == "lm"){
            print("Za združevanje uporabimo uteženo povprečje")
            # Problem optimalnih uteži je linearen (v utežeh): w1 * nap1 + w2 * nap2 + w3 * nap3
            lmModel = lm(y ~ . - 1, podatki)  # -1: brez prostega člena
            w = lmModel$coefficients
            napovedi = w[1] * drevesa + w[2] * linearni + w[3] * svmji
        } else{
            print(sprintf("Za združevanje uporabimo metodo %s", metoda))
            # še bolje kot linearni model bi se morda odrezal stacking:
            # napovedi = f(napovedi elementov heterogenega ansambla)
            rfModel = train(y~., podatki, method = metoda)
            podatkiTestni = data.frame(x1 = drevesa, x2 = linearni, x3 = svmji, y = yTestni)
            napovedi = predict(rfModel, podatkiTestni)
        }
    } else{
        print("Združimo napovedi preko podanih uteži")
        napovedi = w[1] * drevesa + w[2] * linearni + w[3] * svmji
    }
    napaka = mean((napovedi - yTestni)^2)
    if (!is.null(w)){
        print(sprintf("Uteži v uteženem povprečju: %.2f %.2f %.2f", w[1], w[2], w[3]))
    }
    print(sprintf("Napaka heterogenega ansambla: %.3e (Napake članov: %.3e %.3e %.3e)", napaka, testDrevesa, testLM, testSVM))

}

napovedi1 = predict(vrecenjeDreves, xTestni)
napovedi2 = predict(vrecenjeLM, xTestni)
napovedi3 = predict(vrecenjeSVM, xTestni)

# uporabimo navadno povprečje
heterogen(napovedi1, napovedi2, napovedi3)

# izključimo najslabši model
heterogen(napovedi1, napovedi2, napovedi3, w = c(0, 0.5, 0.5))

# optimizirajmo uteži v uteženem povprečju
heterogen(napovedi1, napovedi2, napovedi3, w = NULL, optimist = TRUE)
heterogen(napovedi1, napovedi2, napovedi3, w = NULL, optimist = FALSE)

# namesto uteženega povprečja (linearni model) uporabimo naključne gozdove
heterogen(napovedi1, napovedi2, napovedi3, w = NULL, optimist = TRUE, metoda = "rf")
heterogen(napovedi1, napovedi2, napovedi3, w = NULL, optimist = FALSE, metoda = "rf")

# ... pa še podporne vektorje
heterogen(napovedi1, napovedi2, napovedi3, w = NULL, optimist = TRUE, metoda = "svmLinear")
heterogen(napovedi1, napovedi2, napovedi3, w = NULL, optimist = FALSE, metoda = "svmLinear")

