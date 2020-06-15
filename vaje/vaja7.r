library(caret)
library(kernlab)
RNGkind(sample.kind = "Rejection")
set.seed(12321)


trenutniCas = function(){
    as.numeric(as.POSIXct(Sys.time()))
}

narediPodatke = function(n, m, r, jeDvojiska, stopnjaSuma){
    xs = matrix(nrow = n, ncol = m)
    sredisca = matrix(nrow = n, ncol = m)
    for (i in 1:n){
        sredisce = sign(runif(m) - 0.5)
        while(TRUE){
            primer = 2 * r * (runif(m) - 0.5)
            if (norm(primer, type="2") <= r){
                xs[i, ] = sredisce + primer
                sredisca[i, ] = sredisce
                break
            }
        }
    }
    y = rep("0", n)
    if (jeDvojiska){
        y[sredisca[,1] > 0] = "1"
        for (i in 1:n){
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
        for (i in 1:n){
            razred = sredisca[i, ]
            if (runif(1) < stopnjaSuma){
                drugi = n + 1 - i 
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


nauciNarisi = function(data, imeY = "y", method="svmLinear", tuneGrid=NULL, trControl=trainControl()){
    t0 = trenutniCas()
    formula = as.formula(paste(imeY, " ~ ."))
    model = train(formula, 
                  data=data,
                  method=method,
                  tuneGrid = tuneGrid,
                  trControl=trControl)
    t1 = trenutniCas()
    imePodatkov = deparse(substitute(data))
    if (dim(data)[2] <= 3){
        plot(model$finalModel, data=as.matrix(data[names(data)!=imeY]))
        title(sub = sprintf("%s & %s", imePodatkov, method))
    } else{
        print(sprintf("Modela za %s ne moremo narisati.", imePodatkov))
    }
    t2 = trenutniCas()
    print(sprintf("%s & %s: gradnja: %.2f, risanje (napovedi): %.2f [sekunde]",
                  imePodatkov, method, t1 - t0, t2 - t1))
    return(model)
}


##########################################################################################################
# 1. Preizkusi polinomsko in Gaussovo jedro na dvojiških podatkih, kjer je r = 1/2 in šuma ni.
#    Razsežnost vhodnega prostora naj bo 2, podatkovja pa naj vsebuje vsaj 1000 primerov.
#    Rezultate primerjaj s tistimi od prejšnjič glede na 
#    - število podpornih vektorjev,
#    - lokacijo podpornih vektorjev.
#
#   Pomagaš si lahko s funkcijo nauciNarisi. Parametri metod:
#   - svmLinear: C
#   - svmRadial: C ter sigma v exp(-||u - v||^2 / (2 sigma^2))
#   - svmPoly: C ter scale in degree v (scale * <u, v> + 1)^degree
###########################################################################################################
# podatki1 = narediPodatke(1000, 2, 0.5, TRUE, 0)
# model11 = nauciNarisi(podatki1, method = "svmLinear")
# model12 = nauciNarisi(podatki1, method = "svmRadial")
# model13 = nauciNarisi(podatki1, method = "svmPoly")

###########################################################################################################
# Prepričaš se lahko še, da analogne rezultate dobiš tudi za podatke
#  - z nekaj šuma,
#  - s prevelikimi polmeri žog.
#
###########################################################################################################
# podatki2 = narediPodatke(1000, 2, 0.5, TRUE, 0.1)
# model21 = nauciNarisi(podatki2, method = "svmLinear")
# model22 = nauciNarisi(podatki2, method = "svmRadial")
# model23 = nauciNarisi(podatki2, method = "svmPoly")
# 
# podatki3 = narediPodatke(1000, 2, 1.2, TRUE, 0)
# model31 = nauciNarisi(podatki3, method = "svmLinear")
# model32 = nauciNarisi(podatki3, method = "svmRadial")
# model33 = nauciNarisi(podatki3, method = "svmPoly")
# 
# podatki4 = narediPodatke(1000, 2, 1.2, TRUE, 0.1)
# model41 = nauciNarisi(podatki4, method = "svmLinear")
# model42 = nauciNarisi(podatki4, method = "svmRadial")
# model43 = nauciNarisi(podatki4, method = "svmPoly")


###########################################################################################################
# 3. Preizkusi omenjena jedra še na podatkovju (e)titanic. Katero deluje najbolje in koliko je
#    pripadajoča točnost?
#
#    Pazi, da bo razbitje množice v prečnem preverjanju, ki ga uporabiš za ocenjevanje parametrov,
#    vedno isto. Za vsako od metod naredi ustrezen tuneGrid. Tako trainControl kot tuneGrid lahko
#    podaš metodi nauciNarisi. Podati ji moraš tudi ustrezno ime ciljne spremenljivke.
###########################################################################################################
# cs = exp(-3:3)
# 
# sigmas = exp(-3:3)
# 
# degrees = 1:3
# scales = 10^(-2:1)
# 
# titanic = dobiTitanic()
# particija = createFolds(titanic$survived, k = 10, returnTrain = TRUE)
# trControl = trainControl(method="cv", index=particija)
# 
# titanicLin = nauciNarisi(titanic, "survived", trControl = trControl,
#                          method = "svmLinear",
#                          tuneGrid = data.frame(C = cs))
# titanicRad = nauciNarisi(titanic, "survived", trControl = trControl,
#                          method = "svmRadial",
#                          tuneGrid = expand.grid(C = cs, sigma = sigmas))
# titanicPoly = nauciNarisi(titanic, "survived", trControl = trControl,
#                           method = "svmPoly",
#                           tuneGrid = expand.grid(C = cs, scale = scales, degree = degrees))
# > max(titanicLin$results["Accuracy"])
# [1] 0.7792674
# > max(titanicRad$results["Accuracy"])
# [1] 0.8107143
# > max(titanicPoly$results["Accuracy"])
# [1] 0.8097985
# > 



##########################################################################################################
# 4. Implementiraj metodo 1 : ostali za klasifikacijske naloge (spomnimo se vaj 5).
#    Metoda naj uporablja linearna jedra. Nekaj stvari je že implementiranih, za
#    - fit in
#    - predict
#    pa je treba še poskrbeti.
#
#    Podobno, kot smo to storili pri razširitvah dreves, lahko posamezne modele, ki jih zgradimo v fit,
#    dobimo s klicem
#    ksvm(x = as.matrix(x), y = newY,
#         kernel = vanilladot,         # poskrbi, da bodo jedra linearna
#         kpar = list(),
#         C = param$C,
#         prob.model = classProbs,
#         ...)
#
##########################################################################################################
svm1vsA = list(type = "Classification",
               library = "kernlab",
               loop=NULL)

svm1vsA$parameters = data.frame(parameter = c("C"),
                                class = c("numeric"),
                                label = c("Cena"))
svmGrid = function(x, y, len = NULL, search = "grid") {
    return(NULL);
}


svm1vsA$grid = svmGrid


# fit funkcija nauci en model za vsak "level" oziroma razred.
noviFit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    imena = levels(y)
    modelList = vector("list", length(imena))
    names(modelList) = imena
    # Za vsak razred naučimo en model:
    for (level in lev){
        newY = rep(-1, length(y))
        newY[y==level] = 1
        newY = as.factor(newY)
        modelLevel = ksvm(x = as.matrix(x), y = newY,
                          kernel = vanilladot,
                          kpar = list(),
                          C = param$C,
                          prob.model = classProbs,
                          ...)
        modelList[level] = modelLevel
    }
    # Vrnemo seznam vseh klasifikatorjev
    return(modelList);
}
svm1vsA$fit = noviFit


noviPred = function(modelFit, newdata, preProc = NULL, submodels = NULL){
    # modelFit je zdaj seznam vseh klasifikatorjev, naučenih v metodi fit (pa še kaj)
    predictions = matrix(0, nrow(newdata), length(modelFit$obsLevels))
    dimnames(predictions) = list(c(), modelFit$obsLevels)
    # za vsak razred si pogledamo, koliko so novi primeri oddaljeni od ločnice:
    for(level in modelFit$obsLevels){
        predictions[, level] = predict(modelFit[level][[1]], newdata, type='decision')
    }
    # glasujemo za tisti razred, ki dobi najmocnejsi glas:
    return(as.factor(colnames(predictions)[max.col(predictions)]))
}
svm1vsA$predict = noviPred

noviProb = function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit, newdata, type="probabilities")
}
svm1vsA$prob = noviProb


svmSort <- function(x) x[order(x$C),]
svm1vsA$sort <- svmSort

levelsFunction <- function(x){
    lev(x);
}
svm1vsA$levels = levelsFunction;


fitControl = trainControl(method = "cv", number = 10)
data(iris)
modelIris = train(iris[names(iris) != "Species"], iris$Species,
                  method = svm1vsA,
                  tuneGrid = data.frame(C=c(0.01, 0.1, 1.0, 10.0, 10000.0)),
                  trControl = fitControl)
podatki1 = narediPodatke(1000, 2, 0.5, FALSE, 0)
modelZoge1 = train(podatki1[names(podatki1) != "y"], podatki1$y,
                  method = svm1vsA,
                  tuneGrid = data.frame(C=1),
                  trControl = fitControl)
podatki2 = narediPodatke(1000, 2, 0.5, FALSE, 0.1)
modelZoge2 = train(podatki1[names(podatki1) != "y"], podatki1$y,
                  method = svm1vsA,
                  tuneGrid = data.frame(C=1),
                  trControl = fitControl)



# [1] "podatki1 & svmLinear: gradnja: 2.01, risanje (napovedi): 0.14 [sekunde]"
# [1] "podatki1 & svmRadial: gradnja: 2.32, risanje (napovedi): 0.19 [sekunde]"
# [1] "podatki1 & svmPoly: gradnja: 30.80, risanje (napovedi): 0.19 [sekunde]"
# [1] "podatki2 & svmLinear: gradnja: 1.42, risanje (napovedi): 0.12 [sekunde]"
# [1] "podatki2 & svmRadial: gradnja: 6.31, risanje (napovedi): 0.17 [sekunde]"
# [1] "podatki2 & svmPoly: gradnja: 42.15, risanje (napovedi): 0.22 [sekunde]"
# [1] "podatki3 & svmLinear: gradnja: 0.96, risanje (napovedi): 0.12 [sekunde]"
# [1] "podatki3 & svmRadial: gradnja: 4.08, risanje (napovedi): 0.16 [sekunde]"
# [1] "podatki3 & svmPoly: gradnja: 37.06, risanje (napovedi): 0.15 [sekunde]"
# [1] "podatki4 & svmLinear: gradnja: 1.17, risanje (napovedi): 0.11 [sekunde]"
# [1] "podatki4 & svmRadial: gradnja: 7.12, risanje (napovedi): 0.19 [sekunde]"
# [1] "podatki4 & svmPoly: gradnja: 46.34, risanje (napovedi): 0.23 [sekunde]"
# Loading required package: Formula
# Loading required package: plotmo
# Loading required package: plotrix
# Loading required package: TeachingDemos
# [1] "Modela za titanic ne moremo narisati."
# [1] "titanic & svmLinear: gradnja: 2.60, risanje (napovedi): 0.00 [sekunde]"
# [1] "Modela za titanic ne moremo narisati."
# [1] "titanic & svmRadial: gradnja: 63.99, risanje (napovedi): 0.00 [sekunde]"
# [1] "Modela za titanic ne moremo narisati."
# [1] "titanic & svmPoly: gradnja: 4423.92, risanje (napovedi): 0.00 [sekunde]"