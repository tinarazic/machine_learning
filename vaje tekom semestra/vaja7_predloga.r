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

###########################################################################################################
# Prepričaš se lahko še, da analogne rezultate dobiš tudi za podatke
#  - z nekaj šuma,
#  - s prevelikimi polmeri žog.
#
###########################################################################################################


###########################################################################################################
# 3. Preizkusi omenjena jedra še na podatkovju (e)titanic. Katero deluje najbolje in koliko je
#    pripadajoča točnost?
#
#    Pazi, da bo razbitje množice v prečnem preverjanju, ki ga uporabiš za ocenjevanje parametrov,
#    vedno isto. Za vsako od metod naredi ustrezen tuneGrid. Tako trainControl kot tuneGrid lahko
#    podaš metodi nauciNarisi. Podati ji moraš tudi ustrezno ime ciljne spremenljivke.
###########################################################################################################
cs = exp(-3:3)

sigmas = exp(-3:3)

degrees = 1:3
scales = 10^(-2:1)



##########################################################################################################
# 4. Implementiraj metodo 1 : ostali za klasifikacijske naloge (spomnimo se vaj 5).
#    Metoda naj uporablja linearna jedra. Nekaj stvari je že implementiranih, nekaj pa jih bo še treba:
#    (fit in predict)
##########################################################################################################
svm1vsA = list(type = "Classification",
               library = "kernlab",
               loop=NULL)

svm1vsA$parameters = data.frame(parameter = c("C"),
                                class = c("numeric"),
                                label = c("Cena"))
svmGrid <- function(x, y, len = NULL, search = "grid") {
    return(NULL);
}

svm1vsA$grid = svmGrid


# fit funkcija naj nauči en model za vsak "level" oziroma razred
noviFit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    modelList <- list()
    # Za vsak razred naučimo en klasifikator:
    for (level in lev){
      newY <- rep(-1, length(y))
      newY[y==level] <- 1
      newY<-as.factor(newY)
      modelList[level] <- ksvm(x = as.matrix(x), y = newY,
                               kernel = vanilladot,
                               kpar = list(),
                               C = param$C,
                               prob.model = classProbs,
                               ...)
    }
    # Vrnemo seznam vseh klasifikatorjev
    return(modelList);
}
svm1vsA$fit <- noviFit


noviPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    # modelFit je zdaj seznam vseh klasifikatorjev, naučenih v metodi fit
    # za vsak primer vrni tisti razred, ki ima najmočnejši glas
  predictions <- matrix(0, nrow(newdata), length(modelFit$levels))
  dimnames(predictions) <- list(c(), modelFit$levels)
  # za vsak razred si pogledamo, koliko so novi primeri oddaljeni od ločnice:
  for(level in modelFit$obsLevels){
    predictions[, level] <- predict(modelFit[level][[1]], newdata, type='decision')
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
svm1vsA$levels <- levelsFunction;


##########################################################################################################
# 4. Preizkusi metodo 1 : ostali na podatkih iris
##########################################################################################################
fitControl <- trainControl(method = "cv",
                           number = 10)
data(iris)

mojModel <- train(Species ~ ., data = iris,
                   method = svm1vsA,
                   tuneGrid = data.frame(C=1),
                   trControl = fitControl)
print(svm1vsA, digits = 3)
