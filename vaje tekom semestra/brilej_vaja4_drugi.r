library(earth)
library(caret)
library(pROC)
library(PRROC)

# Drugi del - izbira najboljših parametrov na podlagi različnih mer kakovosti

# Izračunaj najboljšo vrednost parametra k v primeru, ko za mero kakovosti izberemo 
# natančnost (precision), priklic ali ploščino pod krivuljo ROC. 
# Pri zadnjih dveh ni praktično ničesar treba napisati na roke, če si ogledamo i) argument metric 
# metode train, ii) argument summaryFunction v konstruktorju za trainControl in iii) twoClassSummary. 
# Za natančnost napišite summary kar na roke.

data(etitanic)
podatki <- etitanic
podatki$survived[podatki$survived==1] <- "Y"
podatki$survived[podatki$survived==0] <- "N"
podatki$survived <- as.factor(podatki$survived)

# ploščina pod ROC krivuljo
tc = trainControl(method = "cv", number = 20, classProbs = TRUE,savePredictions = TRUE,
                  index = createFolds(podatki$survived, k=20, returnTrain=TRUE), summaryFunction = twoClassSummary
                  )
model<- train(survived ~ ., data = podatki,
              method = "knn",
              metric = "ROC",
              trControl = tc,
              tuneGrid=data.frame(k=1:20))

model$finalModel$k # k = 3

# priklic
model<- train(survived ~ ., data = podatki,
              method = "knn",
              metric = "Sens",
              trControl = tc,
              tuneGrid=data.frame(k=1:20))

model$finalModel$k # k = 19

# natančnost (precision)
calculatePrecision <- function(data, levels, ...){
  # Predpostavljamo, da je levels[1] negativni, levels[2] pa pozitivni razred. To se ujema
  # s funkcijo twoClassSummary.
  predictedPositive <- sum(data$pred==levels[2])
  truePositive <- sum(data$pred==data$obs & data$pred==levels[2])
  precision <- truePositive / predictedPositive
  names(precision) <- 'Precision'
  precision
}

tc = trainControl(method = "cv", number = 20, classProbs = TRUE,savePredictions = TRUE,
                  index = createFolds(podatki$survived, k=20, returnTrain=TRUE), summaryFunction = calculatePrecision
)

model <- train(survived ~ ., data = podatki,
              method = "knn",
              metric = "Precision",
              trControl = tc,
              tuneGrid=data.frame(k=1:20))

model$finalModel$k # k = 14

# Izračunaj najboljšo vrednost parametra k v primeru, ko za mero kakovosti izberemo najmanjšo ceno modela, 
# kjer predpostavljamo, da je napaka tipa 1 dvakrat dražja od napake tipa 2. 
# Napako tipa 1 (zmotno pozitivni)  merite kot delež tistih negativnih primerov, ki so bili zmotno razglašeni za pozitivne. 
# Analogno definirajte napako tipa 2 (zmotno negativni). Končna napaka naj bo torej vsota 2 N1 + N2. 
# Postopate lahko podobno kot zgoraj pri natančnosti.

napaka <- function(data, levels, ...){
  # napaka tipa 2 = "false negative"
  # napaka tipa 1 = "false positive"
  falseNegative <- sum(data$pred==levels[1] & data$obs == levels[2])
  actualPositive <- sum(data$obs==levels[2])
  fnr <- falseNegative / actualPositive
  falsePositive <- sum(data$pred==levels[2] & data$obs == levels[1])
  actualNegative <- sum(data$obs==levels[1])
  fpr <- falsePositive / actualNegative
  
  cost = 2 * fpr + fnr
  names(cost) <- 'cost'
  cost
}

tc = trainControl(method = "cv", number = 20, classProbs = TRUE,savePredictions = TRUE,
                  index = createFolds(podatki$survived, k=20, returnTrain=TRUE), summaryFunction = napaka
)


model <- train(survived ~ ., data = podatki,
              method = "knn",
              metric = "cost",
              maximize = "false",
              trControl = tc,
              tuneGrid=data.frame(k=1:20))

model$finalModel$k # k = 3

# Pri katerih nalogah v resničnem življenju se splača bolj utežiti N1 in pri katerih N2? 
# Kaj je bolj važno za npr. teste, ki zaznavajo okužbe?

# pomembno je, da je malse false negative, torej, da nam rečejo, da nisno bolni, v resnici pa smo

