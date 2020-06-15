library(earth)
library(caret)
library(pROC)
library(PRROC)

# Prvi del - ROC in PR krivulja

# Nariši ROC krivuljo za kNN model, ki napoveduje vrednost spremenljvke survived v podatkovju 
# etitanic (library(earth); data(etitanic)). 
# Uporabi 20-kratno stratificirano prečno preverjanje.

data(etitanic)
podatki <- etitanic
podatki$survived[podatki$survived==1] <- "Y"
podatki$survived[podatki$survived==0] <- "N"
podatki$survived <- as.factor(podatki$survived)

k <- 20

tc = trainControl(method = "cv", number = k,classProbs = TRUE,savePredictions = TRUE,
                  index = createFolds(podatki$survived, k=k, returnTrain=TRUE)
)

model <- train(survived ~ ., data = podatki, method = "knn",trControl = tc,
               tuneGrid=data.frame(k=5))

for(k in 5:5){
  napoved <- model$pred[model$pred$k==k, ]
  roc_obj <- roc(napoved$obs, napoved$Y, direction='<', levels=c('N', 'Y'))
  plot(roc_obj)
  print(auc(roc_obj))
  # ali to
  roc_obj<- roc.curve(scores.class0 = napoved$Y[napoved$obs=='Y'], scores.class1 = napoved$Y[napoved$obs=='N'], curve=TRUE)
  plot(roc_obj)
}

# Nariši PR krivuljo za isti model.

for(k in 5:5){
  napoved <- model$pred[model$pred$k==k, ]
  # pr curve
  pr_obj <- pr.curve(scores.class0 = napoved$Y[napoved$obs=='Y'], scores.class1 = napoved$Y[napoved$obs=='N'], curve=TRUE)
  plot(pr_obj)
}

# Sta krivulji (nujno/nikoli) konveksni/konkavni? 
# Se da ploščino pod njima kako interpretirati?



# Pri katerih nalogah v resničnem življenju je pomembnejši priklic? 
# Kaj pa natančnost? 
# Kaj je bolj važno za npr. teste, ki zaznavajo okužbe?




