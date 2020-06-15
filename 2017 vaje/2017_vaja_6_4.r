# ONE V ALL:

cSVM1vA <- list(type = "Classification",
              library = "kernlab",
              loop = NULL)

prm <- data.frame(parameter = c("C"),
                  class = c("numeric"),
                  label = c("Cost"))

cSVM1vA$parameters <- prm

svmGrid <- function(x, y, len = NULL, search = "grid") {
  return(NULL);
}
cSVM1vA$grid<-svmGrid;

# fit funkcija nauci en model za vsak "level" oziroma razred.
svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
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
cSVM1vA$fit <- svmFit


svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
  # modelFit je zdaj seznam vseh klasifikatorjev, naučenih v metodi fit
  predictions <- matrix(0, nrow(newdata), length(modelFit$levels))
  dimnames(predictions) <- list(c(), modelFit$levels)
  # za vsak razred si pogledamo, koliko so novi primeri oddaljeni od ločnice:
  for(level in modelFit$obsLevels){
    predictions[, level] <- predict(modelFit[level][[1]], newdata, type='decision')
  }
  # glasujemo za tisti razred, ki dobi najmocnejsi glas:
  return(as.factor(colnames(predictions)[max.col(predictions)]))
}
cSVM1vA$predict <- svmPred

svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type="probabilities")
cSVM1vA$prob <- svmProb


svmSort <- function(x) x[order(x$C),]
cSVM1vA$sort <- svmSort

levelsFunction <- function(x){
  lev(x);
}
cSVM1vA$levels <- levelsFunction;

library(caret)

fitControl <- trainControl(method = "cv",
                           number = 10)
data(iris)
Laplacian <- train(Species ~ ., data = iris,
                   method = cSVM1vA,
                   tuneGrid = data.frame(C=1),
                   trControl = fitControl)
print(Laplacian, digits = 3)










