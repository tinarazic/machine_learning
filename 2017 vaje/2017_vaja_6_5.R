#ONE V ONE

lpSVM <- list(type = "Classification",
              library = "kernlab",
              loop = NULL)

prm <- data.frame(parameter = c("C", "sigma"),
                  class = rep("numeric", 2),
                  label = c("Cost", "Sigma"))

lpSVM$parameters <- prm

svmGrid <- function(x, y, len = NULL, search = "grid") {
  return(NULL);
}

lpSVM$grid <- svmGrid

svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  modelList <- list()
  # tokrat se naucimo en model za vsak PAR razredov.
  for (levelPair in combn(lev, 2, simplify=FALSE)){
    t<-as.logical((y==levelPair[1]) + (y==levelPair[2]))
    newY <- as.factor(as.numeric(y)[t])
    newX <- x[t,]
    modelList[paste(levelPair, collapse='-')] <- ksvm(x = as.matrix(newX), y = newY,
                                                      kernel = vanilladot,
                                                      kpar = list(),
                                                      C = param$C,
                                                      prob.model = classProbs,
                                                      ...)
  }
  modelList$levels <- lev;
  return(modelList);
}
lpSVM$fit <- svmFit


svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
  # predictions je tokrat celostevilska matrika:
  predictions <- matrix(0, nrow(newdata), choose(length(modelFit$levels),2))
  dimnames(predictions) <- list(as.character(1:nrow(newdata)), modelFit$levels)
  for(levelPair in combn(modelFit$levels, 2, simplify=FALSE)){
    key <- paste(levelPair, collapse='-');
    # pogledamo za kateri razred glasuje ta klasifikator
    votes <- as.character(predict(modelFit[key][[1]], newdata, type='response'));
    # naredimo vse pare (vrstica, stolpec), kjer je stolpec dolocen s tem, za kateri razred glasuje klasifikator:
    indices <- cbind(1:nrow(newdata), votes)
    # stetje glasov povecamo za 1
    predictions[indices] <- predictions[indices] + 1
  }
  return(as.factor(max.col(predictions)))
}
lpSVM$predict <- svmPred

svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type="probabilities")
lpSVM$prob <- svmProb


svmSort <- function(x) x[order(x$C),]
lpSVM$sort <- svmSort

levelsFunction <- function(x){
  lev(x);
}
lpSVM$levels <- levelsFunction;



library(caret)

wine <- read.csv('wine.csv')
wine$class <- as.factor(wine$class)

fitControl <- trainControl(method = "cv",
                           ## 10-fold CV...
                           number = 10)

Laplacian <- train(class ~ ., data = wine,
                   method = lpSVM,
                   tuneGrid = data.frame(),
                   trControl = fitControl)
print(Laplacian, digits = 3)









