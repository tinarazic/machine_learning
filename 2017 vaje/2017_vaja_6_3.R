library(earth)
library(caret)
library(kernlab)
data(etitanic)


etitanic$survived <- factor(etitanic$survived)
dataX <- etitanic[, names(etitanic)!='survived']
dataY <- etitanic$survived

cvSplit <- createFolds(dataY, k=10, returnTrain = TRUE)

Cs <- exp(-5:5)

# linearno jedro:
bestLinear <- train(survived~., data=etitanic,
                    method = 'svmLinear',
                    trControl = trainControl(method='cv', index=cvSplit),
                    tuneGrid = data.frame(C=Cs))
# radialno jedro:

sigmas <- exp(-2:2)
bestRadial <- train(survived~., data=etitanic,
                    method = 'svmRadial',
                    trControl = trainControl(method='cv', index=cvSplit),
                    tuneGrid = expand.grid(C=Cs, sigma=sigmas))
# polinomsko jedro:

degrees <- 1:3
data(iris)
bestPolynomial <-train(survived~., data=etitanic,
                       method = 'svmPoly',
                       trControl = trainControl(method='cv', index=cvSplit),
                       tuneGrid = expand.grid(C=Cs, degree=degrees, scale=1))

# Primerjamo:

max(bestLinear$results$Accuracy)
max(bestRadial$results$Accuracy)
max(bestPolynomial$results$Accuracy)

# Izkaže se, da je najboljše jedro za te podatke radialno jedro z vrednostima sigma = 0.1353353 and C = 2.718282 (seveda med testiranimi)