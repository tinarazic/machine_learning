library(caret)
library(kernlab)

N=200;

# Linearno ločljivi podatki:

X1 <- c(rnorm(N/2, mean=2), rnorm(N/2, mean=-2))
X2 <- c(rnorm(N/2, mean=2), rnorm(N/2, mean=-2))


Y <- c(rep(TRUE, N/2), rep(FALSE, N/2))
Y <- factor(as.logical(Y))

myData <- data.frame(X1=X1, X2=X2, Y=Y)

myModel <- train(Y~., 
                 data=myData,
                 method='svmLinear',
                 tuneGrid=data.frame(C=Inf),
                 trControl=trainControl(method='none'))

# Če želimo samo narisati podatke:
plot(X1[Y==TRUE], X2[Y==TRUE], col='red', ylim=c(min(X2), max(X2)), xlim=c(min(X1), max(X1)))
points(X1[Y==FALSE], X2[Y==FALSE], col='blue')

# kernlab-ova funkcija za risanje:
plot(myModel$finalModel, data=as.matrix(myData[names(myData)!='Y']))

# Bolj splošno risanje (ta koda je uporabna tudi za ostale klasifikacijske metode)
source('plotAreas.R')
x <- plotAreas(myModel, myData, gridNum=500)


# Dodamo malo šuma:
N <- 200
sd <- 1
X1 <- c(rnorm(N/2, mean=2, sd=sd), rnorm(N/2, mean=-2, sd=sd))
X2 <- c(rnorm(N/2, mean=2, sd=sd), rnorm(N/2, mean=-2, sd=sd))

Y <- c(rep(FALSE, N/20), rep(TRUE, 9 * N/20), rep(TRUE, N/40), rep(FALSE, 19 * N/40))
Y <- factor(as.logical(Y))

myData <- data.frame(X1=X1, X2=X2, Y=Y)

for(C in c(0.001, 0.01, 1, 10000)){
  myModel <- train(Y~., 
                   data=myData,
                   method='svmLinear',
                   tuneGrid=data.frame(C=C),
                   trControl=trainControl(method='cv', number=10))
  plt <- plotAreas(myModel, myData, gridNum=500)
  plot(plt)
}
cvModel <- train(Y~.,
                 data=myData,
                 method='svmLinear',
                 tuneGrid=data.frame(C=c(0.001, 0.01, 1, 10000)),
                 trControl=trainControl(method='cv', number=10))

