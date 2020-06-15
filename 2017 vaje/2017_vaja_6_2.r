library(caret)
library(kernlab)
library(grid)
source('plotAreas.R')
N=200;

# Radialno ločljivi podatki:

# Podatke je najbolje generirati v cilindričnih koordinatah:
sd <- 0.3
r <- c(rnorm(N/2, mean=2, sd=sd), rnorm(N/2, mean=4, sd=sd))
theta <- runif(N)
# Pretvorba v kartezične koordinate:
X1 <- r * cos(2*pi*theta)
X2 <- r * sin(2*pi*theta)

Y <- c(rep(TRUE, N/2), rep(FALSE, N/2))
Y <- factor(as.logical(Y))
plot(X1[Y==TRUE], X2[Y==TRUE], col='red', ylim=c(min(X2), max(X2)), xlim=c(min(X1), max(X1)))
points(X1[Y==FALSE], X2[Y==FALSE], col='blue')

myData <- data.frame(X1=X1, X2=X2, Y=Y)

myModel <- train(Y~., 
                 data=myData,
                 method='svmRadial',
                 tuneGrid=data.frame(C=Inf, sigma=1),
                 trControl=trainControl(method='none'))

plotAreas(myModel, myData, gridNum=500)

# Podatkom dodamo šum preprosto tako, da povečamo vrednost sd
sd <- 1
r <- c(rnorm(N/2, mean=2, sd=sd), rnorm(N/2, mean=4, sd=sd))
theta <- runif(N)
X1 <- r * cos(2*pi*theta)
X2 <- r * sin(2*pi*theta)
s
Y <- c(rep(TRUE, N/2), rep(FALSE, N/2))
Y <- factor(as.logical(Y))
myData <- data.frame(X1=X1, X2=X2, Y=Y)

# Bolje je, če so vrednosti C razporejene "eksponentno", torej da so logaritmi enakomerno porazdeljeni.
# Sprememba C iz 0.01 na 1.01 je zelo velika, sprememba iz 10000 na 10001 pa je neopazna.

Cs <- 10 ^ seq(-10,10,length.out = 4)

# Vizualna ocena vpliva parametra C:

for(C in Cs){
  myModel <- train(Y~., 
                   data=myData,
                   method='svmRadial',
                   tuneGrid=data.frame(C=C, sigma=1),
                   trControl=trainControl(method='none'))
  plot(myModel$finalModel, data=as.matrix(myData[names(myData)!='Y']))
  plt <- plotAreas(myModel, myData, gridNum=500)
  plot(plt)
}

# Ocena vpliva parametra C preko prečnega preverjanja:

cvModel <- train(Y~.,
                 data=myData,
                 method='svmRadial',
                 tuneGrid=data.frame(C=Cs, sigma=rep(1,4)),
                 trControl=trainControl(method='cv', number=10))

