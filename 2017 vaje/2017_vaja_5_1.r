N=300;

X1 <- sample(c(rep('A',N/2), rep('B',N/2)));
X2 <- sample(c(rep('A',N/2), rep('B',N/2)));
X3 <- sample(c(rep('A',N/2), rep('B',N/2)));
X4 <- sample(c(rep('A',N/3), rep('B',N/3), rep('C',N/3)));
X5 <- sample(c(rep('A',N/3), rep('B',N/3), rep('C',N/3)));
X6 <- sample(c(rep('A',N/3), rep('B',N/3), rep('C',N/3)));

Y <- rep('N', N)

Y[X1=='A' & X2=='B'] <- 'D'
Y[X1=='A' & X3=='A' & X4=='B'] <- 'D'
Y[X1=='B' & X4=='B' & X5=='A'] <- 'D'
Y[X3=='A' & X4=='A' & X5=='A' & X6=='C'] <- 'D'
Y[X3=='B' & X4=='C' & X5=='C' & X6=='B'] <- 'D'
Y[X3=='A' & X4=='C' & X5=='A' & X6=='A'] <- 'D'


myData <- data.frame(X1=X1, X2=X2, X3=X3, X4=X4, X5=X5, Y=Y)


# Osnovni klic metode train z opcijo method='rpart'
myModel1 <- train(Y~., data=myData,
               method='rpart', 
               trControl=trainControl(method='cv', number=10))

# Drevo lahko narišemo s pomočjo knjižnice rpart.plot:
library(rpart.plot)
rpart.plot(myModel1$finalModel)

# Osnovni klic metode train z opcijo method='rpart2'
myModel2 <- train(Y~., data=myData,
                  method='rpart2', 
                  trControl=trainControl(method='cv', number=10))


# Direktni klic knjižnice rpart:

rpartModel <- rpart(Y~., myData, control=rpart.control(cp=0, maxdepth = 30, minsplit=0, xval=0))
rpart.plot(rpartModel)

# 1. Za razliko od myModel1 in myModel2 se v zadnjem primeru ni zgodilo nič pred-procesiranja, tako da
# caret podatkov ni spremenil v numerične vektorje. Posledično so vozlišča drevesa lepše berljiva
# (list "X2=B" očitno sprašuje "ali je vrednost spremenljivke X2 enaka 'B'?", medtem ko list "X2B>=0.5" 
# to isto vprašanje postavi precej bolj nerodno. Še bolj nerodna je notacija "X2A<0.5", ki sprašuje
# po isti stvari.)

# 2. Nadzor nad spremenljivko cp imamo tak kot ponavadi, torej jo podamo kot data.frame v spremenljivko
# tuneGrid. Ostale parametre v primeru rpart lahko nadzorujemo samo preko parametra "control", ki je
# imenovani parameter, ki se v klicu funkcije train kar brez obdelave poda naprej funkciji rpart.

myDataX <- myData[names(myData)!='Y']
myDataY <- myData$Y
myModelrpartLike <- train(myDataX, myDataY, 
                  method='rpart', 
                  trControl=trainControl(method='cv', number=10),
                  tuneGrid = data.frame(cp=0),
                  control=rpart.control(maxdepth = 30, minsplit=1, xval=0))
rpart.plot(myModelrpartLike$finalModel)
