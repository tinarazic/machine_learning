library(caret)
library(rpart)
library(rpart.plot)

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

Y[sample(1:N, N/20)] <- 'D'
Y[sample(1:N, N/20)] <- 'N'

myData <- data.frame(X1=X1, X2=X2, X3=X3, X4=X4, X5=X5, Y=Y)


# Učna metoda "po meri" (podrobnosti na https://topepo.github.io/caret/using-your-own-model-in-train.html) 
# potrebuje kar nekaj podatkov. Vrednost type pove, ali gre za regresijo ali klasifikacijo. Vrednost library
# pove vse knjižnice ki so potrebne za izvedbo (če je več kot ena, podamo seznam (c('library1', 'library2',...))

customRpart <- list(
  type="Classification",
  library="rpart"
)

# parameters pove, katere parametre ima na voljo funkcija train (in katere podamo v tuneGrid). Za vsak parameter
# povemo njegov tip in ime, po možnosti še oznako.

customRpart$parameters <- data.frame(
  parameter=c("my_cp", "my_maxdepth"),
  class=c("numeric", "numeric"),
  label=c("Complexity parameter", "Maximum depth")
)

# Najpomembnejši sta funkciji fit in predict. Fit nauči model...

customRpart$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  tree <- rpart(data.frame(y,x), control=rpart.control(maxdepth=param$my_maxdepth, cp=param$my_cp))
  tree <- prune(tree, cp=param$my_cp)
  tree
}

# ... predict pa uporabi model na novih podatkih.

customRpart$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
  predict(modelFit, data.frame(newdata), type="class")
}


# vrednosti prob in grid za naše potrebe nista potrebni, ampak caret brez njiju noče izvesti klica train.
# grid je funkcija, ki ustvari tuneGrid (kar mi vedno podajamo sami)
# prob je funkcija, ki napove ne le razrede ampak tudi verjetnosti za pripadanje danemu razredu (tega trenutno ne rabimo)
customRpart$prob <- 0
customRpart$grid <- 0


myDataX <- myData[names(myData)!='Y']
myDataY <- myData$Y

# customRpart zdaj lahko uporabimo kot metodo učenja:

customRpartModel <- train(myDataX, myDataY,
                          method = customRpart,
                          tuneGrid = expand.grid(my_cp=seq(0,0.09, 0.01), my_maxdepth=1:10),
                          trControl = trainControl(method = "cv", number = 10))
