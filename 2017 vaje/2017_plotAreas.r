plotAreas <- function(model, data, gridNum=10, colNames=NULL, lev=NULL){
  if(is.null(colNames)){
    colNames <- names(data)
  }
  if(is.null(lev)){
    lev <- levels(data[,colNames[3]])
  }
  X1 <- data[, colNames[1]]
  X2 <- data[, colNames[2]]
  Y <- data[, colNames[3]]
  # Naredimo mrežo točk:
  cx <- seq(min(X1), max(X1), length.out=gridNum)
  cy <- seq(min(X2), max(X2), length.out=gridNum)
  cxy <- expand.grid(cx, cy)
  
  # Poimenujemo stolpce enako kot v podatkih, na katerih je bil naučen model
  names(cxy) <- colNames[1:2]
  # Naredimo predikcijo za vsako točko v mreži
  preds <- predict(model, newdata=data.frame(cxy))
  
  # Risanje mreže:
  library(lattice)
  names(cxy) <- c('X1', 'X2')
  cxy$preds<-as.numeric(preds)
  
  
  # Z ukazom levelplot narišemo področja enakih razredov:
  plt <- levelplot(preds~X1*X2, cxy,
                   panel=function(...){
                     panel.levelplot(...)
                     grid.points(X1[Y==lev[1]], X2[Y==lev[1]], pch=4, gp=gpar(col='red'))
                     grid.points(X1[Y==lev[2]], X2[Y==lev[2]], pch=4, gp=gpar(col='blue'))
                   }
  ) 
  return(plt)
}