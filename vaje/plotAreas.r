library(grid)

plotAreas = function(model, data, gridNum=500, colNames=NULL, lev=NULL, naslov="", Y=NULL){
    if(is.null(colNames)){
        colNames = names(data)
    }
    if (is.null(colNames)){
        colNames = 1:dim(data)[1]
    }
    if(is.null(lev)){
        if (is.null(Y)){
            lev = levels(data[,colNames[3]])
        } else{
            lev = c(0, 1)
        }
    }
    X1 = data[, colNames[1]]
    X2 = data[, colNames[2]]
    if (length(colNames) == 3){
        Y = data[, colNames[3]]
    }
    # Naredimo mrežo točk:
    cx = seq(min(X1), max(X1), length.out=gridNum)
    cy = seq(min(X2), max(X2), length.out=gridNum)
    cxy = expand.grid(cx, cy)
    
    # Poimenujemo stolpce enako kot v podatkih, na katerih je bil naučen model
    # Naredimo predikcijo za vsako točko v mreži
    names(cxy) = colNames[1:2]
    preds = NULL
    tryCatch({
        preds = predict(model, newdata=data.frame(cxy))    
    }, error = function(e){
        
    })
    if(is.null(preds)){
        preds = predict(model, as.matrix(cxy))
        preds = lev[max.col(t(preds))]
    }
    
    # Risanje mreže:
    library(lattice)
    names(cxy) = c('X1', 'X2')
    cxy$preds=as.numeric(preds)
    
    
    # Z ukazom levelplot narišemo področja enakih razredov:
    plt = levelplot(preds~X1*X2, cxy,
                     panel=function(...){
                         panel.levelplot(...)
                         grid.points(X1[Y==lev[1]], X2[Y==lev[1]], pch=4, gp=gpar(col='red'))
                         grid.points(X1[Y==lev[2]], X2[Y==lev[2]], pch=4, gp=gpar(col='blue'))
                     },
                    main = naslov
    ) 
    return(plt)
}
