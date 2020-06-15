library(stringr)
library(stringi)

current_working_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

preberiUtezi = function(datoteka){
    con = file(datoteka, "r")
    vrstice = readLines(con)
    close(con)
    prva = 1
    while(!grepl(",1]", vrstice[prva])){
        prva = prva + 1
    }
    vrstice = vrstice[prva:(length(vrstice) - 1)]
    nVrstice = length(vrstice)
    nStolpci = 0
    nBloki = 0
    for (i in 1:length(vrstice)){
        if (grepl(",[0-9]", vrstice[i])){
            nStolpci = nStolpci + str_count(vrstice[i], ",")
            nVrstice  = nVrstice - 1
            nBloki = nBloki + 1
        }
    }
    if (nVrstice %% nBloki != 0){
        print("nekaj je narobe z utežmi")
        return(NULL)
    }
    nVrstice = nVrstice %/% nBloki
    matrika = matrix(nrow = nVrstice, ncol = nStolpci)
    stolpec0 = 0
    stolpec1 = 0
    vrstica = 0
    nStolpci = 0
    for (i in 1:length(vrstice)){
        if (grepl(",[0-9]", vrstice[i])){
            nStolpci = str_count(vrstice[i], ",")
            stolpec0 = stolpec1 + 1
            stolpec1 = stolpec1 + nStolpci
            vrstica = 0
        } else{
            vrstica = vrstica + 1
            v = stri_locate_first(vrstice[i], fixed="]")[1, 1] + 1
            vrsticaStr = strsplit(vrstice[i], "")[[1]]
            vrsticaStr = paste(vrsticaStr[v:length(vrsticaStr)], collapse = "")
            utezi = str_match_all(vrsticaStr, " +([^ ]+)")[[1]]
            nZadetkov = dim(utezi)[1]
            if (nZadetkov != nStolpci){
                print(sprintf("Napacna vrstica %d: pričakoval %d zadekov, dobil %d", vrstica, nStolpci, nZadetkov))
                print(vrstice[i])
                return(NULL)
            }
            matrika[vrstica, stolpec0:stolpec1] = as.numeric(utezi[, 2])
        }
    }
    if (vrstica != nVrstice){
        print("nekaj manjka")
        return(NULL)
    }
    return(matrika)
}

shraniFilter = function(model, datoteka){
    write(capture.output(model$arg.params[[1]]), datoteka)
}

prikaziFilter = function(datoteka=NULL, crka=NULL){
    if (is.null(datoteka)){
        datoteka = sprintf("%sx.csv", crka)
    }
    utezi = preberiUtezi(datoteka)
    n = dim(utezi)[2]
    image(utezi[,n:1])
}
