########################################################################################
# Domača naloga 2
# 1 Drevesa
########################################################################################

# working directory
#setwd("D:/Dokumenti/FAKS/magisterij/machine_learning/DomacaNaloga2")
#setwd(paste(getwd(),"/DomacaNaloga2", sep=""))

# naložimo knjižnice
library(rpart)
library(caret)

# ukazi za kviz
source("kviz1.r")
seme = 288
set.seed(seme)
n = 10^6
x = runif(n)
x = x[order(x)]
y = runif(n)
model = readRDS("model.rds")
podatki13 = naloziPodatke()
options(digits = 16)


# 1.1 Učinkovit izračun variance

vseKakovosti <- function(y, x){
  # sprejme vrednosti ciljne spremenljivke y in neke vhodne 
  # spremenljivke x primerov, ki dospejo v vozlišče
  # x JE UREJEN!
  # vrne največjo kakovost vseh možnih testov v času O(n)
  n.vijuga <- length(y)
  kakovosti <- rep(0, n.vijuga)
  vsota.y <- sum(y)
  vsota.y2 <- sum(y^2)
  varD = vsota.y2/n.vijuga - (vsota.y/n.vijuga)^2
  delna.vsota.y <- 0
  delna.vsota.y2 <- 0
  for (i in 1:(n.vijuga-1)) {
    # zaradi urejenosti x lahko tako po vrsti y jemljemo
    delna.vsota.y  = delna.vsota.y  + y[i]
    delna.vsota.y2 = delna.vsota.y2 + y[i]^2
    varD1 = delna.vsota.y2/i - (delna.vsota.y/i)^2
    varD2 = (vsota.y2 - delna.vsota.y2)/(n.vijuga-i) - ((vsota.y-delna.vsota.y)/(n.vijuga-i))^2
    kakovosti[i] = varD - (i*varD1 + (n.vijuga-i)*varD2)/n.vijuga
  }
  return(max(kakovosti))
}

a <- vseKakovosti(y, x)
a.kviz <- a * 10^6


# 1.2 Globina drevesa

globina <- function(model){
  # sprejme model, ki ga dobimo s klicem model = train(..., method="rpart", ...)
  # izračuna globino dobljenega drevesa
  # Drevo, ki vsebuje le koren, naj ima globino 1.
  nodes <- as.numeric(rownames(model$finalModel$frame))
  globina <- max(rpart:::tree.depth(nodes)) + 1
  # + 1 ker korena ne šteje zraven in ga tako dodamo
  return(globina)
}

globina.kviz <- globina(model)


# 1.3 Sekajmo drevesa
# glej pravila.r za pomoč


pravilo <- function(nodes, napoved){
  function(x){
    # sprejme tabelo x
    pragovi <- as.numeric(regmatches(nodes,regexpr("-?\\d\\.\\d*",nodes)))
    print(pragovi)
    xi <- regmatches(nodes,regexpr("X\\d",nodes))
    print(xi)
    n <- nrow(x)
    napovedi <- rep(NA, n)
    for (i in 1:n) {
      if (all(x[i, xi] < pragovi)) {
        napovedi[i] <- napoved
      }
    }
    return(napovedi)
  }
}


najdiPrvo = function(model) {
  # sprejme model, ki ga dobimo s klice model = train(..., method="rpart", ...)
  # in iz njega izlušči pravilo, ki ga dobimo, če sledimo testom od korena do najbolj levega lista.
  # vrne funkcijo pravilo(x), ki vrne napoved pripadajočega lista 
  # za tiste vektorje x, ki zadoščajo pogojem, in NULL sicer.
  index.min <- min(which(model$finalModel$frame$var == '<leaf>'))
  # najbolj levi list je prvi <leaf> ce poklicemo model$finalModel$frame
  nodes <- labels(model$finalModel)[2:index.min]
  napoved <- model$finalModel$frame$yval2[index.min]
  return(pravilo(nodes, napoved))
}

pravilo.kviz <- najdiPrvo(model)
napovedi.kviz <- pravilo.kviz(podatki13)
steviloNULL <- sum(sapply(napovedi.kviz,is.na))

