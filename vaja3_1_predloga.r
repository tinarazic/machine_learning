library(caret)

RNGkind(sample.kind = "Rejection")

##############################################################
# funkciji
##############################################################

narediPodatke = function(n_primerov, n_stolpcev){
  podatki = vector()
  sum = vector()
  for (i in 1: n_stolpcev){
    podatki = cbind(podatki, runif(n_primerov, 1,2))
    sum = rbind(sum, rnorm(1,0,0.1))
  }
  podatki$y <-  c(rep(0,n_primerov))
  for (i in 1:(n_stolpcev - 3)){
    for (j in 1:n_primerov){
      podatki$y[j] = podatki$y[j] + podatki[j, i] + podatki[j,i]*podatki[j, i + 3] + podatki[j,i]*podatki[j,i]*podatki[j, i +2]*podatki[j, i +3]
    }
    
  }
    return(podatki)
}

razsiriPodatke = function(podatki, stopnja){
    if (stopnja == 1){
        return(podatki)
    }
    n_vhodne = length(podatki) - 1
    razsirjeni = data.frame(poly(as.matrix(podatki[, 1:n_vhodne]),
                                 degree=stopnja,
                                 raw=TRUE),  # da izklopimo ortogonalnost (obcutljivost matrike?)
                            y=podatki$y)
}

##############################################
# linearno in kvadratno

podatki = narediPodatke(160,10)
razsirjeni.podatki = razsiriPodatke(podatki, 2)
lm <- train(y ~., podatki, method="lm")
lm.dodatno <-  train(y ~., razsirjeni.podatki, method="lm") 
##############################################
# cv = crossvalidation
tc = trainControl(method = "cv", number = k,
                  seeds = 1:(k + 1)
)

###########################################################
# Visji redovi
###########################################################

######################################
# kNN
######################################
kji = c(1:30)
# pozor: stevilo spremenljivk hitro raste ...
# DN: koliko jih je pri redu r, ce jih je na zacetku n?
napake = rep(0.0, length(kji))
napakeUcna = rep(0.0, length(kji))

tc = trainControl(method = "cv", number = k,
                  index = createFolds(podatki$y, k=k, returnTrain=TRUE)
)


# pri višjih stopnjah polinomov je pa malo drugače. 
# prečnmo preverjanje je tista stvar!
# koliko so ocene optimistične? na začetku dobro deluje, potem pa so razhajanja vedno večja in to da se napaka iz prečnega preverjanja deluje to pomeni da se vedno bolj prekoprilegajo.


# koliko različni K vp0livajo na kvalieto modelov?
# če vzamemo k = 1 bo napaka vedno nič, ker bo sam svoj najbližji sosed, torej bo imel tudi svojo ciljno vrednost
# kot smo pri klasifikaciji uganili ko se k veča se približuje deležu največjega razreda
# če bi bil k tako velik, da bi vse podatke pokril, kaj bo v tem primeru napoved, če vzammeo za sosesede kar vso učno množico
# celotno povrpečje
# kar je za klasifikacijo večinski razred, to je za regresijo povprečje
# za vsak primer smo napovedal kar povprečje vsega in glede na to da potem vzamemo rmse, bomo dobili povprečje minu prava vrednost an kvadrat pa pdo korenom, kar bi moralo biti zelo blizu standardnemu odklonu

#
