options(encoding = "UTF-8")
library(caret)

RNGkind(sample.kind = "Rejection")

##############################################################
# funkciji
##############################################################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("pomozno.R")


##############################################
# linearno in kvadratno
##############################################
n = 5000  # malo primerov, veliko znacilk: defekten rang matrike, ko ucimo model ...
m = 3
podatki = narediPodatke(n, m)
podatkiRed2 = razsiriPodatke(podatki, 2)

k = 10
# seeds: za vsako precno preverjanje in vsak nabor parametrov eno seme
# in dodatno seme za koncni model (glej dokumentacijo)
# pri nas le en model na preverjanje, torej k + 1
# Alternativa: lahko podamo kar indekse primerov za vsako podmnozico ...
# Oglej si parameter index
tc = trainControl(method = "cv", number = k,
                  seeds = 1:(k + 1)
                  )
lin = train(y ~ ., data = podatki, trControl = tc, method = "lm")
lin2 = train(y ~ ., data = podatkiRed2, trControl = tc, method = "lm") 

napake = c(lin$results$RMSE, lin2$results$RMSE)
print(sprintf("Bolje deluje %d. model", which(napake == min(napake))))

###########################################################
# Visji redovi
###########################################################
maxRed = 10
# pozor: stevilo spremenljivk hitro raste ...
# DN: koliko jih je pri redu r, ce jih je na zacetku n?
napake = c(napake, rep(0.0, maxRed - 2))
napakeUcna = rep(0.0, maxRed)
napakeUcna[1] = RMSE(predict(lin, newdata = podatki), podatki$y)
napakeUcna[2] = RMSE(predict(lin2, newdata = podatkiRed2), podatki$y)

for (red in 3:maxRed){
    podatkiRed = razsiriPodatke(podatki, red)
    lin = train(y ~ ., data = podatkiRed, trControl = tc, method = "lm")
    napake[red] = lin$results$RMSE
    napakeUcna[red] = RMSE(predict(lin, newdata = podatkiRed), podatki$y)
}

mini = min(napake, napakeUcna) * 0.95
maxi = max(napake, napakeUcna) * 1.05

plot(1:maxRed, napake,
     xlab = "red", ylab = "RMSE",
     type="o", col="blue",
     ylim=c(mini, maxi))
lines(1:maxRed, napakeUcna,
     xlab = "red", ylab = "RMSE",
     type="o", col="red")
legend(x = "topright",
       legend = c("prečno preverjanje", "učna množica"),
       col = c("blue", "red"), lty = 1, lwd = 1)
# Opomba: to, kateri red je najboljsi, je precej odvisno od kolicine suma ...

######################################
# kNN
######################################
n = 500
m = 3
podatki = narediPodatke(n, m)
print("Naslednje zna malo trajati (veliko k-jev)")
kji = 1:448
napake = rep(0.0, length(kji))
napakeUcna = rep(0.0, length(kji))

tc = trainControl(method = "cv", number = k,
                  index = createFolds(podatki$y, k=k, returnTrain=TRUE)
                  )
knn = train(y ~ ., data = podatki, trControl = tc, method = "knn",
            tuneGrid = data.frame(k = kji))

napake = knn$results$RMSE
for (i in 1:length(kji)){
    knn = train(y ~ ., data = podatki, trControl = tc, method = "knn",
                tuneGrid = data.frame(k = kji[i]))
    napakeUcna[i] = RMSE(predict(knn, newdata = podatki), podatki$y)
}

mini = min(napake, napakeUcna) * 0.95
maxi = max(napake, napakeUcna) * 1.05

plot(kji, napake,
     xlab = "k", ylab = "RMSE",
     type="o", col="blue",
     ylim=c(mini, maxi))
lines(kji, napakeUcna,
      xlab = "k", ylab = "RMSE",
      type="o", col="red")
legend(x = "bottomright",
       legend = c("prečno preverjanje", "učna množica"),
       col = c("blue", "red"), lty = 1, lwd = 1)
# Opomba: to, kateri red je najboljsi, je precej odvisno od kolicine suma ...
iOpt = which.min(napake)
print(sprintf("Najboljši k: %d (RMSE: %.4f)", kji[iOpt], napake[iOpt]))
