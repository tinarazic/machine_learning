# 3 Variance in se kaj

source("funkcije3.R")
seme = 962
podatki = naloziPodatke(seme)
 
# 3.1 Kdo se najbolj prilega?

ucniIndeksi = createDataPartition(podatki$y, p = 0.8,list=FALSE)

ucna = podatki[ucniIndeksi,]
testna = podatki[-ucniIndeksi,]

e.s <- vector()
for (s in 1:10){
  model.s = train(y ~ poly(x,s, raw = TRUE), data=ucna, method="lm")
  napovedi.s = predict(model.s, testna)
  napaka.s <- rmse(testna$y, napovedi.s)
  e.s <- c(e.s, napaka.s)
}
min(e.s)


