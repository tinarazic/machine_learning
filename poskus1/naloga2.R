# 2 Linearna regresija

library(caret)
library(Metrics)
source("funkcije2.R")
seme = 64
podatki = naloziPodatke(seme);

# 2.1 Osnovna naloga

ucniIndeksi = createDataPartition(podatki$y, p = 0.8,list=FALSE)

ucna = podatki[ucniIndeksi,]
testna = podatki[-ucniIndeksi,]

model0 = train(y ~ ., data=ucna, method="lm")
napovedi = predict(model0, testna)
e.m <- rmse(testna$y, napovedi)
e.m

# 2.2 Koristne znacilke
koef = model0$finalModel$coefficients
sort.koef <- sort(abs(koef), decreasing = TRUE)
sort.koef
znacilke <- names(sort.koef)
znacilke

narediFormulo = function(imena){
  as.formula(paste("y ~", paste(imena, collapse = "+")))
}

e.k = 1000000
for (k in 1:length(znacilke)){
  formula = narediFormulo(znacilke[1:k])
  print(formula)
  model.k = train(formula, data=ucna, method="lm")
  napovedi.k = predict(model.k, testna)
  e.k = rmse(testna$y, napovedi.k)
  print(k)
  print(e.k)
  if (e.k <= 1.1*e.m) {
    break
  }
}

# 2.3 Dodatne znacilke
znacilke3 = names(podatki[, 1:20])
formula3 = "y ~ "
for (i in znacilke3){
  if (i == znacilke3[length(znacilke3)]){
    formula3 = paste(formula3, i, "+ I(", i, "^2)")
  }
  else {formula3 = paste(formula3, i, "+ I(", i, "^2) +")}
}
formula3

model3 = train(as.formula(formula3), data=ucna, method="lm")
napovedi3 = predict(model3, testna)
e.m3 <- rmse(testna$y, napovedi3)
e.m3


# 2.4 Regularizacija
# ?????????????????????

# 2.5 Kako velik lambda?
# ?????????????????????

