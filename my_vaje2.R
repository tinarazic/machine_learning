# VAJE 2

library(caret)
set.seed(123) # nastaviš seme, da bodo vedno enake številke

# PRVI DEL 
# 1. Skonstruiraj umetno množico podatkov, na kateri boš testiral/a metodo linearne regresije. Množica naj ima vsaj 10 spremenljivk.

x1 <- rnorm(500,0,1)
x2 <- rnorm(500,2, 4)
x3 <- runif(500)
x4 <- runif(500, 2, 4)
x5 <- rnorm(500,0,1)
x6 <- rnorm(500,1.5,4)
x7 <- rnorm(500,100, 2)
x8 <- runif(500, 2, 20)
x9 <- runif(500, 0.5,1)
x10 <- rnorm(500,500,1)

e <- rnorm(500, 0, 0.2)

y = 10 + x1 + 0.5*x2 +2*x3*x4 + 1/3*(x5 -x6 + x7) + e

podatki <- data.frame(x1 = x1, x2 = x2, x3 = x3,
                     x4 = x4, x5 = x5, x6 = x6,
                     x7 = x7, x8 = x8, x9 = x9,
                     x10 = x10, y = y)

# 2. Razdeli podatkovje na uèno in testno množico v razmerju 4:1
ucni_indeksi = createDataPartition(podatki$y, p = 0.8, list=FALSE)
ucna = podatki[ucni_indeksi,]
testna = podatki[-ucni_indeksi,]

# 3. Konstruiraj linearni model za napoved ciljne spremenljivke (train(..., method="lm")). Kako posamezne spremenljivke vplivajo na model, tj. katere spremenljivke so pozitivno korelirane s ciljno spremenljivko? Katere spremenljivke so negativno korelirane s ciljno spremenljivko?
natrenirano = train(y ~.,data = ucna, method="lm")
summary(natrenirano)
# summary ti vse vrže, s spodnjim pa dobiš samo beta koeficiente
natrenirano$finalModel$coefficients

# 4. Z izbiro nazaj izberi najboljših 5 spremenljivk. Katere spremenljivke je model izbral? Se to ujema z najbolj vplivnimi spremenljivkami modela iz prejšnje toèke?

# izbira poteka tako, da naredimo nov model pogledamo katera je brezzvzena in vržemo ven in potem nov model z n-1 sprmeenljivkmai in vržemo spet eno in to ponavljamo dokler ne pridemo do npr 5 spremenljivk
najbolj_vplivne <- sort(natrenirano$finalModel$coefficients, decreasing = TRUE)[1:6]

# s tem naèinom zaupamo modelu, da zna že sma doloèit katera je najbolj vplivna. 
# za bolj splošne napišemo funkcijo

napaka = function(y, y_napovedano){
  sqrt(mean((y-y_napovedano)^2))
}

narediFormulo = function(imena){
  as.formula(paste("y ~", paste(imena, collapse = "+")))
}

kakovostModela = function(model, podatki){
  napovedi = predict(model, newdata=podatki)
  napaka(podatki$y, napovedi)
}

znacilke = colnames(podatki)
znacilke = znacilke[znacilke != "y"]


while(length(znacilke) > 5){
  napake = rep(0, length(znacilke))
  for (i in 1:length(znacilke)){
    nove = znacilke[znacilke != znacilke[i]]
    formula = narediFormulo(nove)
    model = train(formula, data=ucna, method="lm")
    napake[i] = kakovostModela(model, testna)
  }
  zaOdmet = znacilke[which.min(napake)]
  znacilke = znacilke[znacilke != zaOdmet]
  print(sprintf("Odvrgli smo %s", zaOdmet))
}


# DRUGI DEL # NEKAJ NI PRAV! glej njegove zapiske!!

# 1. Skonstruiraj umetno množico podatkov, na kateri boš testiral/a metodo logistiène regresije. Ciljna spremenljivka naj bo nominalna z vrednostma "0" in "1".

# moj naèin
#podatki2 <- podatki[,1:10]
#y2 <- sample(c(0,1), replace=TRUE, size=500)
#podatki2 <- cbind(podatki2, y2)

y = podatki$y
enke = y > mean(y)
y[enke] = 1
y[!enke] = 0
podatki$y = y

podatki2 = podatki
podatki2$y = as.factor(podatki2$y)

# 2. Razdeli podatkovje na uèno in testno množico (train test in test set) v razmerju 4:1.
ucni_indeksi2 = createDataPartition(podatki$y, p = 0.8, list=FALSE)
ucna1 = podatki[ucni_indeksi2,]
testna1 = podatki[-ucni_indeksi2,]
ucna2 = podatki2[ucni_indeksi2,]
testna2 = podatki2[-ucni_indeksi2,]

# 3. Konstruiraj linearni model za napoved ciljne spremenljivke (train(..., method='glm')).
model1 <- train(y ~.,data = ucna1, method="glm")
model2 <- train(y ~.,data = ucna2, method="lm")

n1 = predict(model1, testna1)
n1[n1 > 0.5] = 1
n1[n1 <= 0.5] = 1
n1 = as.factor(n1)

n2 = predict(model2, testna2)

napaka = function(y, y_napovedano){
  sqrt(mean((y-y_napovedano)^2))
}

napaka(testna1$y, n1)
napaka(testna2$y, n2)

