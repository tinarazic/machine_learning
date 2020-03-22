library(caret)


napaka = function(y, y_napovedano){
    sqrt(mean((y - y_napovedano)^2))
}

narediFormulo = function(imena){
    as.formula(paste("y ~", paste(imena, collapse = "+")))
}

kakovostModela = function(model, podatki){
    napovedi = predict(model, newdata = podatki)
    napaka(podatki$y, napovedi)
}


set.seed(123)


# Ena moznost

N = 5000
m = 10

x1 = rnorm(N)
x2 = rnorm(N)
x3 = rnorm(N)
x4 = rnorm(N)
x5 = rnorm(N)
x6 = rnorm(N)
x7 = rnorm(N)
x8 = rnorm(N)
x9 = rnorm(N)
x10 = rnorm(N)

y = 1 + x1 - 2 * x2 + (x4 * x5 - x3 * x6) / 3 + rnorm(N, 0, 0.01)
podatki = data.frame(x1 = x1, x2 = x2, x3 = x3,
                     x4 = x4, x5 = x5, x6 = x6,
                     x7 = x7, x8 = x8, x9 = x9,
                     x10 = x10, y = y)

# Druga moznost

# xs = matrix(rnorm(m * N), ncol = m)
# y = 1 + xs[, 1] - 2 * xs[, 2] + ...
# data = as.data.frame(xs)
# data$y = y
# stolpci = character(m + 1)
# for (i in 1:m){
#     stolpci[i] = sprintf("x%d", i)
# }
# stolpci[m + 1] = "y"
# colnames(data) = stolpci

ucniIndeksi = createDataPartition(podatki$y, p = 0.8,
                                  list=FALSE)
ucna = podatki[ucniIndeksi,]
testna = podatki[-ucniIndeksi,]

znacilke = colnames(podatki)
znacilke = znacilke[znacilke != "y"]

model0 = train(y ~ ., data=podatki, method="lm")

indeksiPoz = which(model0$finalModel$coefficients[-1] > 0)
indeksiNeg = which(model0$finalModel$coefficients[-1] < 0)

print(sprintf("Pozitivno korelirane so %s",
              paste(colnames(podatki)[indeksiPoz], collapse = ", ")
            )
    )
print(sprintf("Negativno korelirane so %s",
              paste(colnames(podatki)[indeksiNeg], collapse = ", ")
            )
    )

print("Zacenjamo z izbiro nazaj")
optNapake = rep(0, length(znacilke))

optNapake[length(znacilke)] = kakovostModela(model0, testna)

while (length(znacilke) > 1){
    napake = rep(0, length(znacilke))
    for (i in 1:length(znacilke)){
        nove = znacilke[znacilke != znacilke[i]]
        formula = narediFormulo(nove)
        # oglejte si, kaj naredi trControl v primeru "none"
        model = train(formula, data=ucna, method="lm", trControl=trainControl(method = "none"))
        napake[i] = kakovostModela(model, testna)
    }
    indeksOptNapake = which.min(napake)
    optNapake[length(znacilke) - 1] = napake[indeksOptNapake]
    zaOdmet = znacilke[indeksOptNapake]
    znacilke = znacilke[znacilke != zaOdmet]
    print(sprintf("Odvrgli smo %s", zaOdmet))
}
print(sprintf("Obdrzali smo: %s", paste(znacilke, collapse = ", ")))

# narisimo graf: napaka(stevilo znacilk)
plot(1:(length(podatki)-1), optNapake, xlab = "stevilo znacilk", ylab="rmse")
print("Znacilke zacetnega modela, urejene po |koeficient|:")
bete = model0$finalModel$coefficients[-1]  # summary(model0$finalModel$coefficients) vrne bolj detaljne podatke
imena = colnames(podatki)
urejenaImena = imena[order(-abs(bete))]
print(sprintf("%s", paste(urejenaImena, collapse = ", ")))
