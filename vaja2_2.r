library(caret)


napaka = function(y, y_napovedano){
    mean(y == y_napovedano)
}

narediFormulo = function(imena){
    as.formula(paste("y ~", paste(imena, collapse = "+")))
}

kakovostModela = function(model, podatki){
    napovedi = predict(model, newdata = podatki)
    napaka(podatki$y, napovedi)
}

set.seed(123)


# poskusi tudi z majhnim N, npr. 500
N = 5000
m = 10

xs = matrix(rnorm(m * N), ncol = m)
# odpravi osamelce: poskusi tudi brez tega
xs = pmin(xs, 1)
xs = pmax(xs, -1)

y = 1 + xs[,1] - 2 * xs[,2] + (xs[,4] * xs[,5] - xs[,3] * xs[,6]) / 3 + rnorm(N, 0, 0.01)

podatki = as.data.frame(xs)
podatki$y = y
stolpci = character(m + 1)
for (i in 1:m){
    stolpci[i] = sprintf("x%d", i)
}
stolpci[m + 1] = "y"
colnames(podatki) = stolpci


y = podatki$y
enke = y > mean(y)
y[enke] = 1
y[!enke] = 0
podatki$y = y

podatki2 = podatki
podatki2$y = as.factor(podatki2$y)

ucniIndeksi = createDataPartition(podatki$y, p = 0.8, list=FALSE)
ucna1 = podatki[ucniIndeksi,]
testna1 = podatki[-ucniIndeksi,]
ucna2 = podatki2[ucniIndeksi,]
testna2 = podatki2[-ucniIndeksi,]

model1 = train(y ~ ., data=ucna1, method="lm")
model2 = train(y ~ ., data=ucna2, method="glm")

n1 = predict(model1, testna1)
theta = 0.5
n1[n1 > theta] = 1
n1[n1 <= theta] = 0
n1 = as.factor(n1)
n2 = predict(model2, testna2)

print(sprintf("Tocnost linearne regresije: %.3f",
              napaka(testna1$y, n1)))
print(sprintf("Tocnost logisticne regresije: %.3f",
               napaka(testna2$y, n2)))