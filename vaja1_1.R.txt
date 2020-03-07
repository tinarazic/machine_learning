library(caret)
data(iris)

###############################################################
#
# Razdeli podatke
#
###############################################################
# Na roke?

# st_primerov = dim(iris)[1]
# st_ucna = round(st_primerov * 4 / 5)
# ucna = iris[1:st_ucna,]
# testna = iris[(st_ucna+1):st_primerov,]

# Ne tako dobra ideja: vse setose in versicolore bodo v ucni mnozici ...

# Raje caret :)
indeks_ucna = createDataPartition(iris$Species, p=4/5, list=FALSE)
ucna = iris[indeks_ucna,]  # list=FALSE, da se tukaj pri indeksiranju ne sesuje
testna = iris[-indeks_ucna,]  # negativen indeksi = vzemi vse razen njih


##############################################################
#
# Naucimo se model za take in drugacne k
#
##############################################################

# osnovni klic
# natrenirano = train(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length, data=ucna, method="knn")

# kdo bo pa pisal vsa imena stolpcev na roke ...
natrenirano = train(Species ~ ., data=ucna, method="knn")  # . = vsi stolpci v podatkih, razen ze omenjenih v formuli
print(natrenirano)

# ko ne maramo nekaterih spremenljivk: uporabi vse, razen Petal Length, Petal Width (in Species)
# natrenirano = train(Species ~ . -Petal.Length -Petal.Width, data=trainData, method="knn")

# Kaj se zgodi, ce ne uporabimo argumenta method? Naucimo se modela z metodo RF, ki jo bomo spoznali kasneje ...

# Kako prepricati caret, da preizkusi samo k = 1 (ali samo k = 1 in k = 3)
# Dokumentacija pravi, da tako:
# natrenirano = train(Species ~ ., data=trainData, k=1, method="knn")

# ampak tezava je, da lahko tam nastavimo samo tiste parametre, ki jih metoda train
# ne poskusi nastaviti ze sama. V nasem primeru moramo podati zelene vrednosti
# s parametrom tuneGrid:
# natrenirano = train(Species ~ ., data=ucna, method="knn", tuneGrid = data.frame(k = 1))

# Privoscimo si ....
# natrenirano = train(Species ~ ., data=ucna, method="knn", tuneGrid = data.frame(k = 1:120))
# plot(natrenirano)
# plot(natrenirano$results$k, natrenirano$results$Kappa, type="o", col="blue", xlab="k", ylab="Kappa")
# Hm, kaj pa sploh je Kappa ? ;)
# Pojasni, zakaj tocnost pade ko je k vse vecji

# Se malce goljufije za boljse rezultate
# hihi = ucna;
# hihi$kopijaSpecies = hihi$Species;
# natrenirano = train(Species ~ ., data=hihi, method="knn")

########################################################
#
# Napovejmo vrednosti na testni mnozici
#
########################################################
napovedi = predict(natrenirano, newdata = testna)  # in ne natrenirano$finalModel
n_pravilnih = sum(napovedi == testna$Species)
print(sprintf("Tocnost modela na testni: %.3f", n_pravilnih / length(napovedi)))

# napovedi_ucna = predict(natrenirano, newdata = ucna)
# n_pravilnih_ucna = sum(napovedi_ucna == ucna$Species)
# print(sprintf("Tocnost modela ucni: %.3f", n_pravilnih_ucna / length(napovedi_ucna)))


###############################################################################################################
