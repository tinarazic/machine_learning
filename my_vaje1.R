# PRVI DEL
# 1. naloga
library(caret)
data(iris)

# 2. naloga
ucna_poskus = iris[1:120,]
testna_poskus = iris[121:150,]
# ni v redu delitev, ker prvih 50 primerov je en razred, mi bi pa radi, da sta ucna in testna približno enkao porazdlejeni

# 1. možnost najprej premešamo
# 2. možnost: uporabimo kakšno vgrajeno funckijo

ucni_indeksi = createDataPartition(iris$Species, p = 0.8, list=FALSE)
# èe bi imel en razred 100 primerov, druga dva pa po 50, želel bi da je nekako enakomerno, zato mu damo naš testni stolpec
# p =0.8 ker razmerje 4:1
# list = False, da vrne kot matriko, da lahko uporabimo indeksiranje:

ucna = iris[ucni_indeksi,]
testna = iris[-ucni_indeksi,]

natrenirano = train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                    data = ucna, method="knn")

# uporabi vse vhodne atribute razen Sepal.Width
natrenirano2 = train(Species ~ . - Sepal.Width, 
                    data = ucna, method="knn")
# rf = random forest metodo uporabi
natrenirano2 = train(Species ~ . - Sepal.Width, 
                     data = ucna)

# kappa podobno kot toèno, le da ima popravek še za nakljuèno vrednost

# 3. naloga
k <- natrenirano$finalModel$k

# 4. naloga 
# tuning parametrs parametri za nsatavljanje metode
# tuneGrid = NULL privzeto, treba podat kot dataframe
#natrenirano_parametri = train(Species ~ .,
#                    data = ucna, method="knn", tuneGrid = data.frame(k = 1:30))
# veè sosedov je slabš je
print(plot(natrenirano_parametri))


# 5. naloga
napovedi <- predict(natrenirano, newdata = testna)
#toènost je delež pravilnih napovedi
# èe se ujema s pravimi damo 1, èe ne 0, seštejemo in delimo z št
n_pravilnih = sum(napovedi == testna$Species)
tocnost = n_pravilnih/dim(testna)[1]

# èe imamo k = 1, je najbližji sosed kar on sam in napaka na ucni množici bo  VEDNO 1, na testni pa slabše


# DRUGI DEL
podatki_neurejeni = read.csv("U:/strojno ucenje/podatki.csv", fileEncoding = "UTF-8")
View(podatki_neurejeni)
str(podatki_neurejeni)

podatki <- podatki_neurejeni
podatki$X4 <- gsub("NE", "NO", podatki$X4)
podatki$X4 <- gsub("DA", "YES", podatki$X4)
podatki$X4 <- gsub("“", "", podatki$X4)
podatki$X4 <- gsub("”", "", podatki$X4)
podatki$X6 <- as.numeric(as.character(podatki$X6))
podatki$Y <- as.factor(podatki$Y)


ucni_indeksi2 = createDataPartition(podatki$Y, p = 0.8, list=FALSE)
ucna2 = podatki[ucni_indeksi2,]
testna2 = podatki[-ucni_indeksi2,]

natrenirano2 = train(Y ~ .,data = ucna2, method="knn")
k2 <- natrenirano2$finalModel$k
# k2 = 7


#X NE BI SMELI UPORABITI, ker je kot nek števec
natrenirano2 = train(Y ~ .-X,data = ucna2, method="knn")

# normaliziramo stolpce, ker drugaèe x6 najbolj vpliva
# MORALA ZGODBE: podatki niso urejeni
