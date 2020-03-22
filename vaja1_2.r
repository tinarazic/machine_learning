library(caret)

# Tole: podatki = read.csv("podatki.csv") bo prineslo cudne znake, npr â€ś
# Resitev:  fileEncoding = "UTF-8"
podatki_utf8 = read.csv("podatki.csv", fileEncoding = "UTF-8")

# str(podatki) pokaze, da imamo nekaj narekovajev, saj x4 in x6 trenutno nista numericni
# View(podatki) razkrije se, da je stolpec 4 zapisan v dveh jezikih
# Nacrt:
# 1) znebiti se narekovajev (lahko tudi pred nalaganjem v R)
# 2) YES --> DA, NO --> NE

podatki_utf8$X4 = gsub('“|”', '', as.character(podatki_utf8$X4))
podatki_utf8$X6 = gsub('“|”', '', as.character(podatki_utf8$X6))

# Ali kar v obeh stolpcih hkrati:
# podatki_utf8[, c(5, 7)] = apply(podatki_utf8[,c(5, 7)],
#                                 2,
#                                 function(x) {
#                                     gsub("“|”", "", as.character(x))
#                                           }
#                                 )

podatki_utf8$X4[podatki_utf8$X4 == 'YES'] = 'DA'
podatki_utf8$X4[podatki_utf8$X4 == 'NO'] = 'NE'
# Ce bi imeli x4 kot factor, se s klicem
# podatki_utf8$X4 = droplevels(podatki_utf8$X4)
# znebimo "mogocih" vrednosti x4, ki jih ni vec (YES, NO)

podatki_utf8$X6 = as.numeric(as.character(podatki_utf8$X6))

##########################################################
#
# Natrenirajmo
#
##########################################################

# Spodnje vrstice povzrocijo opozorilo:
# You are trying to do regression and your outcome only has two possible values
# Are you trying to do classification? 
# If so, use a 2 level factor as your outcome column.
# trainIndex = createDataPartition(podatki_utf8$Y, p=0.8, list=FALSE)
# trainData = podatki_utf8[trainIndex,]
# testData = podatki_utf8[-trainIndex,]
# knnFit = train(Y ~ ., data=trainData, method="knn")

# Upostevajmo (pa se stolpec X ignorirajmo, ker sluzi le kot ID)
podatki_utf8$Y = as.factor(podatki_utf8$Y)
trainIndex = createDataPartition(podatki_utf8$Y, p=0.8, list=FALSE)
trainData = podatki_utf8[trainIndex,]
testData = podatki_utf8[-trainIndex,]
knnFit = train(Y ~ . -X, data=trainData, method="knn")

napovedi_ucna = predict(knnFit, newdata = trainData)
napovedi_testna = predict(knnFit, newdata = testData)
tocnost1 = sum(napovedi_ucna == trainData$Y) / length(napovedi_ucna)
tocnost2 = sum(napovedi_testna == testData$Y) / length(napovedi_testna)
print(sprintf("Tocnost ucna: %.2f; Tocnost test: %.2f", tocnost1, tocnost2))

# Tocnost (ocenjena) je dokaj slaba,
# ker v resnici upostevamo samo X6 (skala!)

podatki_utf8$X2 = podatki_utf8$X2 / max(podatki_utf8$X2)
podatki_utf8$X3 = podatki_utf8$X3 / (max(podatki_utf8$X3) - min(podatki_utf8$X3))
podatki_utf8$X5 = podatki_utf8$X5 / max(podatki_utf8$X5)
podatki_utf8$X6 = podatki_utf8$X6 / max(podatki_utf8$X6)

trainData = podatki_utf8[trainIndex,]
testData = podatki_utf8[-trainIndex,]
knnFit = train(Y ~ . -X, data=trainData, method="knn", tuneGrid = data.frame(k = 1:30))

napovedi_ucna = predict(knnFit, newdata = trainData)
napovedi_testna = predict(knnFit, newdata = testData)

tocnost1 = sum(napovedi_ucna == trainData$Y) / length(napovedi_ucna)
tocnost2 = sum(napovedi_testna == testData$Y) / length(napovedi_testna)
print(sprintf("Tocnost ucna: %.2f; Tocnost test: %.2f", tocnost1, tocnost2))
