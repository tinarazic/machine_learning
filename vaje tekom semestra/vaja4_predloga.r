options(encoding = "UTF-8")

library(caret)  # ucenje
library(earth)  # podatki
library(pROC)   # krivulje1
library(PRROC)  # krivulje2

data(etitanic) # v osnovi je survived numericna ...

# K: optimalna točka levo zgoraj, noben negativen primer ni identificiarn kot pozitiven. Bližje ko tmos temu delu boljše je.

#############################################################################
# 1. SPREMENI survivied v faktor z vrednostma P (pozitivni) in N (negativni)
#############################################################################
data(etitanic)
podatki <- etitanic
podatki$survived[podatki$survived==1] <- "P"
podatki$survived[podatki$survived==0] <- "N"
podatki$survived <- as.factor(podatki$survived)

################################################################################
# 2. Z uporabo funkcije createFolds naredi stratificirane ucne mnozice
# Ker bo treba te indekse podatkniti v trainControl, uporabi returnTrain = TRUE
# (poglej dokumentacijo za trainControl)
################################################################################

k <- 20

cvIndex = createFolds(podatki$survived, k=k, returnTrain=TRUE)

###################################################################################
# 3. Definiraj train control objekt za stratificirano 20-kratno precno preverjanje.
# Uporabi cvIndex iz prejšnje naloge. Ustrezno nastavi tudi
# - classProbs (za izračun krivulj)
# - savePredictions (za izračun krivulj)
# Poskusi nastaviti tudi k ;)
###################################################################################

# K: TRAINcontrol pri index pričakuje indexe v učni množici 
# cv indeks bo seznamd olžine 20 kar tudi določa sam k

trC =  trainControl(method = "cv", classProbs = TRUE, savePredictions = TRUE,
                    index = cvIndex)

###########################################################
# 4. Za 1 <= k <= 20 (kar za vse naenkrat) naredi knn-model
###########################################################
kji = 1:20

model <- train(survived ~ ., data = podatki, method = "knn",trControl = trC,
               tuneGrid=data.frame(k=kji))

#################################################################################################
# 5. Napiši funkcijo narisiROC(model, k), ki sprejme model iz prejšnje točke
# in nariše ROC krivuljo za dani k.
# Pomagaj si z
# - model$pred
# - funkcijo roc (iz paketa pROC):
# Funkcija roc generira objekt, ki ga lahko narišemo, ker vsebuje vse potrebno za risanje
# ROC krivulje. Sprejme dva parametra:
# - vektor dejanskih vrednosti,
# - vektor verjetnosti za dani razred (za enega od razredov).
# Ta funkcija ima tudi pomembne imenovane parametre:
#   - levels: seznam dveh elementov, prvi je ime NEGATIVNEGA, drugi ime POZITIVNEGA razreda
#   - direction: če '<', potem majhne vrednosti spadajo v negativni razred, velike pa v pozitivni.
#                če '>', potem velike vrednosti spadajo v negativni razred, majhne pa v pozitivni.
# Toplo priporočamo, da VEDNO uporabi funkcije roc VEDNO podate direction in NE zaupate avtomatiki
#
# Alternativa funkciji roc je roc.curve (iz paketa PRROC), ki tudi sprejme dva parametra:
# - scores.class0: verjetnosti za pozitivni razred,
# - scores.class1: verjetnosti za negativni razred.
# Dodatno pri roc.curve uporabi še curve = TRUE.
##################################################################################################

narisiROC = function(model, k){
    napoved <- model$pred[model$pred$k==k, ]
    rocObj = roc(napoved$obs, napoved$P, direction='<', levels=c('N', 'P'))
    plot(rocObj)
    print(auc(rocObj))
    # ali to
    roc_obj<- roc.curve(scores.class0 = napoved$P[napoved$obs=='P'], scores.class1 = napoved$P[napoved$obs=='N'], curve=TRUE)
    plot(roc_obj)
}

narisiROC(model, 20)

########################################################################
# 6. Napiši funkcijo narisiPR(model, k), ki stori enako kot narisiROC,
# le da nariše PR-krivuljo.
#
# Ponovno si lahko pomagaš s funkcijo pr.curve.
########################################################################

narisiPR = function(model, k){
    napoved <- model$pred[model$pred$k==k, ]
    prObj <- pr.curve(scores.class0 = napoved$P[napoved$obs=='P'], scores.class1 = napoved$P[napoved$obs=='P'], curve=TRUE)
    plot(prObj)
}

narisiPR(model, 20)

########################################################################
# 7. Za vse izbrane k nariši ROC in PR krivuljo za dobljeni knn model
########################################################################

for(k in kji){
  napoved <- model$pred[model$pred$k==k, ]
  # roc curve 
  roc_obj<- roc.curve(scores.class0 = napoved$P[napoved$obs=='P'], scores.class1 = napoved$P[napoved$obs=='N'], curve=TRUE)
  plot(roc_obj)
  
  # pr curve
  pr_obj <- pr.curve(scores.class0 = napoved$P[napoved$obs=='P'], scores.class1 = napoved$P[napoved$obs=='N'], curve=TRUE)
  plot(pr_obj)
}

# K:
# 3. Ugotovili smo da preciznost in priklic je vedno odgovor NE  ampak mogoče pa so. 
# Optimalen modle bo šel naravnost v desno. Pri pr krivulji je odvisno od podatkov. 
# ROC krivulaj pa je dost lepa. Iz samih definicij TPR in FPR se da pokazat, da pri pragu 0, bomo ziher vse napovedal in ko prag večamo, bo delež pravilno papovedanih pozitivnih stalno padal in delež ta drugih bo pa stalnot udi padal.
# Števac se spreminjata direktno s pragom. Imenovalca pa vedno ostajata ista.BREZZVEZE odgovor je NE!!
# zaključek: ROC krivulja ima boljšo interpretacijo, nista niti konkavni niti konveksni.

# 4.
# Npr test az zasznavanje okužb, če bi želeli čist vse okužene odkrit, se odločimo za preklic. Ker bi želeli vse pozitivne napovedat kot pozitivne in za false positive se niti ne sekiramo. 
# Primer za nasportno smer pa je mogoče iskanje kandidatnih spojin za neke nadaljne kemijske teste. Imammo ogromno molekul in nas zanima ali je stavr strupena,...
# Glede na to da pri strojnem iščemo samo kandidate in jih potem predlagamo kemikom. Bi v tem primeru se raje osredotočili an natačnost. To bi bil primer kjer je natančnost bolj primerna, priklic pa niti ni toliko, ker če napovemo dve v redu zdravili je še vedno bolje kot 

###############################################################################
# 8. Napišite funkcijo, ki na podatkih etitanic natrenira knn model
# (uporabi kar isti trainControl kot prej (ali pa zmanjšaj k, da bo hitreje),
# nujno pa spremeni summaryFunction),
# pri čemer se najboljši k izbere glede na ploščino pod ROC krivuljo. Oglej si
# -argument metric metode train,
# - argument summaryFunction metode trainControl (in twoClassSummary)
# Funkcija naj vrne optimalni k.
###############################################################################

trC2 = trainControl(method = "cv", classProbs = TRUE, savePredictions = TRUE,
                  index = createFolds(podatki$survived, k=5, returnTrain=TRUE), summaryFunction = twoClassSummary)

model.ROC <- train(survived ~ ., data = podatki,
              method = "knn",
              metric = "ROC",
              trControl = trC2,
              tuneGrid=data.frame(k=1:20))

model.ROC$finalModel$k # k = 3


##################################################################################
# 9. Stori enako še za priklic.
# (Priklic v ang. govorečih deželah poznajo pod imenoma recall in sensitivity ...)
##################################################################################

model.priklic <- train(survived ~ ., data = podatki,
              method = "knn",
              metric = "Sens",
              trControl = trC2,
              tuneGrid=data.frame(k=1:20))

model.priklic$finalModel$k # k = 20

#################################################################
# 10. Stori enako še za natančnost. Pomagaš si lahko s
# funkcijo izracunajNatancnost(data, levels, ...), ki jo podtakneš v
# summaryFunction (ustrezno nastavi tudi metric)!
# ###############################################################
izracunajNatancnost = function(data, levels, ...){
    # Predpostavljamo, da je levels[1] negativni, levels[2] pa pozitivni razred.
    # To se ujema s funkcijo twoClassSummary.
    predictedPositive <- sum(data$pred==levels[2])
    truePositive <- sum(data$pred==data$obs & data$pred==levels[2])
    precision <- truePositive / predictedPositive
    names(precision) <- 'Precision'
    precision
}

trC.natancnost = trainControl(method = "cv", classProbs = TRUE,savePredictions = TRUE,
                  index = createFolds(podatki$survived, k=5, returnTrain=TRUE), summaryFunction = izracunajNatancnost)

model <- train(survived ~ ., data = podatki,
               method = "knn",
               metric = "Precision",
               trControl = trC.natancnost,
               tuneGrid=data.frame(k=1:20))

model$finalModel$k # k = 6


###############################################################################
# 11. Izberi najboljšo vrednost parametra k še glede ne povsem standardno mero:
# tj. kot 2 N1 + N2, kjer je N_i napaka tipa i
###############################################################################

napaka <- function(data, levels, ...){
  # napaka tipa 2 = "false negative"
  # napaka tipa 1 = "false positive"
  falseNegative <- sum(data$pred==levels[1] & data$obs == levels[2])
  actualPositive <- sum(data$obs==levels[2])
  fnr <- falseNegative / actualPositive
  falsePositive <- sum(data$pred==levels[2] & data$obs == levels[1])
  actualNegative <- sum(data$obs==levels[1])
  fpr <- falsePositive / actualNegative
  
  cost = 2 * fpr + fnr
  names(cost) <- 'cost'
  cost
}

trC.napaka = trainControl(method = "cv",classProbs = TRUE,savePredictions = TRUE,
                  index = createFolds(podatki$survived, k=5, returnTrain=TRUE), summaryFunction = napaka)

model <- train(survived ~ ., data = podatki,
               method = "knn",
               metric = "cost",
               maximize = "false", # PAZI!!! V tem primeru minimiziramo metriko!!!
               trControl = trC.napaka,
               tuneGrid=data.frame(k=1:20))

model$finalModel$k 

# K:
# Drugi del - 3.vprašanje:
# Pri katerih nalogah v resničnem življenju se splača bolj utežiti N1 in pri katerih N2? 
# Kaj je bolj važno za npr. teste, ki zaznavajo okužbe?
# Včasih nas bodo predvsem zanimal primeri ko ne smemo nobenega pozitivnega narobe napovedat npr okužbe. 
# Če pa se želimo usmerit na negativne primere, da zares dobro negativne zadanemo, to bi bilo pa mogoče spet takrat ko hočemo,
# če bo veliko napovedanih pozitivnih bodo kemiki za nami imeli veliko delo, ker bodo vse pozitivne preizkusit. 
# V tem primeru so tisti ki jih napovemo ponesreč za pozitivne, kar nezaželjeni.
