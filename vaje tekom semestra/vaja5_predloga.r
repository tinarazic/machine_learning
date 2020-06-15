library(caret)
RNGkind(sample.kind = "Rejection")
set.seed(12321)

##########################################################################################
# 1. Zapiši funkcijo nakljucniNominalniVektor(domena, verjetnosti, N),
# ki sprejme
# - vektor domena, npr. c("A", "B", "C"),
# - vektor verjetnosti, npr. c(1/2, 1/4, 1/4); velja torej sum(verjetnosti) = 1,
# - število n
# in vrne naključno permutacijo vektorja dolžine n, za katerega velja
# P(domena[i]) = verjetnosti[i].
#
# Pomagaš si lahko s funkcijama
# - rep (npr. rep("A", 4) vrne c("A", "A", "A", "A")),
# - sample (npr. za x = c("A", "A", "B", "B") bo sample(x) vrnil naključno permutacijo x).
#
# Eden od možnih vrednosti za ukaz
# nakljucniNominalniVektor(c("x", "y", "z"), c(1/2, 1/3, 1/6), 6) je tako
# c("x", "z", "x", "y", "y", "x").
##########################################################################################

nakljucniNominalniVektor = function(domena, verjetnosti, n){
  vektor <- vector()
  dolzina <- length(domena)
  for (i in 1:dolzina){
    ponovitev = n*verjetnosti[i]
    vektor <- c(vektor, rep(domena[i], ponovitev))
  }
  vektor <- sample(vektor)
  return(vektor)
}

test <- nakljucniNominalniVektor(c("x", "y", "z"), c(1/2, 1/3, 1/6), 6)
test
##########################################################################################
# 2. Zapiši funkcijo dodajSum(y, domena, p),
# ki sprejme
# - vektor y, katerega komponente so elementi vektorja domena, npr. y = c("+", "-", "-", ...)
# - vektor domena dolžine 2, npr. c("-", "+"),
# - število p, 0 <= p <= 1, ki predstavlja stopnjo šuma,
# in vrne kopijo z vektorja y, za katero približno velja P(z[i] = y[i]) = p.
#
# Funkcija naj torej z verjetnostjo p (neodvisno) spremeni vsako od komponent v vektorju y.
# Eden od možnih vrednosti za ukaz dodajSum(c("+", "-", "-", "-"), 1/4) je tako
# c("+", "-", "-", "+") (spremenili smo zadnjo komponento)
##########################################################################################
dodajSum = function(y, domena, p){
  dolzina <- length(y)
  st.sprememb <- dolzina*p
  vektor <- y
  for (i in 1:st.sprememb){
    if (vektor[i] == domena[1]){
      vektor[i] = domena[2]
    }
    else {vektor[i] == domena[1]}
  }
  return(vektor)
}

test2 <- dodajSum(c("+", "-", "-", "-"),c("-", "+"), 1/4)
test2

##########################################################################################
# 3. Naredi podatkovje podatki1 z vhodnima spremenljivkama x1 in x2
# ter izhodno spremenljivko y, ki je definirano po naslednjih pravilih:
# - x1 je nominalna in zavzame vrednosti A in B. Velja še P(x1 = A) = 3 / 5
# - x2 je numerična in porazdeljena enakomerno zvezno na intervalu [0, 1]
#
# Možni vrednosti y sta + in - in sta določeni z naslednjima praviloma:
# 1. Če je x1 = B ali x2 > 0.5, potem y = +
# 2. V ostalih primerih je y = -
#
#
# Po zgornjih pravilih naredi podatkovje s 1000 primeri.
# Ko ga narediš, vnesi se šum, tako da 5% primerov zamenjaš vrednost spremenljivke y.
##########################################################################################
N = 1000

x1 <- nakljucniNominalniVektor(c("A", "B"), c(3/5, 2/5), N)
x2 <- rnorm(N)
y <- if (x1 == "B" || x2 >0.5) {"+"} else {"-"}
podatki1 = cbind(x1, x2, y)

##########################################################################################
# 4. Naredi podatkovje podatki2 z vhodnimi spremenljivkami x1, x2, x3 in x4
# ter izhodno spremenljivko y, ki je definirano po naslednjih pravilih:
# - x1 je nominalna in zavzame vrednosti A in B. Velja še P(x1 = A) = 3 / 5
# - x2 je nominalna in zavzame vrednosti C, D in E. Velja še P(x2 = C) = P(x2 = D) = 1 / 3
# - x3 je numerična in porazdeljena enakomerno zvezno na intervalu [0, 1]
# - x4 je numerična in porazdeljena standardno normalno
# 
# Možni vrednosti y sta + in - in sta določeni z naslednjimi pravili:
# 1. Če je x1 = B, potem y = +
# 2. Če 1 ne velja in x2 = D, potem y = -
# 3. Če 1 in 2 ne veljata in x4 > 0 in x3 > 0,2, potem y = +
# 4. Če 1 in 2 ne veljata in x4 <= 0 in x3 > 0,6, potem y = +
# 5. V ostalih primerih je y = -
#
# Po zgornjih pravilih naredi podatkovje s 1000 primeri.
# Ko ga narediš, vnesi se šum, tako da 5% primerov zamenjaš vrednost spremenljivke y.
##########################################################################################
N = 1000


podatki2 = NULL

########################################################################################
# 5. Za vsake od podatkov izgradi drevo z uporabo metode train,
# pri čemer uporabiš method="rpart". Pri gradnji in ocenjevanju najboljših parametrov
# uporabi 10-kratno prečno preverjanje.
# Kateri parameter gradnje dreves smo optimizirali?
#
# Ker bo treba podobno storiti še pri naslednji nalogi, definiraj funkcijo
# treniraj12(podatki, metoda), ki za učenje uporabi method=metoda.
#
# Nariši končni model s pomočjo knjižnice rpart.plot:
# narišeš ga kar z ukazom rpart.plot(končniModel)
#
# Ali se dobljeni drevesi ujemata s konstrukcijo, tj. ali test v notranjih vozliščih
# odražajo pravila, po katerih smo določili y?
########################################################################################
library(rpart.plot)


treniraj12 = function(podatki, metoda){
    
}


model1 = NULL
model2 = NULL



##########################################################################
# 6. Stori enako še z uporabo metode method="rpart2".
# Kateri parameter gradnje dreves smo optimizirali?
# Sta dobljeni drevesi enaki kot prej?
##########################################################################

model1_2 = NULL
model2_2 = NULL


###########################################################################################
# 7. Stori enako še z neposredno uporabo knjižnice rpart:
# rpart(formula, podatki, control=rpart.control(cp=0, maxdepth=10, minsplit=0, ...))
# Pomen parametrov (za preostale si oglej rpart.control):
# - cp: complexity parameter, kot ga že poznamo ->K:  ne kaznuje kompleksnosti
# - maxdepth: največja dovoljena globina, pri čemer je koren na globini 0
# - minsplit: najmanjše število primerov v vsakem notranjem vozlišču, tj. v vozlišču, kjer
#             razdeimo podatke
###########################################################################################

# Poigrajte se npr. s cp parametrom: cp = 0.0 in cp = 0.1

# drevo večje, zaradi šuma

#########################################################################################################
# Opombe:
# 1. Opazili boste, da se za razliko od prej zgoraj npr. ni zgodila
# pretvorba nominalnih v numerične vektorje. Zato so testi v drevesu lepše berljivi:
# npr. x1 = A (in ne x1A = 1 oz. x1B = 0).
#
# 2. Izgubili smo možnost optimizacije. Če želimo optimizirati spremenljivko cp
# ter hkrati določiti še kak drug parameter, postopamo tako:
# Uporabimo train in s spremenljivko cp postopamo kot ponavadi in jo podamo kot data.frame v spremenljivko
# tuneGrid. Ostale parametre v primeru rpart lahko nadzorujemo samo preko parametra control, ki je
# imenovani parameter, ki se v klicu funkcije train poda naprej funkciji rpart.
########################################################################################################
treniraj3 = function(podatki){
    
}

model_rpart1 = treniraj3(podatki1)
model_rpart2 = treniraj3(podatki2)

# rpart.plot(model_rpart1$finalModel)
# rpart.plot(model_rpart2$finalModel)


###############################################################################################
# 8. Ampak kaj pa, ko hočemo optimizirati npr. maxdepth in cp?
# Ideja: za vsak maxdepth, najdi optimalen cp ...
# Ali deluje: JA. Ali je "lepo"? NE.
# Boljša ideja: razširimo caret s svojo metodo:
# https://topepo.github.io/caret/using-your-own-model-in-train.html
# (na sploh je to zelo dober uvod v caret).
# Kot opisuje stran, do katere povezavo najdete zgoraj, je treba načeloma svojo metodo opisati
# s kar precej stvarmi:
# - type: seznam z elementi iz množice {"Classification", "Regression"} - pove, kdaj lahko uporabimo metodo
# - library: seznam potrebnih knjižnic (vseeno nam ni treba vsega napisati na roke, ampak lahko
#            enostavno povežemo več koncev v smiselno celoto)
# - parameters: parametri, ki jih lahko optimiziramo v train
# - fit: funkcija, s katero naučimo model
# - predict: funkcija, s katero napovedujemo
# - prob: funkcija, s katero dobimo verjetnosti razredov
# - grid: funkcija, ki nastavi tuneGrid, če ta ni podan
#
# Te parametre podamo kot seznam (list) argumentu method - za že vgrajene je dovolj
# namesto seznama podati ime (npr. "knn", "rpart" ipd.).
#
# Majceni problem optimizacije dveh parametrov bomo torej napadli s težkimi topovi,
# a se bomo hkrati naučili reševati tudi precej težje probleme. Npr. lahko bi naredili
# svoja drevesa, ki v notranjih vozliščih za izbiro optimalnega testa uporabljajo
# metodo podpornih vektorjev namesto testov oblike x_i < 3.14 ...
#################################################################################################

# najprej lahke stvari
rpartBolje = list(type="Classification",  # regresije ne potrebujemo
                  library="rpart"         # naslonimo se na že obstoječo knjižnico
)

# parameters: potrebujemo ime spremenljivke, tip in - spodobi se - "lepo ime" parametra,
# nato pa to podamo kot data.frame
rpartBolje$parameters = data.frame(
    parameter=c("cpBolje", "maxdepthBolje"),
    class=c("numeric", "numeric"),
    label=c("Izboljsan complexity parameter", "Izboljsana maxdepth")
)

# fit: uporabiti moramo imena parametrov, kot smo jih definirali zgoraj
rpartBolje$fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    tree = rpart(data.frame(y,x),
                 control=rpart.control(maxdepth=param$maxdepthBolje,
                                       cp=param$cpBolje))
    tree = prune(tree, cp=param$cpBolje)
    tree
}

# predict: pomembna, a v našem primeru lahka:
rpartBolje$predict = function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit, data.frame(newdata), type="class")
}


# V našem konkretnem primeru vrednosti prob in grid ne potrebujemo:
# prob je odveč, tuneGrid bomo pa tako ali tako podali.
# Definiramo ju, ker caret brez njiju noče izvesti klica train.
rpartBolje$prob = 0
rpartBolje$grid = 0


#################################################################
# 9. Zgoraj ustrezno popravi rpartBolje (parameters in fit),
# tako da bo metoda omogočala še optimizacijo parametra minsplit.
#################################################################

#######################################################################################################
# 10. Zaradi implementacije fit (pričakuje x in y) tokrat ne bomo uporabili formule,
# ampak bomo podatke razdelili na vhodni in izhodni del. Razdeli podatki1 in podatki2 na
# xs1 in y1 ter xs2 in y2.
#######################################################################################################

xs1 = NULL
xs2 = NULL
y1 = NULL
y2 = NULL

##################################################################################
# 11. Definiraj primerno vrednost tg za tuneGrid parameter.
# Uporabi smiselne vrednosti za cpBolje, maxdepthBolje in minsplitBolje.
# Enostaven tg = data.frame(cpBolje=..., maxdepthBolje = ..., minsplitBolje = ...)
# ni dovolj, saj imamo več kot en parameter. Pomagaj si z expand.grid.
##################################################################################

tg =  data.frame(cpBolje = seq(0,0.09, 0.01), maxdepthBolje = 1:10)  # To je narobe :)

################################################################################
# 12. Nauči se model po metodi rpartBolje. Uporabi 10-kratno prečno preverjanje
# ter tg, kot ga definiraš pri prejšnji nalogi.
# Katere vrednosti parametrov so bile izbrane?
################################################################################

mojModel = NULL
# rpart.plot(mojModel$finalModel)
