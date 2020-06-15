library(caret)
RNGkind(sample.kind = "Rejection")
set.seed(12321)

##########################################################################################
# 1. Zapiši funkcijo nakljucniNominalniVektor(domena, verjetnosti, n),
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
#
# ENG: Implement the method na nakljucniNominalniVektor(domain, probabilities, n)
# (i.e., randomNominalVector(...)) that takes as its input
# - vector domain, e.g., c("A", "B", "C"),
# - vector of probabilities, e.g., c(1/2, 1/4, 1/4); (therefore, sum(probabilities) = 1),
# - number n
# and returns a random permutation of a vector of length n, for which P(domain[i]) = probabilities[i].
#
# Two helpful methods:
# - rep (e.g., rep("A", 4) returns c("A", "A", "A", "A")),
# - sample (e.g., for x = c("A", "A", "B", "B") the call sample(x) retunrs a random permutation of x).
#
# One of the possible outcomes of
# nakljucniNominalniVektor(c("x", "y", "z"), c(1/2, 1/3, 1/6), 6) is thus
# c("x", "z", "x", "y", "y", "x").
##########################################################################################

nakljucniNominalniVektor = function(domena, verjetnosti, n){
    k = length(domena)
    nPojavitev = rep(0, k)  # number of occurrences of each value
    for (i in 1:(k - 1)){
        nPojavitev[i] = round(verjetnosti[i] * n)
        
    }
    # zaradi zaokrožitev moramo zadnjega izračunati posebej, sicer se nam lahko zgodi,
    # da skupno število ne bo n
    # Due to the rounding, the last number of occurences have to be computed separately
    # ensure that the total number of occurrences is n
    nPojavitev[k] = n - sum(nPojavitev)  
    # prazen vektor, ki ga bomo dopolnjevali (empty vector that we are going to extend)
    vektor = c()  
    # Učinkoviteje bi bilo reči npr. vektor = rep("", n) in nato popraviti posamezne odseke
    # It would be more efficient to initialize vector = rep("", n) and then set its subranges
    for(i in 1:k){
        vektor = c(vektor, rep(domena[i], nPojavitev[i]))
    }
    sample(vektor)
}


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
#
# ENG. Implement function dodajSum(y, domain, p) (i.e., addNoise)
# that takes as input
# - vector y, whose components are elements of the vector domain, e.g., y = c("+", "-", "-", ...)
# - vector domain of length 2, e.g., c("-", "+"),
# - number p, 0 <= p <= 1, that represents the level of noise,
# and outputs a copy of y, such that P(z[i] = y[i]) = p (approximately).
#
# Thus, the function should change each component of y with probability p (independently).
# One possible outcome of the call dodajSum(c("+", "-", "-", "-"), 1/4) is
# c("+", "-", "-", "+") (the last component has been modified)
##########################################################################################
dodajSum = function(y, domena, p){
    z = y
    for (i in 1:length(y)){
        if (runif(1) < p){
            # P(runif(1) < p) = p
            if (y[i] == domena[1]){
                z[i] = domena[2]
            } else {
                z[i] = domena[1]
            }
        }
    }
    z
}

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
#
# ENG. Create a dataset podatki1 with input variables x1 and x2, and output variable y.
# The variables are defined as follows:
# - x1 is nominal and can take the values A and B. Additionally, P(x1 = A) = 3 / 5
# - x2 is numeric and is distributed uniformly on the interval [0, 1]
#
# y can take the values + and -, and follows the following rules:
# 1. If x1 = B or x2 > 0.5, then y = +
# 2. Otherwise, y = -
#
#
# Create a dataset with 1000 examples, following the rules above. Finally, add some noise
# to y, so that 5% of examples change their target value.
##########################################################################################
N = 1000
# vhodne / input variables
x1 = nakljucniNominalniVektor(c("A", "B"), c(3/5, 2/5), N)
x2 = runif(N)
# y: najprej vse na - (da upostevamo pravilo 2)
# set y to -, so that rule2 is taken into account
y = rep('-', N)
# pravila za y  / rules for y
x1b = x1 == "B"
y[x1b | x2 > 0.5] = "+"
# šum / noise
z = dodajSum(y, c("-", "+"), 0.05)

podatki1 = data.frame(x1=x1, x2=x2, y=z)

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
#
# ENG Create a dataset podatki2 with the input variables x1, x2, x3 in x4
# and the output variable y. Rules:
# - x1 is nominal and takes the values A in B. Additionally, P(x1 = A) = 3 / 5
# - x2 is nominal and takes the values C, D in E. Additionally, P(x2 = C) = P(x2 = D) = 1 / 3
# - x3 is numeric and distributed uniformly on the interval [0, 1]
# - x4 is numeric with standard normal distribution
# 
# Possible values of y are + and -:
# 1. If x1 = B, then y = +
# 2. If not 1 and x2 = D, then y = -
# 3. If not 1, not 2, x4 > 0 and x3 > 0,2, then y = +
# 4. If not 1, not 2, x4 <= 0 and x3 > 0,6, then y = +
# 5. Otherwise, y = -
#
# Create a dataset with 1000 examples, following the rules above. Finally, add some noise
# to y, so that 5% of examples change their target value.
##########################################################################################
N = 1000
x1 = nakljucniNominalniVektor(c("A", "B"), c(3 / 5, 2 / 5), N)
x2 = nakljucniNominalniVektor(c("C", "D", "E"), c(1 / 3, 1 / 3, 1 / 3), N);
x3 = runif(N)
x4 = rnorm(N)

y = rep('-', N)

x1b = x1 == "B"
y[x1b] = "+"

x2d = x2 == "D"
# y[!x1b & x2d] = "-"  # Tega ne potrebujemo / don't need that

x4_0 = x4 > 0
x3_02 = x3 > 0.2
y[!x1b & !x2d & x4_0 & x3_02] = "+"

x3_06 = x3 > 0.6
y[!x1b & !x2d & !x4_0 & x3_06] = "+"

y = dodajSum(y, c("-", "+"), 0.05)
podatki2 = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)

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
#
# ENG. For both datasets, grow a tree by using the method train in which you use the argument
# method="rpart". When training, use 10-fold cross-validation for estimation of optimal parameters.
# Which parameter was tuned?
#
# Since the next Problem is similar, implement a function
# treniraj12(data, chosenMethod) (i.e., train12) in which method=chosenMethod is used for training.
#
# Draw the final model with the library rpart.plot:
# you can simpliy call rpart.plot(finalModel)
#
# Do the obtained trees reflect the rules that define the values of y?
########################################################################################
treniraj12 = function(podatki, metoda){
    train(y ~.,
          data=podatki,
          method=metoda, 
          trControl=trainControl(method='cv', number=10))
}

model1 = treniraj12(podatki1, "rpart")
model2 = treniraj12(podatki2, "rpart")

library(rpart.plot)
rpart.plot(model1$finalModel)
rpart.plot(model2$finalModel)

##########################################################################
# 6. Stori enako še z uporabo metode method="rpart2".
# Kateri parameter gradnje dreves smo optimizirali?
# Sta dobljeni drevesi enaki kot prej?
#
# ENG. Do the same but use mehtod="rpart2". Which parameter was tuned?
# Are the obtained trees equal?
##########################################################################

model1_2 = treniraj12(podatki1, "rpart2")
model2_2 = treniraj12(podatki2, "rpart2")

rpart.plot(model1_2$finalModel)
rpart.plot(model2_2$finalModel)


###########################################################################################
# 7. Stori enako še z neposredno uporabo knjižnice rpart:
# rpart(formula, podatki, control=rpart.control(cp=0, maxdepth=10, minsplit=0, ...))
# Pomen parametrov (za preostale si oglej rpart.control):
# - cp: complexity parameter, kot ga že poznamo
# - maxdepth: največja dovoljena globina, pri čemer je koren na globini 0
# - minsplit: najmanjše število primerov v vsakem notranjem vozlišču, tj. v vozlišču, kjer
#             razdeimo podatke
#
# ENG. Do the same by directly using the rpart library:
# rpart(formula, data control=rpart.control(cp=0, maxdepth=10, minsplit=0, ...))
# The meanign of the parameters (see docs of rpart.control for the others):
# - cp: complexity parameter, as we know it
# - maxdepth: maximal depth, where depth(root) = 0
# - minsplit: minimal number of examples in a node, for which a split is still allowed
###########################################################################################

# Poigrajte se npr. s cp parametrom: cp = 0.0 in cp = 0.1
# Play with the cp parameter, e.g., cp = 0.0 or c = 0.1
rpart1 = rpart(y~., podatki1, control=rpart.control(cp=0.1, maxdepth = 10, minsplit=1, xval=0))
rpart2 = rpart(y~., podatki2, control=rpart.control(cp=0, maxdepth =4, minsplit=0, xval=0))
rpart.plot(rpart1)
rpart.plot(rpart2)

#########################################################################################################
# Opombi:
# 1. Opazili boste, da se za razliko od prej zgoraj npr. ni zgodila
# pretvorba nominalnih v numerične vektorje. Zato so testi v drevesu lepše berljivi:
# npr. x1 = A (in ne x1A = 1 oz. x1B = 0).
#
# 2. Izgubili smo možnost optimizacije. Če želimo optimizirati spremenljivko cp
# ter hkrati določiti še kak drug parameter, postopamo tako:
# S spremenljivko cp postopamo kot ponavadi in jo podamo kot data.frame v spremenljivko
# tuneGrid. Ostale parametre v primeru rpart lahko nadzorujemo samo preko parametra control, ki je
# imenovani parameter, ki se v klicu funkcije train poda naprej funkciji rpart.
#
# Notes:
# 1. As you see, the nominal vectors are not converted into numeric ones,
# thus the tests are easier to read: x1 = A (instaed of x1A = 1 or even x1B = 0).
#
# 2. We cannot fine-tune the parameters. If we wanted to do so for, e.g., cp,
# (and also specify the others) we proceed as follows:
# We define possible values of cp via tuneGrid, as we are used to, whereas the other parameters
# are passed to the train method via control parameter.
########################################################################################################
treniraj3 = function(podatki){
    train(y~., data = podatki,
          method='rpart', 
          trControl=trainControl(method='cv', number=10),
          tuneGrid = data.frame(cp=c(0.0, 0.05, 0.1, 0.2, 0.4, 0.8, 1.0)),
          control=rpart.control(maxdepth = 4, minsplit=1, xval=0))
}

model_rpart1 = treniraj3(podatki1)
model_rpart2 = treniraj3(podatki2)
rpart.plot(model_rpart1$finalModel)
rpart.plot(model_rpart2$finalModel)


###############################################################################################
# 8. Ampak kaj pa, ko hočemo optimizirati npr. maxdepth in cp?
# Ideja: za vsak maxdepth, najdi optimalen cp ...
# Ali deluje: JA. Ali je "lepo"? NE.
# Boljša ideja: razširimo caret s svojo metodo:
# https://topepo.github.io/caret/using-your-own-model-in-train.html
# (na sploh je to zelo dober uvod v caret).
# Kot opisuje stran, do katere povezavo najdete zgoraj, je treba načeloma svojo metodo opisati
# s kar precej parametri:
# - type: seznam z elementi iz množice {"Classification", "Regression"}
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
# a hkrati se bomo naučili reševati tudi precej težje probleme. Npr. lahko bi naredili
# svoja drevesa, ki v notranjih vozliščih za izbiro optimalnega testa uporabljajo
# metodo podpornih vektorjev namesto testov oblike x_i < 3.14 ...
#
# ENG. What if we want to fine-tune, e.g.,  maxdepth and cp?
# Idea: for every maxdepth, find optimal cp ...
# Does it work? Yes. Is this the preferred way to go: NO!
# We will rather extend caret with our own method:
# https://topepo.github.io/caret/using-your-own-model-in-train.html
# (in general, this is a good introduction to caret).
# As described there, a new method needs to be described by quite some many parameters:
# - type: vector whose components are elements of {"Classification", "Regression"}
# - library: a vector of necessary libraries (if we use them)
# - parameters: parameters that can be fine-tuned via train method
# - fit: function for fitting the model
# - predict: function for making predictions
# - prob: function that computes probabilities of class values
# - grid: function for creating the default tuneGrid value
#
# This parameters are given as a list to the method argument - for the built-in models,
# it suffices to pass their names (e.g., "knn", "rpart" ipd.).
#
# A simple problem of optimizing two parameters is thus approached with large cannons.
# However, by doing so, we have also learned how to approach much more complicated tasks,
# such as creating trees that use support vector machines in their splits instead of the
# standard x_i < 3.14 tests ...
#################################################################################################

# najprej lahke stvari  / easy stuff first
rpartBolje = list(type="Classification",  # regresije ne potrebujemo / don't need regression
                  library="rpart"         # naslonimo se na že obstoječo knjižnico / will use rpart
                  )

# parameters: potrebujemo ime spremenljivke, tip in "lepo ime" parametra
#             we need name of the variable, type and "nice name" of the parameter
# Podamo kot data.frame
rpartBolje$parameters = data.frame(
    parameter=c("cpBolje", "maxdepthBolje", "minsplitBolje"),  # bolje means better
    class=c("numeric", "numeric", "numeric"),
    label=c("Izboljsan complexity parameter", "Izboljsana maxdepth", "Moj minsplit")
)

# fit: uporabiti moramo imena parametrov, kot smo jih definirali zgoraj
#      we have to use the names of the parameters, as defined above
rpartBolje$fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    tree = rpart(data.frame(y,x),
                 control=rpart.control(maxdepth=param$maxdepthBolje,
                                       cp=param$cpBolje,
                                       minsplit = param$minsplitBolje))
    tree = prune(tree, cp=param$cpBolje)
    tree
}

# predict: pomembna, a v našem primeru lahka:
#          important, but easy to implement
rpartBolje$predict = function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit, data.frame(newdata), type="class")
}


# V našem konkretnem primeru vrednosti prob in grid ne potrebujemo:
# # prob je odveč, tuneGrid bomo pa tako ali tako podali.
#
# In our case, we do not need prob and grid:
# we do not need prob; as for tuneGrid we will pass it directly to train anyways ...
#
# Definiramo ju, ker caret brez njiju noče izvesti klica train.
# We defined them since caret does not want to train otherwise
rpartBolje$prob = 0
rpartBolje$grid = 0


#################################################################
# 9. Zgoraj ustrezno popravi rpartBolje (parameters in fit),
# tako da bo metoda omogočala še optimizacijo parametra minsplit.
#
# ENG. Extend the upper definition of rpartBolje (parameters and fit),
# so that minsplit can also be fine-tuned.
#################################################################

#######################################################################################################
# 10. Zaradi implementacije fit (pričakuje x in y) tokrat ne bomo uporabili formule,
# ampak bomo podatke razdelili na vhodni in izhodni del. Razdeli podatki1 in podatki2 na
# xs1 in y1 ter xs2 in y2.
#
# ENG. Due to the implementation of fit (it expects x and y) we won't use formula. Rather,
# we will pass our data in two parts. Divide the datasets podatki1 and podatki2 accordingly.
#######################################################################################################

xs1 = podatki1[names(podatki1)!= "y"]
xs2 = podatki2[names(podatki2)!= "y"]
y1 = podatki1$y
y2 = podatki2$y

##################################################################################
# 11. Definiraj primerno vrednost tg za tuneGrid parameter.
# Uporabi smiselne vrednosti za cpBolje, maxdepthBolje in minsplitBolje.
# Enostaven tg = data.frame(cpBolje=..., maxdepthBolje = ..., minsplitBolje = ...)
# ni dovolj, saj imamo več kot en parameter. Pomagaj si z expand.grid.
#
# ENG. Defined an appropriate value tg for tuneGrid parameter.
# Use appropriate values for cpBolje, maxDepthBolje and minsplitBolje (bolje = better).
# Simply calling tg = data.frame(cpBolje=..., maxdepthBolje = ..., minsplitBolje = ...)
# won't do the job. Help: expand.grid
##################################################################################

tg = expand.grid(cpBolje = seq(0,0.05, 0.01),
                 maxdepthBolje = 1:8,
                 minsplitBolje = c(1, 10, 50, 100, 200, 400)
                 )


################################################################################
# 12. Nauči se model po metodi rpartBolje. Uporabi 10-kratno prečno preverjanje
# ter tg, kot ga definiraš pri prejšnji nalogi.
#
# ENG. Train the model by using method=rpartBolje. Use 10-fold cross-validation
# and tg, as you defined above.
################################################################################

mojModel = train(xs2, y2,
                 method = rpartBolje,
                 tuneGrid = tg,
                 trControl = trainControl(method = "cv", number = 10)
                 )
rpart.plot(mojModel$finalModel)
