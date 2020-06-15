# working directory
current_working_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)

# knjiznice

library(rpart)
library(caret)

# Postavitev okolja
source("pravila.R")
source("kviz1.R")
seme = 288
set.seed(seme)
n = 10^6
x = runif(n)
x = x[order(x)]
y = runif(n)
model = readRDS("model.rds")
podatki13 = naloziPodatke()

###### Drevesa

# 1.1 Ucinkovit izracun variance

vseKakovosti = function(y, x) {
  n = length(y)
  kakovosti = rep(0, n)
  sum = sum(y)
  sum2 = sum(y^2)
  var_D = sum2/n - (sum/n)^2
  partial_sum = 0
  partial_sum2 = 0
  for (i in 1:(n-1)) {
    # zaradi urejenosti x lahko tako po vrsti y jemlemo
    partial_sum = partial_sum + y[i]
    partial_sum2 = partial_sum2 + y[i]^2
    var_D1 = partial_sum2/i - (partial_sum/i)^2
    var_D2 = (sum2 - partial_sum2)/(n-i) - ((sum-partial_sum)/(n-i))^2
    kakovosti[i] = var_D - (i*var_D1 + (n-i)*var_D2)/n
  }
  return(kakovosti)
}

kakovosti = vseKakovosti(y, x)
a = max(kakovosti)
sprintf("%.16f",a * 10^6)

# 1.2 Globina drevesa

globina = function(model) {
  nodes <- as.numeric(rownames(model$finalModel$frame))
  max(rpart:::tree.depth(nodes)) + 1 # + 1 ker korena ne šteje zraven in ga tako dodamo
}

globina(model)

# 1.3 Sekajmo drevesa
# pomoc pravila.R

najdiPrvo = function(model) {
  # najbolj levi list je prvi <leaf> ce poklicemo model$finalModel$frame
  index_min = min(which(model$finalModel$frame$var == '<leaf>'))
  nodes = labels(model$finalModel)[2:index_min]
  # napoved kar na roke napišem niti ni tako važno kaj je ker gledamo druge stvari
  napoved <- "---"
  return(c(nodes, napoved))
}

pravilo <- function(x) {
  nodes <- najdiPrvo(model)
  napoved <- tail(nodes, n=1)
  meje <- as.numeric(regmatches(nodes,regexpr("-?\\d\\.\\d*",nodes)))
  xi <- regmatches(nodes,regexpr("X\\d",nodes))
  n <- nrow(x)
  napovedi <- rep(NA, n)
  for (i in 1:n) {
    if (all(x[i, xi] < meje)) {
      napovedi[i] <- napoved
    }
  }
  return(napovedi)
}

napovedi <- pravilo(podatki13)
null_val <- sapply(napovedi, is.na)
length(napovedi[null_val])

# če slučajno funkcija ne deluje, se da tudi na roke naredit
# vidimo da sta pravili do levega lista "X1< -0.2526"  "X4< -0.08022"
# napovemo tako:
# napoved <- rep(NULL, 50)
# napoved[podatki13[,1] < -0.2526 & podatki13[,4] < -0.08022] <- "---"
