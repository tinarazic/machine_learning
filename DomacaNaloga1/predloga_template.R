#####################################
# nalozi knjiznice, ki jih potrebujes
# load the libraries you need
#####################################
library(caret)

# nalozi jih tukaj, ne po klicu RNGkind spodaj
# load them here and not after the call of the RNGkind method below


#########################################################################
# Ignoriraj opozorilo (ignore the warning)
# RNGkind(sample.kind = "Rounding") : non-uniform 'Rounding' sampler used
#########################################################################
RNGkind(sample.kind = "Rounding")

#####################################
# Nekaj testov: ne spreminjaj
# Some tests: do not change
#####################################

test_runif = function(){
  set.seed(1234)
  x = runif(5);
  x1 = c(0.1137034113053232, 0.6222994048148394, 0.6092747328802943, 0.6233794416766614, 0.8609153835568577)
  if (sum(abs(x - x1)) > 10^-10){
    stop("Test runif ni ok (has failed)")
  }
}

test_sample = function(){
  set.seed(1234)
  x = sample(20);
  x1 = c(3, 12, 11, 18, 14, 10, 1, 4, 8, 6, 7, 5, 20, 15, 2, 9, 17, 16, 19, 13)
  if (sum(abs(x - x1)) > 0){
    stop("Test sample ni ok (has failed)")
  }
}

test_runif()
test_sample()


#####################################
# Nalozi se potrebne funkcije
# Load the necessary functions
#####################################
# setwd("pot do mape (path to directory)")

naloga_problem = 1
source(sprintf("funkcije%d.R", naloga_problem))

####################################
# Resi nalogo
# Solve the problem
####################################

# vasa koda gre semkaj
# your code goes here








