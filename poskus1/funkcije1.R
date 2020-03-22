naloziPodatke = function(seme){
  set.seed(seme)
  podatki = read.csv("podatki1.csv")
  n = dim(podatki)[1]
  izbrani = sample(1:n, ceiling(0.8 * n))
  podatki[izbrani, ]
}

predictKviz = function(model, podatki, seme){
  set.seed(seme)
  predict(model, newdata = podatki)
}

