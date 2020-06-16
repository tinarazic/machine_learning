naloziPodatke = function(){
  p = read.csv("podatki13.csv")
  p$y = NULL
  return(as.matrix(p)[1:50,])
}