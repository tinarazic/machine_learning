naloziPodatke2 = function(){
    RNGkind(sample.kind = "Rejection")
    set.seed(123)
    n = 121
    x = runif(n, min = -5, max = 2)
    y = runif(n, min = -2, max = 2)
    podatki = data.frame(x, y)
}

loadData2 = function(){
    naloziPodatke2()
}