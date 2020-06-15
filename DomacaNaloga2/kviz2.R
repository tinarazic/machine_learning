preveri = function(p1, p2, sigma1, sigma2, b){
    data = read.csv("podatki23.csv")
    y = data$y
    for (i in 1:nrow(data)){
        pred = radialnoJedro(p1, data[i,1:2], sigma1) + radialnoJedro(p2,data[i,1:2], sigma2) - b
        pred2 = sign(pred)
        if (pred2 * y[i] < 0){
            return(0)
        }
    }
    return(1)
}

check = function(p1, p2, sigma1, sigma2, b){
    preveri(p1, p2, sigma1, sigma2, b)
}