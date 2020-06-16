pravilo = function(pragovi, napoved){
    function(xs){
        if(all(xs < pragovi)){
            return(napoved)
        } else{
            return(0.0)
        }
    }
}


najdiPrvo = function(model){
    # najdi prave pragove in napoved lista
    pragovi = c(1, 2, 3)
    napoved = 3.14
    # vrni pravilo
    return(pravilo(pragovi, napoved))
}


p = najdiPrvo(NULL)

x1 = c(0, 1, 2)
x2 = c(0, 1, 4)
napoved1 = p(x1)
napoved2 = p(x2)
print(sprintf("Napoved za x1: %f", napoved1))
print(sprintf("Napoved za x2: %f", napoved2))
