library(png)
library(colorspace)
library(stringr)
library(stringi)

current_working_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)


jeOk = function(beseda){
    # odfiltrirajmo tiste s čudnimi znaki
    crke = c(LETTERS, letters, "č", "š", "ž", "Č", "Š", "Ž")
    crke = paste(crke, collapse = "|")
    for (cc in strsplit(beseda, split = "")[[1]]){
        if (!grepl(crke, cc)){
            return(FALSE)
        }
    }
    return(TRUE)
}

# preberi besede; ena beseda na vrsto
datotekaBesed = "mBesede.txt"
besede = levels(read.csv(datotekaBesed, header=FALSE, encoding = "UTF-8")[[1]])
soOk = rep(TRUE, length(besede))
for (i in 1:length(besede)){
    soOk[i] = jeOk(besede[i])
}
besede = besede[soOk]

imeDatoteke = function(beseda){
    # vrne ime datoteke s sliko besede. šumniki v imenu so nadomeščeni
    # s pripadajočim sičnikom, ki mu sledi xx (nekaj, česar sicer ni v besedah)
    b = beseda
    b = gsub("č", "cxx", b)
    b = gsub("š", "sxx", b)
    b = gsub("ž", "zxx", b)
    b = gsub("Č", "Cxx", b)
    b = gsub("Š", "Sxx", b)
    b = gsub("Ž", "Zxx", b)
    sprintf("crke/%s.png", b)
}

textPlot = function(plotname, string){
    # narišimo sliko: če so besede daljše, ustrezno spremeni mere slike ...
    par(mar=c(0,0,0,0))
    png(plotname, width = 500, units = "px", height = 150, res = 50)
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste(string), cex = 4, col = "black", family="mono", font=4, adj=0.5)
    dev.off()
}

narediSlike = function(){
    # 'narišemo' vse besede
    for (beseda in besede){
        textPlot(imeDatoteke(beseda), beseda)
    }
}

okvir = function(beseda){
    # izračunamo, koliko lahko obrežemo:
    # vitalni deli slike se nahajajo na območju b[y0:y1, x0:x1]
    # tj. gremo od
    #
    # xxxxxxxxxxx
    # xxxgozdxxxx
    # xxxxxxxxxxx
    #
    # (x je belo ozadje) do slike
    #
    # gozd
    #
    b = 1 - readPNG(imeDatoteke(beseda))
    b = b[,,1]
    d = dim(b)
    yMax = d[1]
    xMax = d[2]
    # poreži levo
    x = 1
    while(max(b[, x]) == 0){
        x = x + 1
    }
    x0 = max(x - 1, 1)
    # desno
    x = xMax
    while(max(b[, x]) == 0){
        x = x - 1
    }
    x1 = min(xMax, x + 1)
    # zgoraj
    y = 1
    while(max(b[y, ]) == 0){
        y = y + 1
    }
    y0 = max(1, y - 1)
    # spodaj
    y = yMax
    while(max(b[y, ]) == 0){
        y = y - 1
    }
    y1 = min(yMax, y + 1)
    
    c(y0, y1, x0, x1)
}

dolociMeje = function(){
    # najdi unijo vitalnih delov slik
    meje = c(10000, 0, 10000, 0)
    
    for (beseda in besede){
        y01x01 = okvir(beseda)
        meje[1] = min(meje[1], y01x01[1])
        meje[2] = max(meje[2], y01x01[2])
        meje[3] = min(meje[3], y01x01[3])
        meje[4] = max(meje[4], y01x01[4])
    }
    return(meje)
}

obrezi = function(izhod = "besede.csv"){
    # obrezi slike in jih shrani v tabelo
    meje = dolociMeje()
    vrstice = meje[2] - meje[1] + 1
    stolpci = meje[4] - meje[3] + 1
    piksli = stolpci * vrstice
    print("Meje:")
    print(meje)
    tabela = matrix(nrow = length(besede), ncol = piksli)
    for (i in 1:length(besede)){
        beseda = 1 - readPNG(imeDatoteke(besede[i]))
        beseda = beseda[,,1]
        obrezano = t(beseda[meje[1]:meje[2], meje[3]:meje[4]])
        dim(obrezano) = piksli
        tabela[i,] = obrezano
    }
    row.names(tabela) = besede
    write.csv(tabela, izhod, fileEncoding="UTF-8")
    return(list(stolpci = stolpci, vrstice = vrstice))
}

dolociY = function(besedas, crka){
    # določi razred glede na to, ali beseda vsebuje črko ali ne
    y = rep(1.0, length(besedas))
    for (i in 1:length(besedas)){
        y[i] = as.numeric(grepl(crka, besedas[i]))
    }
    return(y)
}

pripraviPodatke = function(crka, stolpci, vrstice, vhod="besede.csv"){
    piklsi = vrstice * stolpci
    tabela = read.csv(vhod, fileEncoding="UTF-8")
    vrsticeTabela = dim(tabela)[1]
    stolpciTabela = dim(tabela)[2]
    if (stolpciTabela != piklsi + 1){
        print("Nekaj je narobe")
    }
    besede = levels(tabela[, 1])
    xs = as.matrix(tabela[, 2:stolpciTabela])
    # iteracija po matriki poteka po stolpcih
    xs = t(xs)
    # dim slike x kanali x število primerov
    dim(xs) = c(stolpci, vrstice, 1, vrsticeTabela)
    # določimo še y
    y = dolociY(besede, crka)
    nPrimeri = length(y)
    print(sprintf("Primeri: %d, pozitivni: %d", nPrimeri, sum(y)))
    return(list(xs = xs, y = y))
}

############################################################
# Poženi enkrat
narediSlike()
sv = obrezi()
# Poženi, ko želiš konkretne podatke za učenje
p = pripraviPodatke("a", sv$stolpci, sv$vrstice)

