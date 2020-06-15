library(png)

naloziSliko = function(){  # loadImage
    slika = readPNG("slikaKviz.png")
    slika = slika[,,1:3]
    return(slika)
}
