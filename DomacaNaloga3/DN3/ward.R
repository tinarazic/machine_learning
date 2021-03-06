d = function(x1, x2){
    sqrt(sum((x1 - x2)^2))
}

dSkupina = function(matrika, indeksi1, indeksi2){
    # izračunamo povprečno razdaljo
    dKoncni = 0.0
    for (i1 in indeksi1){
        for (i2 in indeksi2){
            dKoncni = dKoncni + d(matrika[i1, ], matrika[i2, ])
        }
    }
    dKoncni / (length(indeksi1) * length(indeksi2))
}


ime1 = function(idSkupine){
    sprintf("%d", idSkupine)
}

ime2 = function(id1, id2){
    sprintf("par%d_%d", id1, id2)
}


ward = function(matrika){
    # implementacija morda ni najbolj učinkovita, ker je bil poudarek na razumljivosti :)
    skupine = list()  # skupine[[ime skupine]] = indeksi vrstic, ki pripadajo tej skupini
    razdalje = list() # razdalja[[ime para]] = (razdalja, ime skupine1, ime skupine2)
    n = nrow(matrika)
    # osnovne skupine so enojci
    for (i in 1:n){
        skupine[[ime1(i)]] = c(i)
    }
    # na začetku "navadne" razdalje
    for (i in 1:(n - 1)){
        for (j in (i + 1):n){
            razdalje[[ime2(i, j)]] = list(d(matrika[i, ], matrika[j, ]), ime1(i), ime1(j))
        }
    }
    naslednjiID = n + 1
    dendrogram = list()  # četverec (razdalja, id1, id2, id(1 unija 2))
    for (zdruzitev in 1:(n - 1)){
        # najdemo najbližji par skupin
        iMin = 1
        for (i in 1:length(razdalje)){
            dZdaj = razdalje[[i]][[1]]
            if (dZdaj < razdalje[[iMin]][[1]]){
                iMin = i
            }
        }
        # dodajmo točko v dendrogram
        noviElement = razdalje[[iMin]]  # prve tri komponente se ujemajo
        novaSkupina = ime1(naslednjiID)
        noviElement[[4]] = novaSkupina
        dendrogram[[zdruzitev]] = noviElement
        # množici, ki ju združujemo
        id1 = razdalje[[iMin]][[2]]
        id2 = razdalje[[iMin]][[3]]
        # odstranimo se razdalje, ki vključujejo id1 ali id2
        zaOdmet = list()
        imenaParov = names(razdalje)
        for (i in 1:length(razdalje)){
            i1 = razdalje[[i]][[2]]
            i2 = razdalje[[i]][[3]]
            if (length(unique(c(id1, id2, i1, i2))) < 4){
                # če se kak ponovi, bodo največ trije različni
                # vemo pa že: id1 != id2 in i1 != i2
                zaOdmet[[length(zaOdmet) + 1]] = imenaParov[i]
            }
        }
        for(imeOdmet in zaOdmet){
            razdalje[[imeOdmet]] = NULL
        }
        # izračunajmo razdaljo nove skupine do prejšjnih
        indeksi1 = skupine[[id1]]
        indeksi2 = skupine[[id2]]
        for (id3 in names(skupine)){
            indeksi3 = skupine[[id3]]
            if (length(unique(c(id1, id2, id3))) == 3){
                l1 = length(indeksi1)
                l2 = length(indeksi2)
                l3 = length(indeksi3)
                alfa1 = (l1 + l3) / (l1 + l2 + l3)
                alfa2 = (l2 + l3) / (l1 + l2 + l3)
                beta = - l3 / (l1 + l2 + l3)
                d13 = dSkupina(matrika, indeksi1, indeksi3)
                d23 = dSkupina(matrika, indeksi2, indeksi3)
                d12 = dSkupina(matrika, indeksi1, indeksi2)
                razdalja = alfa1 * d13 + alfa2 * d23 + beta * d12
                razdalje[[ime2(as.numeric(id3), naslednjiID)]] = list(razdalja, id3, novaSkupina)
            }
        }
        # odstranimo obe stari skupini in dodamo novo
        skupine[[novaSkupina]] = c(skupine[[id1]], skupine[[id2]])
        skupine[[id1]] = NULL
        skupine[[id2]] = NULL
        naslednjiID = naslednjiID + 1
    }
    # spremenimo v lep data frame
    razdalje = c()
    skupina1 = c()
    skupina2 = c()
    skupina12 = c()
    for (i in 1:(n - 1)){
        element = dendrogram[[i]]
        razdalje = c(razdalje, element[[1]])
        skupina1 = c(skupina1, element[[2]])
        skupina2 = c(skupina2, element[[3]])
        skupina12 = c(skupina12, element[[4]])
    }
    data.frame(d = razdalje, skupina1 = skupina1, skupina2 = skupina2, skupina12 = skupina12)
}

####################################################################################################
# Test #############################################################################################
####################################################################################################


m = matrix(NA, ncol = 2, nrow = 5)
m[1:4,1] = c(0.9, 2, 4, 5)
m[1:4,2] = 0
m[5,] = c(3, 5)
# m:
# 
#    x
#
#
#
#
# x x  x x
#


# View(ward(m))

















