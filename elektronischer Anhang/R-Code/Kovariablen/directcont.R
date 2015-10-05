#directcont aufbereiten
#direct.cont ist eine liste von adjacency matizen von 1966-2000 (1-35)
#wir verwenden die werte aus 2000 auch für die Folgejahre bis 2011
#erstellt wird eine lsite mit den matritzen von 1992 -2011 (27-35,35...)

# AUsgabe:
# - directcont: Liste mit Adjajecny Matitzen von 1992-2011

load("Importdaten/extern/directcont.short.RData")

library(network)

ks <- c(27:35, rep(35,11))
directcont <- list()
i <- 1
for (k in ks){
directcont[[i]] <- as.matrix.network(directcont.short[[k]])
i <- i+1
}

ADirectCont <- directcont 
ADirectCont[[1]]
rm("directcont.short", "i", "k" , "ks", "directcont")
