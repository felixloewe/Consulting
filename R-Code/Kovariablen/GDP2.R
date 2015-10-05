# Datensatz GDP2 aufbereiten
#Ausgabe:
# - GDP2: GDP2 - Gesamtdatensaz
# - GDP2Year: Liste mit Datensatz pro Jahr (nur 1992 - 2010!) 

library("countrycode")
# einlesen
GDP2 <- read.delim("~/Consulting/Importdaten/extern/GDP2.csv")
#ccode hinzufügen
GDP2$ccode <- countrycode(GDP2$Country.Name, origin = "country.name", destination = "cown", warn = T)
#einträge ohne ccode löschen
cna <- which(is.na(GDP2$ccode))
GDP2 <- GDP2[-cna,]
#jährliche Liste erstellen
GDP2Year <- list()
for(i in 1:20){
  colname <- paste("X",1991+i, sep = "")
  mat <- GDP2[,c("ccode",colname)]
  colnames(mat) <- c("ccode", "gdp")
  GDP2Year[[i]] <- mat 
}
#Workspace aufräumen
rm("mat", "cna", "colname", "i")


