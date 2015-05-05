# Datensatz GDPc aufbereiten
#Ausgabe:
# - GDP: GDP - Gesamtdatensaz
# - GDPYear: Liste mit Datensatz pro Jahr (nur 1992 - 2010!) 

library("countrycode")
# einlesen
GDP <- read.csv("Importdaten/extern/GDP.csv", skip = 2)

# erste Spalte enthält Jahreszahlen
names(GDP)[1] <- "year"

# interessierende Jahre herausfiltern
GDP <- GDP[GDP$year >= 1992 & GDP$year <= 2011,]

# Ländernamen sind nicht alle konform mit cow Bezeichnungen:
names(GDP)[13] <- "United Kingdom"
names(GDP)[28] <- "Czechoslovakia"
names(GDP)[77] <- "Haiti"
names(GDP)[98] <- "Hong Kong"
names(GDP)[22] <- "New Zealand"
names(GDP)[57] <- "Turkmenistan"
# restlichen Länder/Regionen werden nicht benötigt
ccode <- countrycode(names(GDP)[-1],origin = "country.name", destination = "cown")
GDP <- GDP[,-which(is.na(c(0,ccode)))]
ccode <- ccode[-which(is.na(ccode))]

# Spalten für späteren data.frame erstellen
year <- rep(GDP$year, 161)
country.name <- rep(names(GDP)[-1], each = 19)
ccode <- rep(ccode, each = 19)
gdp.mat <- as.matrix(GDP[, - 1])
gdp.vec <- as.vector(gdp.mat, mode = "any")

# fertiges GDP Objekt erstellen
GDP <- cbind(country.name, as.numeric(ccode), as.numeric(year), as.numeric(gdp.vec))
GDP <- as.data.frame(GDP)
names(GDP) <- c("country.name", "ccode", "year", "gdp")
GDP$ccode <- as.numeric(as.character(GDP$ccode))
GDP$year <- as.numeric(as.character(GDP$year))
GDP$gdp <- as.numeric(GDP$gdp)
GDP$country.name <- as.character(GDP$country.name)
GDP <- GDP[-which(GDP$country.name == c("F..USSR")),]
GDP <- GDP[-which(GDP$country.name == c("F..Yugoslavia")),]

#nach Jahren trennen:
Year <- 1992:2011
GDPYear <- lapply(Year, function(jahr) subset(GDP, year==jahr))

rm(gdp.mat, ccode, country.name, gdp.vec, year)
