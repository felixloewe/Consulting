## Kontinente hinzufügen

# Countrycode-Paket laden
install.packages("countrycode")
library(countrycode)

# Kontinente hinzufügen. Format des Ursprungscodes: CoW-Numeric = Correlates of War Numeric
# Correlates of War versucht, wissenschaftlich standardisierte Daten über Kriege zu sammeln und
# zu Verfügung zu stellen.
MADdata$continents_exp <- countrycode(MADdata$Reporter_Code, "cown", "continent", warn = TRUE)
MADdata$continents_imp <- countrycode(MADdata$Partner_Code, "cown", "continent", warn = TRUE)

# Herausfinden, welche Länder der Kontinente nicht gematcht werden können.
# Sind keinem Kontinent zugeordnet (Sonderverwaltungszonen).
# Betreffen ca. 7000 Zeilen.
unique(
  MADdata[
    MADdata$Reporter_Code %in%
      c(10, 11, 30, 65, 66, 68, 120, 231, 315, 345, 348, 585, 713, 720, 721, 815, 925, 930, 960, 
        980, 991, 995, 1029, 1032, 1065, 1069, 1073, 1163, 4050, 5812, 9002, 9004, 9201, 9321),
    c("Reporter_Name", "Reporter_Code")
    ]
)

# So liegen die Kontinente jetzt vor (Edge-Attribute).
# E(Graph)$continents_exp # Export-Kontinente
# E(Graph)$continents_imp # Import-Kontinente


