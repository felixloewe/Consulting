## Kontinente hinzufügen
# Countrycode-Paket laden
install.packages("countrycode")
library(countrycode)
# countrycode_data, Für komplette Auflistung der im Paket enthatenen Daten


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
      c( 10, 1029, 1032, 1033, 1061, 1062, 1065, 1069, 1073, 11, 1163, 1165, 120, 203, 231,
         30, 315, 328, 345, 347, 348, 4050, 5, 5812, 585, 59, 5920, 65, 66, 6631, 68, 713,
         720, 721, 815, 9001, 9002, 9004, 9201, 925, 9253, 930, 9321, 960, 9620, 980, 984,
         991, 995, 9970),
    c("Reporter_Name", "Reporter_Code")
    ]
)

# So liegen die Kontinente jetzt vor (Edge-Attribute).
# E(Graph)$continents_exp # Export-Kontinente
# E(Graph)$continents_imp # Import-Kontinente


