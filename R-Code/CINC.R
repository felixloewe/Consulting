# CINC Datensatz aufbereiten
# Ausgabe:
# - CINC: Gesamtdatensatz
# - CINCYear: Liste mit Datensatz pro Jahr (nur 1992 - 2007!)
CINC <- read.csv("Importdaten/extern/CINC.csv")
CINC <- CINC[CINC$year >= 1992 & CINC$year <= 2011, c("ccode", "year", "cinc")]
nas <- which(is.na(CINC$cinc))
CINC$cinc[nas] <- 0
Year <- 1992:2011
CINCYear <- lapply(Year, function(jahr) subset(CINC, year==jahr))


rm("nas")