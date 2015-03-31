## Einlesen und Vorbereitung der Analyse
## Roman's Code
## Was bedeuted MAD?

## Ausgabe:
# - MADdata (Originaldaten)
# - Graph (iGraph)
# - GraphYear (iGraph)

# Pakete laden
#install.packages("stringr")
#install.packages("igraph")
library(stringr)
library(igraph)

# NISAT-Database MasterTableFinal einlesen (109.000 Zeilen = 109.000 Exporte)
MADdata <- read.csv("Importdaten/MAD Data 1992-2011 bearbeitet.csv")

# Leerzeichen in Import- und Exportland entfernen
MADdata$Reporter_Name <- str_trim(MADdata$Reporter_Name)
MADdata$Partner_Name <- str_trim(MADdata$Partner_Name)
MADdata$Value <- as.numeric(MADdata$Value)

# Spalten vertauschen, sodass Exportland an erster Stelle steht
MADdata <- cbind(Reporter_Name = MADdata$Reporter_Name, Partner_Name = MADdata$Partner_Name, MADdata[,-c(2,9)])

# Kontinente der Import- und Exportl?nder hinzufügen
source("R-Code/continents.R", local = T)

# Waffenwert in Millionen umwandeln
MADdata$ValueMil <- MADdata$Value/10^6

# Tabellen-Datensatz attachen
# attach(MADdata)

# Gesamtgraph erstellen (iGraph-Objekt)
Graph <- graph.data.frame(MADdata)

# iGraph-Objekt pro Jahr erstellen (20 Jahre, 1992 - 2011)
Year <- 1992:2011
GraphYear <- lapply(Year, function(jahr) subgraph.edges(Graph, E(Graph)[Year==jahr]))
                    


# Funktionen einlesen