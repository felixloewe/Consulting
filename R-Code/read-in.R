## Statistisches Consulting, SoSe 2015
## Internationaler Waffenhandel: Die Anwendung neuer Verfahren der statistischen Netzwerkanalyse
## Kooperation mit dem Lehrstuhl für empirische Politikforschung, LMU
## Felix Loewe, loewe.felix@gmail.com
## Roman Dieterle, roman.dieterle@hotmail.de

## Eine Analyse des internationalen Kleinwaffen-Handels, 1992 - 2011

## Einlesen und Vorbereitung der Analyse
## Was bedeuted MAD?

## Ausgabe:
# - MADdata (Originaldaten)
# - Graph (iGraph)
# - GraphYear (iGraph)

# Pakete laden
install.packages("stringr")
install.packages("igraph")
install.packages("countrycode")
install.packages("network")
install.packages("ergm")
library(countrycode)
library(stringr)
library(igraph)
library(network)
library(ergm)
# NISAT-Database MasterTableFinal einlesen (109.000 Zeilen = 109.000 Exporte)
MADdata <- read.csv("Importdaten/MAD Data 1992-2011 bearbeitet.csv")

# Leerzeichen in Import- und Exportland entfernen
MADdata$Reporter_Name <- str_trim(MADdata$Reporter_Name)
MADdata$Partner_Name <- str_trim(MADdata$Partner_Name)
MADdata$Value <- as.numeric(MADdata$Value)

# Spalten vertauschen, sodass Exportland an erster Stelle steht
MADdata <- cbind(Reporter_Code = MADdata$Reporter_Code, Partner_Code = MADdata$Partner_Code, MADdata[,-c(1,8)])

# Kontinente der Import- und Exportlaender hinzufügen
# source("R-Code/continents.R", local = T)

# Waffenwert in Millionen umwandeln
MADdata$ValueMil <- MADdata$Value/10^6

# Tabellen-Datensatz attachen
# attach(MADdata)

# Gesamtgraph erstellen (iGraph-Objekt)
Graph <- graph.data.frame(MADdata, vertices = KnotenAttr)

# Vertex Attribute hinzufügen:
# Ländername
V(Graph)$Country_Name <- countrycode(V(Graph)$name, "cown", "country.name", warn = T)
# Kontinent
V(Graph)$Continent <- countrycode(V(Graph)$name, "cown", "continent", warn = T)
# Region
V(Graph)$Region <- countrycode(V(Graph)$name, "cown", "region", warn = T)
# "Americas" in "America" abändern
V(Graph)$Continent[V(Graph)$Continent == "Americas"] <- "America"


# iGraph-Objekt pro Jahr erstellen (20 Jahre, 1992 - 2011)
Year <- 1992:2011
GraphYear <- lapply(Year, function(jahr) subgraph.edges(Graph, E(Graph)[Year==jahr]))

# externe Kovariablen hinzufügen
for(i in 1:20){
  set.vertex.attribute(GraphYear[[i]], "GDP", #index = V(GraphYear[[i]]), 
                       KnotenAttrYear[[i]]$GDP)
}
set.vertex.attribute()
                    


# Funktionen einlesen