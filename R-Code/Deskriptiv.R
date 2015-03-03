## Netzwerkanalyse Waffenhandel



setwd("C:/Users/TEMP/Desktop/Statistisches Consulting (1)")

# Pakete
install.packages("stringr")
install.packages("igraph")
library(stringr)
library(igraph)

# Einlesen

MADdata <- read.csv("C:/Users/s10370952/Dropbox/Statistisches Consulting/Exel/MAD Data 1992-2011 bearbeitet.csv")
MADdata$Reporter_Name <- str_trim(MADdata$Reporter_Name) # Leerzeichen entfernen
MADdata$Partner_Name <- str_trim(MADdata$Partner_Name) # Leerzeichen entfernen

MADdata <- cbind(Reporter_Name = MADdata$Reporter_Name, Partner_Name = MADdata$Partner_Name, MADdata[,-c(2,9)]) # Spalten vertauschen
head(MADdata)
Gr <- graph.data.frame(MADdata)
MADdata$Value <- as.numeric(MADdata$Value)
attach(MADdata)

# Clean-Up

#E(Gr)$Value <- gsub("\\.", "", E(Gr)$Value)
#E(Gr)$Value <- as.numeric(E(Gr)$Value)

# Deskriptive Statistiken

trans <- vector()
edges <- vector()
vertices <- vector()
diameter <- vector()
value <- vector()
year <- 1992:2011

for(i in 1:20){
  
  sub <- subgraph.edges(Gr, E(Gr)[Year==year[i]])
  
  trans[i] <- transitivity(sub)
  edges[i] <- ecount(sub)
  vertices[i] <- vcount(sub)
  diameter[i] <- diameter(sub)
  value[i] <- sum(as.numeric(E(sub)$Value))
   
}

descriptives <- list(trans = trans, edges = edges, vertices = vertices, diameter = diameter, value = value)
descriptives

# relativ hohe Netzwerkdichte

# Anzahl Lieferungen steigt, Anzahl Länder steigt: wird Datenqualität besser, oder wirklich mehr Lieferungen?

# Gesamtwert in US-Dollar steigend: von 1.850 Milliarden bis hin zu 4.232 Milliarden

par(mfrow=c(4,1))
barplot(value, names.arg = year, main = "Gesamtwert")
barplot(edges, names.arg = year, main = "Lieferungen") # Lieferungen
barplot(vertices, names.arg = year, main = "Teilnehmer") # Teilnehmer
barplot(trans, names.arg = year, main = "Dichte") # Dichte


# Effekt 2001?

table(Data_Source, Year) # neue Datenquellen ab 2003

table(Data_Source, Reporter_Name) # ein Land von mehreren reported?


# variablen betrachten
table(Partner_Name)
table(Reporter_Code)
table(Year)
table(PRIO_Weapons_Code)
table(IsMirror)

MADdata[Partner_Name == "Libya",]


# Top-Tabellen

Top_Export <- aggregate(Value, list(Land = Reporter_Name), sum)
Top_Export <- Top_Export[order(-Top_Export$x),]
Top_Export[1:10,]
Top_Import <- aggregate(Value, list(Land = Partner_Name), sum)
Top_Import <- Top_Import[order(-Top_Import$x),]
Top_Import[1:10,]

# Variable IsMirror
# Haben wir doppelte Zeilen? Dem Anschein nach nicht.



# Verlauf einseitiger Handel
# benötige Adjazenzmatrix pro Jahr

# Verteilung durchschnittlicher In-Degree (Anzahl der Importe)

# Graph pro Jahr erstellen
year <- 1992:2011
GraphYear <- vector("list", 20)
for(i in 1:20){
  GraphYear[[i]] <- subgraph.edges(Gr, E(Gr)[Year==year[i]])
}
par(mfrow= c(1,1))

mean(table(degree(Gr, mode = "in")))
table(degree(Gr, mode = "out"))

is.directed(Gr)
hist(degree(Gr, mode = "out"),plot = T, prob = T,  right = F)
GraphYear
sort(table(Reporter_Name), decreasing= T, prob = T)[1:10]
sort(table(Partner_Name), decreasing= T)[1:10]
V(Gr)
table(MADdata$Reporter_Name)
