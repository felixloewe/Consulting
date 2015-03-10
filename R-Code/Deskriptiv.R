## Netzwerkanalyse Waffenhandel



# Pakete
install.packages("stringr")
install.packages("igraph")
library(stringr)
library(igraph)

# Einlesen

MADdata <- read.csv("Exel/MAD Data 1992-2011 bearbeitet.csv")
MADdata$Reporter_Name <- str_trim(MADdata$Reporter_Name) # Leerzeichen entfernen
MADdata$Partner_Name <- str_trim(MADdata$Partner_Name) # Leerzeichen entfernen

MADdata <- cbind(Reporter_Name = MADdata$Reporter_Name, Partner_Name = MADdata$Partner_Name, MADdata[,-c(2,9)]) # Spalten vertauschen
head(MADdata)
Gr <- graph.data.frame(MADdata)
MADdata$Value <- as.numeric(MADdata$Value)
attach(MADdata)

# Graph pro Jahr erstellen
year <- 1992:2011
GraphYear <- vector("list", 20)
for(i in 1:20){
  GraphYear[[i]] <- subgraph.edges(Gr, E(Gr)[Year==year[i]])
}

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

# Anzahl Lieferungen steigt, Anzahl L?nder steigt: wird Datenqualit?t besser, oder wirklich mehr Lieferungen?

# Gesamtwert in US-Dollar steigend: von 1.850 Milliarden bis hin zu 4.232 Milliarden

par(mfrow=c(4,1))
barplot(value, names.arg = year, main = "Gesamtwert")
barplot(edges, names.arg = year, main = "Lieferungen") # Lieferungen
barplot(vertices, names.arg = year, main = "Teilnehmer") # Teilnehmer
barplot(trans, names.arg = year, main = "Dichte") # Dichte


# Effekt 2001?

plot(table( Year, Data_Source)) # neue Datenquellen ab 2003, anscheinend ist der Datensatz gegen Ende des erhobenen Zeitraums nicht komplett.

table(Data_Source, Reporter_Name) # ein Land von mehreren reported?


# Variablen betrachten
table(Partner_Name)
table(Reporter_Code)
table(Year)
table(PRIO_Weapons_Code)
table(IsMirror)




# Top-Tabellen
#2011
Top_Export2011 <- aggregate(MADdata[Year == 2011,]$Value, list(Land = MADdata[Year == 2011,]$Reporter_Name), sum)
Top_Export2011 <- Top_Export2011[order(-Top_Export2011$x),]
Top_Export2011[1:10,]
#1992
Top_Export1992 <- aggregate(MADdata[Year == 1992,]$Value, list(Land = MADdata[Year == 1992,]$Reporter_Name), sum)
Top_Export1992 <- Top_Export1992[order(-Top_Export1992$x),]
Top_Export1992[1:10,]

#2011
Top_Import2011 <- aggregate(MADdata[Year == 2011,]$Value, list(Land = MADdata[Year == 2011,]$Partner_Name), sum)
Top_Import2011 <- Top_Import2011[order(-Top_Import2011$x),]
Top_Import2011[1:10,]
#1992
Top_Import1992 <- aggregate(MADdata[Year == 1992,]$Value, list(Land = MADdata[Year == 1992,]$Partner_Name), sum)
Top_Import1992 <- Top_Import1992[order(-Top_Import1992$x),]
Top_Import1992[1:10,]

Top_Import <- aggregate(Value, list(Land = Partner_Name), sum)
Top_Import <- Top_Import[order(-Top_Import$x),]
Top_Import[1:10,]

# Variable IsMirror
# Haben wir doppelte Zeilen? Dem Anschein nach nicht.



# Verlauf einseitiger Handel
# ben?tige Adjazenzmatrix pro Jahr

# Verteilung durchschnittlicher In-Degree (Anzahl der Importe)

table(degree(Gr, mode = "in")/20)
table(degree(Gr, mode = "out"))
hist(degree(Gr, mode = "out"),plot = T, prob = T,  right = F)
sort(table(Reporter_Name), decreasing= T, prob = T)[1:10]
sort(table(Partner_Name), decreasing= T)[1:10]


######## Grafiken erstellen
par(mfrow= c(2,1), mar = c(7,5,5,2))
plot(table(cut(degree(Gr, mode = "in")/20, c(1,seq(from = 0, to = 100, by = 5), max(degree(Gr, mode = "in")/20)),right = F, dig.lab=4))/241,
     main = "Durchschnittlicher In-Degree pro Jahr", las = 2, ylab = " relative Häufigkeit" ,
     col = "blue", lwd = 10)
grid(lwd = 1)


plot(table(cut(degree(Gr, mode = "out")/20, c(1,seq(from = 0, to = 100, by = 5), max(degree(Gr, mode = "out")/20)),right = F, dig.lab=4))/241,
     main = "Durchschnittlicher Out-Degree pro Jahr", las = 2, ylab = " relative Häufigkeit" ,
     col = "green", lwd = 10)
grid(lwd = 1)



