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

table(Data_Source, Year) # neue Datenquellen ab 2003

table(Data_Source, Reporter_Name) # ein Land von mehreren reported?


# Variablen betrachten
table(Partner_Name)
table(Reporter_Code)
table(Year)
table(PRIO_Weapons_Code)
table(IsMirror)




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
# ben?tige Adjazenzmatrix pro Jahr

# Verteilung durchschnittlicher In-Degree (Anzahl der Importe)

mean(table(degree(Gr, mode = "in")))
table(degree(Gr, mode = "out"))
hist(degree(Gr, mode = "out"),plot = T, prob = T,  right = F)
sort(table(Reporter_Name), decreasing= T, prob = T)[1:10]
sort(table(Partner_Name), decreasing= T)[1:10]
<<<<<<< HEAD

par(mfrow= c(1,1))

plot(cut(degree(Gr, mode = "in")/20, 50), main = "Durchschnittlicher In-Degree")
plot(cut(degree(Gr, mode = "out")/20, 1:200), main = "Out-Degree")




# Zeitreihen Degree-Werte plotten
breaks <- c(0,1,100,1000,max(degree(Gr))) # Klassen
xAchse <- year
yAchse <- c(0,250)
colors_in <- c("#b2e2e2", "#66c2a4", "#238b45", "#006d2c") # Farben
colors_out <- c("#b3cde3", "#8c96c6", "#8856a7", "#810f7c") # Farben
legend_in <- c("In-Deg 0", "In-Deg 1-100", "In-Deg 100-1000", "In-Deg > 1000")
legend_out <- c("Out-Deg 0", "Out-Deg 1-100", "Out-Deg 100-1000", "Out-Deg > 1000")

get.degree <- function(graph, mode = mode){
  table(cut(degree(graph, mode = mode), breaks = breaks, right = F))
}

in_degree_ts <- sapply(GraphYear, FUN = get.degree, USE.NAMES = F, mode = "in")
out_degree_ts <- sapply(GraphYear, FUN = get.degree, USE.NAMES = F, mode = "out")

windows(width=10, height=6)
par(mfrow = c(1,2), lwd = 3, xpd = TRUE)
matplot(xAchse, t(in_degree_ts), type = "l", col = colors, ylim = yAchse,
        lwd = 3,
        main = "In-Degree-Entwicklung 1992 - 2012",
        xlab = "Jahr",
        ylab = "Anzahl Länder")
legend("topright", inset = 0, legend = legend_in, col = colors_in, 
       lty = 1:4,
       lwd = 3,
       cex = 0.7)

matplot(xAchse, t(out_degree_ts), type = "l", col = colors_out, ylim = yAchse,
        lwd = 3,
        main = "Out-Degree-Entwicklung 1992 - 2012",
        xlab = "Jahr",
        ylab = "Anzahl Länder")
legend("topright", inset = 0, legend = legend_out, col = colors_out, 
       lty = 1:4,
       lwd = 3,
       cex = 0.7)





SMI <- EuStockMarkets[, "SMI"]
plot(lag(SMI,  1), SMI, pch = ".")
plot(lag(SMI, 20), SMI, pch = ".", log = "xy",
     main = "4 weeks lagged SMI stocks -- log scale", xy.lines =  TRUE)


lends <- c("round","butt","square")
matplot(matrix(1:12, 4), type="c", lty=1, lwd=10, lend=lends)
text(cbind(2.5, 2*c(1,3,5)-.4), lends, col= 1:3, cex = 1.5)



nam.var <- colnames(iris)[-5]
nam.spec <- as.character(iris[1+50*0:2, "Species"])
iris.S <- array(NA, dim = c(50,4,3),
                dimnames = list(NULL, nam.var, nam.spec))
for(i in 1:3) iris.S[,,i] <- data.matrix(iris[1:50+50*(i-1), -5])

matplot(iris.S[, "Petal.Length",], iris.S[, "Petal.Width",], pch = "SCV",
        col = rainbow(3, start = 0.8, end = 0.1),
        sub = paste(c("S", "C", "V"), dimnames(iris.S)[[3]],
                    sep = "=", collapse= ",  "),
        main = "Fisher's Iris Data")



## Multivariate
z <- ts(matrix(rt(200 * 8, df = 3), 200, 8),
        start = c(1961, 1), frequency = 12)
plot(z, yax.flip = TRUE)
plot(z, axes = FALSE, ann = FALSE, frame.plot = TRUE,
     mar.multi = c(0,0,0,0), oma.multi = c(1,1,5,1))
title("plot(ts(..), axes=FALSE, ann=FALSE, frame.plot=TRUE, mar..., oma...)")
=======
######## Grafiken erstellen
par(mfrow= c(1,1), mar = c(7,5,5,2))
#In-Degree
plot(cut(degree(Gr, mode = "in")/20, c(1,seq(from = 0, to = 100, by = 5), max(degree(Gr, mode = "in")/20)),right = F, dig.lab=4), main = "Durchschnittlicher In-Degree", las = 2, ylab = "durchschnittliche Häufigkeit" )
#png(filename = "Grafiken/Durchschnittlicher In-Degree.png")
#Out-Degree
plot(cut(degree(Gr, mode = "out")/20, c(1,seq(from = 0, to = 100, by = 5), max(degree(Gr, mode = "out")/20)),right = F, dig.lab=4), main = "Durchschnittlicher Out-Degree", las = 2, ylab = "durchschnittliche Häufigkeit")
#png(filename = "Grafiken/Durchschnittlicher Out-Degree.png")

# Zeitreihe Teilnehmer
plot(year,vertices, main = "Teilnehmer", pch = 1, cex = 2, ylab = "#Teilnehmer", xlab = "Jahr") # Teilnehmer
#png(filename = "Grafiken/Teilnehmer Zeitreihe.png")
>>>>>>> 56fe29289f178678fee42fe5248d31bc80bb80ec
