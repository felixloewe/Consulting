# Erstellen Grafik zu In- und Outdegree Verteilung 

# Ausgabe: - Grafik mit 2 Plots
#          - 

# Klassengrenzen
classes <- seq(from = 0, to = 100, by = 5)
extra_classes <- c(1, classes, 1000)  

# Durchschnittliche Degrees (Import-/ Exportanzahlen)
inDeg <- degree(Graph, mode = "in")/20
outDeg <-  degree(Graph, mode = "out")/20

# Klassieren und zählen
table(cut(inDeg, extra_classes, right = F, dig.lab = 4))
      ,

             
inDeg <- table(cut(degree(Graph, mode = "in")/20,
          extra_classes, 
          right = F, dig.lab=4))/241

outDeg <- table(cut(degree(Graph, mode = "out")/20,
                    c(1,seq(from = 0, to = 100, by = 5),
                      max(degree(Graph, mode = "out")/20)),
                    right = F, dig.lab=4))/241

####### Graphafiken erstellen
par(mfrow= c(2,1), mar = c(7,5,5,2))

plot(inDeg,
     main = "Durchschnittlicher In-Degree pro Jahr",
     las = 2, ylab = " relative Häufigkeit",
     col = "blue",
     lwd = 10)
grid(lwd = 1)

plot(outDeg,
     main = "Durchschnittlicher Out-Degree pro Jahr",
     las = 2, ylab = " relative Häufigkeit" ,
     col = "green",
     lwd = 10)
grid(lwd = 1)