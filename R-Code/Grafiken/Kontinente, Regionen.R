# Übersichtliche Netzwerkgrafiken mit Kontinenten und Regionen

# Optionen:
# - Jahr(year)
# - Aufteilung nach "Region" oder "Continent" (groups)
# - seed
# - Faktor für edge.width (ew)
# - Faktor für vertex.size (vs)


Continent.plot <- function(year, groups = "Region", seed = 1, ew = 0.5, vs = 0.5){
  #Jahr auswählen
  Gr <- GraphYear[[year-1991]]
  
  #Kontinente oder Regionen?
  if(groups == "Continent"){sep_char <- V(Gr)$Continent}
  if(groups == "Region"){sep_char <- V(Gr)$Region}
  
  #numerischen Vektor mit Gruppenzuweisung erstellen
  sep_fac <- as.factor(sep_char)
  sep_nums <- as.numeric(sep_fac)
  
  #Wert für NA's hinzufügen
  sep_nums[is.na(sep_nums)] <- max(sep_nums, na.rm = T) + 1
  
  #gruppiertes Graph-Objekt erstellen
  groupedGraph <- contract.vertices(graph = Gr, mapping = sep_nums)
  
  #Plot Parameter:
  vertex.size <- sqrt(as.vector(degree(groupedGraph, mode = "total"))) # Größe der Knoten proportional zur Handelsanzahl
  vertex.label <- c(names(table(sep_char)), "Rest")
  edge.width <-  0.005*sqrt(E(Gr)$Value) # Breite der Kanten proportional zum Handelswert
  groupedGraph <- simplify(groupedGraph)
  #Plot Aufruf
  #windows(height = 10, width = 10)
  set.seed(seed)
  par(mar = c(1,1,2,1))
  plot.igraph(groupedGraph,
       vertex.size = vs*vertex.size,
       vertex.label = vertex.label,
       vertex.label.dist = 1.5,
       vertex.label.degree = c(0,-pi/2,-pi/2,pi,pi/2,pi/2),
       vertex.label.color = "black"
       vertex.color = "skyblue1",
       edge.width = ew*edge.width,
       edge.color = "royalblue3",
       layout = layout.circle(groupedGraph), # Layout fixieren
       edge.arrow.size = 0,
       vertex.label.cex = 2
  )
  title (paste(year), cex.main = 2)
  
}

Continent.plot(1992, groups = "Continent")


windows(width = 9, height = 6)
for (i in 1:20){
  Continent.plot(1991 + i, groups = "Continent")
  savePlot(paste("Bericht/Grafiken/Cont_Ani/cont", i, sep = ""), type = "pdf")  
  #Continent.plot(1991 + i, groups = "Region")
  #savePlot(paste("Bericht/Grafiken/Reg_Ani/reg", i, sep = ""), type = "pdf")  
}
