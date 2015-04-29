# ?bersichtliche Netzwerkgrafiken mit Kontinenten und Regionen

# Optionen:
# - Jahr(year)
# - Aufteilung nach "Region" oder "Continent" (groups)
# - seed
<<<<<<< HEAD:R-Code/Grafiken/Koninente, Regionen.R
# - Faktor für edge.width (ew)
# - Faktor für vertex.size (vs)


=======
# - Faktor f?r edge.width (ew)
# - Faktor f?r vertex.size (vs)
>>>>>>> 5a5dea7560d5cd029c904d36ba7b570287c1db88:R-Code/Grafiken/Kontinente, Regionen.R
Continent.plot <- function(year, groups = "Region", seed = 1, ew = 0.5, vs = 0.5){
  #Jahr ausw?hlen
  Gr <- GraphYear[[year-1991]]
  
  #Kontinente oder Regionen?
  if(groups == "Continent"){sep_char <- V(Gr)$Continent}
  if(groups == "Region"){sep_char <- V(Gr)$Region}
  
  #numerischen Vektor mit Gruppenzuweisung erstellen
  sep_fac <- as.factor(sep_char)
  sep_nums <- as.numeric(sep_fac)
  
  #Wert f?r NA's hinzuf?gen
  sep_nums[is.na(sep_nums)] <- max(sep_nums, na.rm = T) + 1
  
  #gruppiertes Graph-Objekt erstellen
  groupedGraph <- contract.vertices(graph = Gr, mapping = sep_nums)
  
  #Plot Parameter:
  vertex.size <- sqrt(as.vector(degree(groupedGraph, mode = "total"))) # Gr??e der Knoten Proportional zur Handelsanzahl
  vertex.label <- c(names(table(sep_char)), "Rest")
  edge.width <-  0.005*sqrt(E(Gr)$Value) # Breite der Kanten proportional zum Handelswert
  groupedGraph <- simplify(groupedGraph)
  #Plot Aufruf
  set.seed(seed)
  plot(groupedGraph,
       vertex.size = vs*vertex.size,
       vertex.label = vertex.label,
       vertex.label.dist = 0,
       vertex.color = "white",
       edge.width = ew*edge.width,
       layout = layout.circle(groupedGraph) # Layout fixieren
       edge.arrow.size = 0
  )
  title (paste(year))
  
}