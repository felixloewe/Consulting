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
  vertex.size <- sqrt(as.vector(degree(groupedGraph, mode = "total"))) # Größe der Knoten Proportional zur Handelsanzahl
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
       
       edge.arrow.size = 0
  )
  title (paste(year))
  
}