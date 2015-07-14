#Funktion zum plotten eines teilnetzwerkes
#Optionen: Jahr, Anzahl hervorgehobene Top-Staaten, Größe Knoten - Stichprobe

topplot <- function(year, n_tops, N, seed = 1){
    
  #jahr auswählen
  Gr <- GraphYear[[year-1991]]
  #Exporttabelle erstellen
  top_export <- Top_Export(year)
  #inducierter Subgraph ziehen
  ind <- induced.subgraph(Gr, vids = as.character(top_export$Land[1:N]))
  
  #Knotenparameter einstellen
  V(ind)$size <- degree(ind)*20/max(degree(ind))
  #V(ind)[as.character(top_export$Land[1:n_tops])]$size <- 5
  V(ind)$label <- ""
  V(ind)[as.character(top_export$Land[1:n_tops])]$label <- as.character(top_export$Land[1:n_tops])
  
  #Kanten Klassifizieren
  top <- V(ind)[as.character(top_export$Land[1:n_tops])]
  if (n_tops == N){
   flop <- c() 
  }
  else{
  flop <- V(ind)[as.character(top_export$Land[(n_tops+1):N])]
  }
  #Kantenparameter einstellen
  E(ind)[ flop %--% flop]$color <- "yellow"
  E(ind)[ flop %--% top]$color <- "green"
  E(ind)[ top %--% top]$color <- "red"
  
  #Grafikaufruf
  #Fenster erstellen
  if(n_tops == N){
    par(mfrow = c(1,1))
  }
  else
    par(fig=c(0,0.8,0,1), new=F)
  #Netzwerkgrafik
  set.seed(seed)
  plot.igraph(ind, edge.arrow.size = 0.2, main = "Netzwerk")
  #Barplot Grafik
  flopflop <- length(E(ind)[ flop %--% flop])
  floptop <-  length(E(ind)[ flop %--% top])
  toptop <- length(E(ind)[ top %--% top])
  if(N != n_tops){
    par(fig=c(0.8,1,0.25,0.75), new=TRUE)
  barplot(c(toptop,floptop,flopflop)/ sum(c(flopflop,floptop,toptop)),
          ylim = c(0,1), col = c("red", "green", "yellow"),
          names.arg = c("Top <-> Top", "Top <-> Andere", "Andere <-> Andere"),
          main = "Verteilung \ndes Handels",
          las = 2,
          
          )
  }
}


