# Funktion zum plotten eines Teilnetzwerkes
source("R-Code/Funktionen/Top.R")
# Optionen: 
# - Jahr (year), 
# - Anzahl hervorgehobene Top-Staaten (n_tops),
# - Größe Knoten - Stichprobe (N),
# - Import/Export (trade),
# - seed (seed),
# - simplify (simplify)

# Ausgabe: - Netzwerkgraph eines Jahres mit hervorgehobenen Top Exporteuren

topplot <- function(year, n_tops = 5, N = 10, seed = 1, trade = "e", simplify = F){
    
  #jahr auswählen
  Gr <- GraphYear[[year-1991]]
  
  #Exporttabelle erstellen
  if(trade == "e")
    top_tabelle <- Top_Export(year)
  if(trade == "i")
    top_tabelle <- Top_Import(year)
  #inducierter Subgraph ziehen
  ind <- induced.subgraph(Gr, vids = as.character(top_tabelle$ccode[1:N]))
  if (simplify) {ind <- simplify(ind)}
  #Knotenparameter einstellen
  V(ind)$size <- 2
  V(ind)[as.character(top_tabelle$ccode[1:n_tops])]$size <- 5
  V(ind)$label <- ""
  V(ind)[as.character(top_tabelle$ccode[1:n_tops])]$label <- as.character(top_tabelle$Land[1:n_tops])
  
  #Kanten Klassifizieren
  top <- V(ind)[as.character(top_tabelle$ccode[1:n_tops])]
  if (n_tops == N){
   flop <- c() 
  }
  else{
  flop <- V(ind)[as.character(top_tabelle$ccode[(n_tops+1):N])]
  }
  #Kantenparameter einstellen
  E(ind)[ flop %--% flop]$color <- "yellow"
  E(ind)[ flop %--% top]$color <- "green"
  E(ind)[ top %--% top]$color <- "red"
  
  #Grafikaufruf
  #Fenster erstellen
  windows(width = 10, height = 12)
  if(n_tops == N){
    par(mfrow = c(1,1))
  }
  else
    par(fig=c(0,0.8,0,1), new=F)
  #Netzwerkgrafik
  set.seed(seed)
  plot.igraph(ind, edge.arrow.size = 0.2)
  if (trade == "i"){title("Netzwerk der Top Importeure" )}
  if (trade == "e"){title("Netzwerk der Top Exporteure" )}
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

