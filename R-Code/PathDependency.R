# Pathdependency
# Handelswerte der letzten 4 Jahre werden für jede gerichtete Kante aufsummiert
# Ausgabe:
# -APathDependency: Liste mit Ajajencymatritzen für jedes Jahr


# 
# APathDependency <- list()
# # Schleife für Jahre
# for(Year in 1992:2011){
#   #Jahr ausgeben für Fortschritt Kontrolle
#   cat(Year,"\n")
#   #Zeilennamen abgreifen
#   row.names <- V(GraphYear[[Year-1991]])$name
#   #Spaltennamen abgreifen
#   col.names <- V(GraphYear[[Year-1991]])$name
#   #Zeilenanzahl abgreifen
#   nrow <- length(row.names)
#   #Spaltenanzahl abgreifen
#   ncol <- length(col.names)
#   #Matrix initialisieren mit richtigen Dimensionen und Namen
#   mat <- matrix(nrow = nrow, ncol = ncol, dimnames = list(row.names, col.names))
#   #Schleife für Zeilen
#   for(i in row.names){
#     
#     #Schleife für Spalten
#     for(j in col.names){
#       
#       ids <- which(MADdata$Reporter_Code == i & MADdata$Partner_Code == j & MADdata$Year %in% (Year-4):(Year-1))
#       mat[as.character(i),as.character(j)] <- sum(MADdata$Value[ids], na.rm = T)
#     }
#   }
#   #Matrix abspeichern
#   APathDependency[[Year - 1991]] <- mat
# }
# 
# save(APathDependency, file = "APathDependency.Rdata")
load("APathDependency.Rdata")


