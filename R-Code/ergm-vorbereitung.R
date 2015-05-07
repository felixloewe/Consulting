# Liste externe Kovariablen
source("R-Code/Alliance.R")
source("R-Code/Conflict.R")
source("R-Code/GDP.R")
source("R-Code/CINC.R")
source("R-Code/Polity.R")
source("R-Code/directcont.R")

# Ausgabe: NetGraphYearSimple: Liste mit jährlichen Graphen mit externen Attributen (Kante und Knoten)
                                          


# externe Kovariablen hinzufügen

# Kantenattribute
# Alliance Kantenattribut: Allianzen (0,1)
# Polity Kantenattribut: Unterschied im Demokratie-Score, Skala 0 bis 20
# DirectCont: Gibt an, ob die Länder benachbart sind (0,1)

# MADdata1 <- MADdata
# MADdata1$alliance <- vector(length = nrow(MADdata1))
# MADdata1$polity <- vector(length = nrow(MADdata1))
# MADdata1$directcont <- vector(length = nrow(MADdata1))
# MADdata1$alliance <- 2
# MADdata1$polity <- 2
# MADdata1$directcont <- 2
# 
# for(i in nrow(MADdata1):1){
#   #Alliance
#   A <- AAlliance[[-(1991-MADdata1$Year[i])]]
#   if(as.character(MADdata1[i,]$Reporter_Code) %in% rownames(A) & 
#        as.character(MADdata[i,]$Partner_Code) %in% colnames(A))
#   {
#     MADdata1$alliance[i] <- A[as.character(MADdata1[i,]$Reporter_Code), as.character(MADdata[i,]$Partner_Code)]
#   }
#   else{
#     MADdata1$alliance[i] <- 0    
#   }
#   #Polity
#   P <- APolity[[-(1991-MADdata1$Year[i])]]
#   if(as.character(MADdata1[i,]$Reporter_Code) %in% rownames(P) & 
#        as.character(MADdata[i,]$Partner_Code) %in% colnames(P))
#   {
#     MADdata1$polity[i] <- P[as.character(MADdata1[i,]$Reporter_Code), as.character(MADdata[i,]$Partner_Code)]
#   }
#   else{
#     MADdata1$polity[i] <- 0    
#   }
#   #DirectCont
#   D <- ADirectCont[[-(1991-MADdata1$Year[i])]]
#   if(as.character(MADdata1[i,]$Reporter_Code) %in% rownames(D) & 
#        as.character(MADdata[i,]$Partner_Code) %in% colnames(D))
#   {
#     MADdata1$directcont[i] <- D[as.character(MADdata1[i,]$Reporter_Code), as.character(MADdata[i,]$Partner_Code)]
#   }
#   else{
#     MADdata1$directcont[i] <- 0    
#   }
# }
# save(MADdata1, file = "MADdata1.RData")
load("MADdata1.RData")
# Gesamtgraph erstellen (iGraph-Objekt)
Graph <- graph.data.frame(MADdata1)
# Vertex Attribute hinzufügen:
# Ländername
V(Graph)$Country_Name <- countrycode(V(Graph)$name, "cown", "country.name", warn = F)
# Kontinent
V(Graph)$Continent <- countrycode(V(Graph)$name, "cown", "continent", warn = F)
# Region
V(Graph)$Region <- countrycode(V(Graph)$name, "cown", "region", warn = F)
# "Americas" in "America" abändern
V(Graph)$Continent[V(Graph)$Continent == "Americas"] <- "America"

# iGraph-Objekt pro Jahr erstellen (20 Jahre, 1992 - 2011)
Year <- 1992:2011
GraphYear <- lapply(Year, function(jahr) subgraph.edges(Graph, E(Graph)[Year==jahr]))

source("R-Code/simplify.R")

# Vertex-Attribute

# GDP Knotenattribut: Bruttosozialprodukt, Skala Internation Dollar
# Conflict Knotenattribut 
# CINC Knotenattribut
# Polity Knotenattribut: Demokratie-Score, Skala -10 bis 10

# Knotenattributsliste initialisieren
KnotenAttrYear <- list()

# Kovariablen zusammenmergen (Vertex-Attributes)
for(i in 1:20){
KnotenAttr <- merge(V(GraphYearSimple[[i]])$name, PolityYear[[1]], all.x = T, all.y = F, by.x = 1, by.y = "ccode")
names(KnotenAttr)[1] <- "ccode"
KnotenAttr <- merge(KnotenAttr, GDPYear[[i]], all.x = T, all.y = F, by = "ccode")
KnotenAttr <- merge(KnotenAttr, CINCYear[[i]], all.x = T, all.y = F, by = "ccode")
KnotenAttr <- merge(KnotenAttr, ConflictYear[[i]], all.x = T, all.y = F, by = "ccode")
KnotenAttr <- KnotenAttr[,c("ccode", "polity", "gdp", "cinc", "Mag")]
KnotenAttrYear[[i]] <- KnotenAttr
}

# V-Attributes an Graph-Objekt anhängen
for(i in 1:20){
V(GraphYearSimple[[i]])$ext_polity <- KnotenAttrYear[[i]]$polity
V(GraphYearSimple[[i]])$ext_gdp <- KnotenAttrYear[[i]]$gdp
V(GraphYearSimple[[i]])$ext_cinc <- KnotenAttrYear[[i]]$cinc
V(GraphYearSimple[[i]])$ext_mag <- KnotenAttrYear[[i]]$Mag
}

NetGraphYearSimple <- list()
for (i in 1:20){
  A <- get.adjacency(GraphYearSimple[[i]])
  v.attrs <- get.data.frame(GraphYearSimple[[i]], what = "vertices")
  e.attrs <- get.data.frame(GraphYearSimple[[i]], what = "edges")
 
  Graph1.s <- network::as.network(as.matrix(A), directed = T)
  
  network::set.vertex.attribute(Graph1.s, "Country_Code", v.attrs$name)
  network::set.vertex.attribute(Graph1.s, "Country_Name", v.attrs$Country_Name)
  network::set.vertex.attribute(Graph1.s, "Continent", v.attrs$Continent)
  network::set.vertex.attribute(Graph1.s, "Region", v.attrs$Region)
  network::set.vertex.attribute(Graph1.s, "ext_polity", v.attrs$ext_polity)
  network::set.vertex.attribute(Graph1.s, "ext_gdp", v.attrs$ext_gdp)
  network::set.vertex.attribute(Graph1.s, "ext_cinc", v.attrs$ext_cinc)
  network::set.vertex.attribute(Graph1.s, "ext_conflict", v.attrs$ext_mag)
  
  network::set.edge.attribute(Graph1.s, "Year", e.attrs$Year)
  network::set.edge.attribute(Graph1.s, "Value", e.attrs$Value)
  network::set.edge.attribute(Graph1.s, "ext_directcont", e.attrs$directcont)
  network::set.edge.attribute(Graph1.s, "ext_polity", e.attrs$polity)
  network::set.edge.attribute(Graph1.s, "ext_alliance", e.attrs$alliance)
 
  NetGraphYearSimple[[i]] <- Graph1.s
}
