# Liste externe Kovariablen
source("R-Code/Alliance.R")
source("R-Code/Conflict.R")
source("R-Code/GDP.R")
source("R-Code/CINC.R")
source("R-Code/Polity.R")
source("R-Code/directcont.R")

# GDP Knotenattribut: Bruttosozialprodukt, Skala Internation Dollar
# Conflict Knotenattribut 
# CINC Knotenattribut
# Polity Knotenattribut: Demokratie-Score, Skala -10 bis 10

# externe Kovariablen hinzufügen
# Vertex-Attribute

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


# Kantenattribute
# Alliance Kantenattribut: Allianzen (0,1)
# Polity Kantenattribut: Unterschied im Demokratie-Score, Skala 0 bis 20
# DirectCont: Gibt an, ob die Länder benachbart sind (0,1)

MADdata1 <- MADdata
MADdata1$alliance <- vector(length = nrow(MADdata1))
MADdata1$alliance <- 2
for(i in 20:1){
  A <- AAlliance[[-(1991-MADdata1$Year[i])]]
  if(as.character(MADdata1[i,]$Reporter_Code) %in% rownames(A) & 
     as.character(MADdata[i,]$Partner_Code) %in% colnames(A))
     {
  MADdata1$alliance[i] <- A[as.character(MADdata1[i,]$Reporter_Code), as.character(MADdata[i,]$Partner_Code)]
  }
  else{
    MADdata1$alliance[i] <- 0    
  }
}
rownames(AAlliance[[1]])
MADdata1 <- merge(x = MADdata, y = Alliance, 
          by.x = c("Reporter_Code", "Partner_Code", "Year"), 
          by.y = c("ccode1", "ccode2", "year"),
          all.y = TRUE)

MADdata[(MADdata$Reporter_Code == 31 & MADdata$Partner_Code == 200)
        | (MADdata$Reporter_Code == 200 & MADdata$Partner_Code == 31),]

MADdata[(MADdata$Reporter_Code == 2 & MADdata$Partner_Code == 115)
        | (MADdata$Reporter_Code == 115 & MADdata$Partner_Code == 2),]

Alliance[(Alliance$ccode1 == 31 & Alliance$ccode2 == 200)
        | (Alliance$ccode1 == 200 & Alliance$ccode2 == 31),]

sapply(AAlliance, FUN = function(Matrix) Matrix["115","2"])

Graph1.s <- network::as.network(as.matrix(AAlliance[[1]]), directed = T)

head(AllianceYear[[1]])

table(MADdata1$alliance)


#sum(duplicated(KnotenAttr$ccode))
