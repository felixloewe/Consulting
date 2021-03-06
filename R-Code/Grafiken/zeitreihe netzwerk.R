windows(height = 24, width = 36)
par(mfrow = c(1,2), oma = c(0,0,0,0), mar = c (1,1,1,1))

years<- c(1992, 2011)


for(year in years){
  Gr <- GraphYearSimple[[year - 1991]]
  V(Gr)$size <- 1
  
  bigImp <- which(degree(Gr, mode = "in") >= 30)
  bigExp <- which(degree(Gr, mode = "out") >= 30)
  col <- rep("black", times = length(V(Gr)))
  col[which(is.element(V(Gr), bigImp))] <- "green"
  col[which(is.element(V(Gr), bigExp))] <- "blue"
  col[which(is.element(V(Gr), bigImp) & is.element(V(Gr), bigExp))] <- "red"
  
  
  plot(Gr,
       vertex.size = 4,
       vertex.label = "",
       vertex.label.dist = 0.5,
       vertex.color = col,
       edge.width = 1,
       edge.arrow.size = 0,
       vertex.label.color = "red"
  )
}
savePlot("Bericht/Grafiken/ts_network", type = "png")


  Gr <- GraphYearSimple[[1]]
bigImp <- which(degree(Gr, mode = "in") >= 30)
bigExp <- which(degree(Gr, mode = "out") >= 30)
  V(Gr)$Country_Name[which(is.element(V(Gr), bigImp) & is.element(V(Gr), bigExp))]


