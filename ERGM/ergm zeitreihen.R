gwidegree <- vector(length = 20)
gwdsp <- vector(length = 20)
mutual <- vector(length = 20)
esp <- vector(length = 20)
dsp <- vector(length = 20)
for (i in 1:20){
  A <- get.adjacency(GraphYear[[i]])
  v.attrs <- get.data.frame(GraphYear[[i]], what = "vertices")
  e.attrs <- get.data.frame(GraphYear[[i]], what = "edges")
  head(e.attrs)
  Graph1.s <- network::as.network(as.matrix(A), directed = T)
  network::set.vertex.attribute(Graph1.s, "Country_Code", v.attrs$name)
  network::set.vertex.attribute(Graph1.s, "Country_Name", v.attrs$Country_Name)
  network::set.vertex.attribute(Graph1.s, "Continent", v.attrs$Continent)
  network::set.vertex.attribute(Graph1.s, "Region", v.attrs$Region)
  network::set.edge.attribute(Graph1.s, "Year", e.attrs$Year)
  network::set.edge.attribute(Graph1.s, "Value", e.attrs$Value)
  network::set.edge.attribute(Graph1.s, "Data_Source", e.attrs$Data_Source)
  network::set.edge.attribute(Graph1.s, "IsMirror", e.attrs$IsMirror)
  network::set.edge.attribute(Graph1.s, "PRIO_Weapons_Code", e.attrs$PRIO_Weapons_Code)
  
  
  
  
  ergm.gwidegree <- formula(Graph1.s ~ edges + gwidegree(0.9, fixed = T))
  #ergm.gwdsp <- formula(Graph1.s ~ edges + gwdsp(fixed = T))
  ergm.mutual <- formula(Graph1.s ~ edges + mutual)
  ergm.esp <- formula(Graph1.s ~ edges + esp(1))
  ergm.dsp <- formula(Graph1.s ~ edges + dsp(1))
  
  ergm.gwidegree.fit <- ergm(ergm.gwidegree)
  #ergm.gwdsp.fit <- ergm(ergm.gwdsp)
  ergm.mutual.fit <- ergm(ergm.mutual)
  ergm.esp.fit <- ergm(ergm.esp)
  ergm.dsp.fit <- ergm(ergm.dsp)
  
  gwidegree[i] <- as.numeric(ergm.gwidegree.fit$coef[2])
  #gwdsp[i] <- as.numeric(ergm.gwdsp.fit$coef[2])
  mutual[i] <- as.numeric(ergm.mutual.fit$coef[2])
  esp[i] <- as.numeric(ergm.esp.fit$coef[2])
  dsp[i] <- as.numeric(ergm.dsp.fit$coef[2])
  message(cat("iteration", i))
  
}

stats <- as.matrix(nrow=3, ncol = 20)
stats[1,] <- gwidegree
stats[2,] <- mutual
stats[3,] <- esp

par(mfrow = c(2,2), oma = c(2,4,2,2))

plot(gwidegree,
     type = "o",
     pch = 1,
     col = 1,
     lty = 1,
     lwd = 2,
     xlab = "Year",
     ylab = expression(theta[gwidegree]),
     main = "gwidegree(decay = 0.9, fixed = T)",
     xaxt = "n"    )
axis(1, at = 1:20, labels = Year)    
grid(lwd = 1)

plot(mutual,
     type = "o",
     pch = 1,
     col = 1,
     lty = 1,
     lwd = 2,
     xlab = "Year",
     ylab = expression(theta[mutual]),
     main = "mutual",
     xaxt = "n"    )
axis(1, at = 1:20, labels = Year)    
grid(lwd = 1)

plot(esp,
     type = "o",
     pch = 1,
     col = 1,
     lty = 1,
     lwd = 2,
     xlab = "Year",
     ylab = expression(theta[esp]),
     main = "esp(1)",
     xaxt = "n"    )
axis(1, at = 1:20, labels = Year)    
grid(lwd = 1)

plot(dsp,
     type = "o",
     pch = 1,
     col = 1,
     lty = 1,
     lwd = 2,
     xlab = "Year",
     ylab = expression(theta[dsp]),
     main = "dsp(1)",
     xaxt = "n"    )
axis(1, at = 1:20, labels = Year)    
grid(lwd = 1)

title(outer = T, "Time Series of Parameter Values (1992-2011)")

