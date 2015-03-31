## Erster Versuch Exponential-Random-Graph Modell

library(ergm) # ERGM-Paket
library(sand) # SAND-Paket

## 1992 in ERGM-Format bringen

A <- get.adjacency(GraphYear[[1]])
v.attrs <- get.data.frame(GraphYear[[1]], what = "vertices")
e.attrs <- get.data.frame(GraphYear[[1]], what = "edges")

Graph1.s <- network::as.network(as.matrix(A), directed = T)

network::set.vertex.attribute(Graph1.s, "Name", v.attrs$name)
network::set.edge.attribute(Graph1.s, "Year", e.attrs$Year)
network::set.edge.attribute(Graph1.s, "Value", e.attrs$Value)
network::set.edge.attribute(Graph1.s, "Data_Source", e.attrs$Data_Source)


##################### Modell der Vorgängerarbeit:

my.ergm2 <- formula(Graph1.s ~ edges + asymmetric + idegree(1) + dsp(1))
summary.statistics(my.ergm2)

Gr.ergm2.fit <- ergm(my.ergm2)
anova.ergm(Gr.ergm2.fit)
summary.ergm(Gr.ergm2.fit)

mcmc.diagnostics(Gr.ergm2.fit)

gof.Gr.ergm2.fit <- gof(Gr.ergm2.fit)
par(mfrow = c(2,2))
plot(gof.Gr.ergm2.fit)

######### In-degree zwischen 2-10 falsch modelliert, Out degree nicht steil genug modelliert, mcmc-dignostic war gut, 

######Modell verbessern:

my.ergm3 <- formula(Graph1.s ~ edges + asymmetric + idegree(1) + idegrange(2, to = 10) + odegree(0) + dsp(1))
summary.statistics(my.ergm3)

Gr.ergm3.fit <- ergm(my.ergm3)

anova.ergm(Gr.ergm3.fit)
summary.ergm(Gr.ergm3.fit)

mcmc.diagnostics(Gr.ergm3.fit)

gof.Gr.ergm3.fit <- gof(Gr.ergm3.fit)
par(mfrow = c(2,2))
plot(gof.Gr.ergm3.fit)


