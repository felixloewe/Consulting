## Erster Versuch Exponential-Random-Graph Modell
#install.packages("statnet")
library(ergm) # ERGM-Paket
library(sand) # SAND-Paket
library(statnet)
## Graph Year in ERGM-Format bringen

A <- get.adjacency(GraphYear[[10]])
v.attrs <- get.data.frame(GraphYear[[10]], what = "vertices")
e.attrs <- get.data.frame(GraphYear[[10]], what = "edges")
head(e.attrs)
ergm.Graph1.s <- network::as.network(as.matrix(A), directed = T)
network::set.vertex.attribute(Graph1.s, "Country_Code", v.attrs$name)
network::set.vertex.attribute(Graph1.s, "Country_Name", v.attrs$Country_Name)
network::set.vertex.attribute(Graph1.s, "Continent", v.attrs$Continent)
network::set.vertex.attribute(Graph1.s, "Region", v.attrs$Region)
network::set.edge.attribute(Graph1.s, "Year", e.attrs$Year)
network::set.edge.attribute(Graph1.s, "Value", e.attrs$Value)
network::set.edge.attribute(Graph1.s, "Data_Source", e.attrs$Data_Source)
network::set.edge.attribute(Graph1.s, "IsMirror", e.attrs$IsMirror)
network::set.edge.attribute(Graph1.s, "PRIO_Weapons_Code", e.attrs$PRIO_Weapons_Code)





#####################Modell nach Empfehlung aus Buch: ERGM for Social Networks (Robins)

# empfohlene Statistiken:
# - edges
# This is a baseline propensity for the formation
# - reciprocity (mutual)
# This is often positive, suggesting that reciprocated ties are very likely
# to be observed for positive networks
# - popularity spread (gwidegree)
# A negative popularity spread parameter indicates that most actors have
# simular levels of popularity (the network is not centralized on in - degree)
# - activity spread (gwodegree)
# A negative activity spread parameter indicates that most actors have
# similar levels of activity 
# - some form of alternating triangle and cyclic parameters (gwesp, ctriple)
# - Connectivity (gwdsp)


#robin.ergm <- formula(Graph1.s ~ edges + mutual + gwidegree(0.25) + gwodegree(0.25) + istar(2) + ostar(2) + gwesp + ctriple + gwdsp)
#summary.statistics(robin.ergm)
#robin.ergm.fit <- ergm(robin.ergm)
#summary.ergm(robin.ergm.fit)
# Volles Modell degeneriert/ funktioniert nicht

#einzelne Statistiken von Robin ausprobieren


robin.ergm1 <- formula(Graph1.s ~ edges + gwidegree(0.25, fixed = T))
robin.ergm2 <- formula(Graph1.s ~ edges + gwodegree(0.25, fixed = T))
robin.ergm3 <- formula(Graph1.s ~ edges + gwesp(fixed = T))
robin.ergm4 <- formula(Graph1.s ~ edges + ctriple)
robin.ergm5 <- formula(Graph1.s ~ edges + gwdsp(fixed = T))
robin.ergm6 <- formula(Graph1.s ~ edges + mutual)

summary.statistics(robin.ergm1)
summary.statistics(robin.ergm2)
summary.statistics(robin.ergm3)
summary.statistics(robin.ergm4)
summary.statistics(robin.ergm5)
summary.statistics(robin.ergm6)

#robin.ergm1.fit <- ergm(robin.ergm1) # konvergiert
summary.ergm(robin.ergm1.fit)

#robin.ergm2.fit <- ergm(robin.ergm2) # läuft durch aber konvergiert nicht
summary.ergm(robin.ergm2.fit)

#robin.ergm3.fit <- ergm(robin.ergm3) # konvergiert
summary.ergm(robin.ergm3.fit)

#robin.ergm4.fit <- ergm(robin.ergm4) # Fehlermeldung. degeneriert anscheinend
summary.ergm(robin.ergm4.fit)

#robin.ergm5.fit <- ergm(robin.ergm5) # läuft durch aber konvergiert nicht
summary.ergm(robin.ergm5.fit)

#robin.ergm6.fit <- ergm(robin.ergm6) # konvergiert
summary.ergm(robin.ergm6.fit)


# mcmc.diagnostics nutzen um herauszufinden welche statistiken für degeneration verantwortlich sind:

# Dazu in Vorgängerarbeit:
#
# Die Abbildungen sind hierbei jeweils wie folgt zu interpretieren: Die linke Seite zeigt
# für jede in das jeweilige Modell integrierte Statistik die Werte, welche diese Statistik
# auf den via MCMC simulierten Netzwerken annimmt (man nennt diese Abbildungen
# auch trace plots). Diese Werte sind hierbei um den Wert der jeweiligen Statistik auf
# dem beobachteten Netzwerk zentriert. Die rechte Seite zeigt die empirische Dichtefunktion
# über den möglichen (zentrierten) Zuständen der betreffenden Statistik, basierend
# auf den simulierten Netzwerken
# [...]
# Wie sieht nun also eine gute MCMC-Diagnose aus? Die empirische Dichtefunktion
# sollte für jede zentrierte integrierte Statistik 􀀀# symmetrisch um Null verlaufen, da
# der Erwartungswert der zentrierten Statistik ...
#
# gerade Null betragen sollte. Ist dies nicht der Fall, so weichen die konstruierten Netzwerke
# in der betreffenden Statistik systematisch vom beobachteten Netzwerk ab und
# können somit nicht als aus der selben Verteilung stammend angenommen werden.
# Zusätzlich sollten die Trajektorien auf der rechten Seite weder eine Abhängigkeitsstruktur
# erkennbar werden lassen, noch konstant auf einem Wert verharren. Ersteres
# wäre ein Indikator dafür, dass der konstruierte stochastische Prozess die Markov-
# Eigenschaft verletzt, letzteres dafür, dass er konstant auf einem Netzwerk verharrt.

mcmc.diagnostics(robin.ergm1.fit) #passt (gwidegree)
mcmc.diagnostics(robin.ergm2.fit) # schlecht
mcmc.diagnostics(robin.ergm3.fit) # schlecht
mcmc.diagnostics(robin.ergm4.fit) # schlecht
mcmc.diagnostics(robin.ergm5.fit) # schlecht
mcmc.diagnostics(robin.ergm6.fit) # passt (mutual)

##### mutual, gwidegree 

robin.ergm16 <- formula(Graph1.s ~ edges + mutual + gwidegree(0.25, fixed = T))
summary.statistics(robin.ergm16)
robin.ergm16.fit <- ergm(robin.ergm16)
summary.ergm(robin.ergm16.fit)
mcmc.diagnostics(robin.ergm16.fit) ### passt

#####ersatz suchen für restliche statistiken
robin.ergm7 <- formula(Graph1.s ~ edges + ostar(2))
robin.ergm8 <- formula(Graph1.s ~ edges + dsp(1))
robin.ergm9 <- formula(Graph1.s ~ edges + esp(1))
robin.ergm10 <- formula(Graph1.s ~ edges + cycle(4)) # The directed cycle terms of lengths 2 and 3
# are equivalent to mutual and ctriple (respectively)

robin.ergm7.fit <- ergm(robin.ergm7)
robin.ergm8.fit <- ergm(robin.ergm8)
robin.ergm9.fit <- ergm(robin.ergm9)
robin.ergm10.fit <- ergm(robin.ergm10)

mcmc.diagnostics(robin.ergm7.fit) # passt nicht
mcmc.diagnostics(robin.ergm8.fit) #passt (dsp(1))
mcmc.diagnostics(robin.ergm9.fit) #passt (esp(1))
mcmc.diagnostics(robin.ergm10.fit)

#dsp und esp aufnehmen
robin.ergm1689 <- formula(Graph1.s ~ edges + mutual + gwidegree(0.25, fixed = T) + dsp(1) + esp(1))
summary.statistics(robin.ergm1689)
robin.ergm1689.fit <- ergm(robin.ergm1689)
summary.ergm(robin.ergm1689.fit)
mcmc.diagnostics(robin.ergm1689.fit)











############################################################################################################
############################################################################################################

robin.ergm136 <- formula(Graph1.s ~ edges + mutual + gwidegree(0.25, fixed = T) + gwesp(fixed = T))
summary.statistics(robin.ergm136)
robin.ergm136.fit <- ergm(robin.ergm136)
summary.ergm(robin.ergm136.fit)
mcmc.diagnostics(robin.ergm136.fit)

robin.ergm13 <- formula(Graph1.s ~ edges + gwidegree(0.25, fixed = T) + gwesp(fixed = T))
summary.statistics(robin.ergm13)
robin.ergm13.fit <- ergm(robin.ergm13)
summary.ergm(robin.ergm13.fit)
mcmc.diagnostics(robin.ergm13.fit)


#modell mit allen statistiken degeneriert
#versuch mit minimalempfehlung(density, degree distribution and closure)
robin.ergm.min <- formula(Graph1.s ~ edges + mutual + istar(c(1,2,3)) + ostar(c(1,2,3)) + esp(c(1,2,3)))
summary.statistics(robin.ergm.min)
robin.ergm.fit <- ergm(robin.ergm.min)
#degeneriert auch
#versuch einfachstes modell schrittweise erweitern
robin.ergm.base <- formula(Graph1.s ~ edges)
summary.statistics(robin.ergm.base)
robin.ergm.base.fit <- ergm(robin.ergm.base)
summary.ergm(robin.ergm.base.fit)

robin.ergm.base <- formula(Graph1.s ~ edges + mutual)
summary.statistics(robin.ergm.base)
robin.ergm.base.fit <- ergm(robin.ergm.base)
summary.ergm(robin.ergm.base.fit)

robin.ergm.base <- formula(Graph1.s ~ edges + mutual + gwesp + ctriple + gwdsp)
summary.statistics(robin.ergm.base)
robin.ergm.base.fit <- ergm(robin.ergm.base)
summary.ergm(robin.ergm.base.fit)
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


