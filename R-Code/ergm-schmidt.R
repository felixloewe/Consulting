source("R-Code/read-in.R")

install.packages("statnet")
library(ergm) # ERGM-Paket
library(statnet)
# Modell von Christian Schmidt

# edges+ gwodegree(1, fixed=F)+idegree(1)+dsp(0)+esp(0)+
# edgecov(defense agreement) + edgecov(direct contiguity) +
# nodeicov(GDP) + nodeocov(GDP)+ nodeicov(CINC) + nodeocov(CINC) +
# edgecov(democracy score) + nodeicov(intra-state conflict) +
# edgecov(path dependency)

# Erster Versuch ohne externe Einflussfaktren
G <- NetGraphYearSimple[[1]]
ergm.schmidt.endo <- formula(G ~ edges+ gwodegree(1, fixed=F)+idegree(1)+dsp(0)+esp(0))
                        #+ edgecov(AAlliance[[1]]) + edgecov(ADirectCont[[1]])
                        #+ nodeicov("ext_mag"))
                        #+ edgecov(AAlliance[[1]]) + edgecov(ADirectCont[[1]])  + edgecov(APolity[[1]])
                        #+ nodeicov("ext_gdp") + nodeocov("ext_gdp")+ nodeicov("ext_cinc")
                        #+ nodeocov("ext_cinc") + nodeicov("ext_mag"))
)
summary.statistics(ergm.schmidt)

#erm.schmidt.endo.fit <- ergm(ergm.schmidt.endo) 
#save(erm.schmidt.endo.fit, file = "ergm.schmidt.endo.fit.RData")
load("ergm.schmidt.endo.fit.RData")
summary.ergm(erm.schmidt.endo.fit)
mcmc.diagnostics(erm.schmidt.endo.fit)



G <- NetGraphYearSimple[[1]]
ergm.schmidt.exo <- formula(G ~ edges + dsp(0) + esp(0) + edgecov(AAlliance[[1]]) + edgecov(ADirectCont[[1]])  + edgecov(APolity[[1]])
                                + nodeicov("ext_gdp") + nodeocov("ext_gdp")+ nodeicov("ext_cinc")
                                + nodeocov("ext_cinc") + nodeicov("ext_conflict"))

summary.statistics(ergm.schmidt.exo)

ergm.schmidt.exo.fit <- ergm(ergm.schmidt.exo)
summary.ergm(ergm.schmidt.exo.fit)
