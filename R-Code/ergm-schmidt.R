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



G <- NetGraphYearSimple[[16]]
ergm.schmidt.exo.0 <- formula(G ~ edges + edgecov(AAlliance[[16]]) + edgecov(ADirectCont[[16]])  + edgecov(APolity[[16]]) + edgecov(APathDependency[[16]])
                                + nodeicov("ext_gdp") + nodeocov("ext_gdp")+ nodeicov("ext_cinc")
                                + nodeocov("ext_cinc") + nodeicov("ext_conflict"))

summary.statistics(ergm.schmidt.exo.0)
ergm.schmidt.exo.0.fit <- ergm(ergm.schmidt.exo.0)
summary.ergm(ergm.schmidt.exo.0.fit)
mcmc.diagnostics(ergm.schmidt.exo.0.fit)
plot(gof.ergm(ergm.schmidt.exo.0.fit))


ergm.schmidt.comb <- formula(G ~ edges + mutual + idegree(1) + esp(1) + dsp(1) 
                             + edgecov(AAlliance[[1]]) + edgecov(ADirectCont[[1]])  + edgecov(APolity[[1]])
                             + nodeicov("ext_gdp") + nodeocov("ext_gdp")+ nodeicov("ext_cinc") + nodeocov("ext_cinc") + nodeicov("ext_conflict"))

summary.statistics(ergm.schmidt.comb)
ergm.schmidt.comb.fit <- ergm(ergm.schmidt.comb)
xtable(summary.ergm(ergm.schmidt.comb.fit))
mcmc.diagnostics(ergm.schmidt.comb.fit)
ergm.schmidt.comb.gof <- gof.ergm(ergm.schmidt.comb.fit)
plot(ergm.schmidt.comb.gof)
save.image(file = "schmdit.ergm.workspace.RData")




table(APathDependency[[10]])


G <- NetGraphYearSimple[[10]]
ergm.schmidt.path <- formula(G ~ edges +  edgecov(APathDependency[[10]]))
                             

summary.statistics(ergm.schmidt.path)
ergm.schmidt.path.fit <- ergm(ergm.schmidt.path)
summary.ergm(ergm.schmidt.path.fit)
mcmc.diagnostics(ergm.schmidt.path)
plot(gof.ergm(ergm.schmidt.path))