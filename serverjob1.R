#ein Jahr zum testen auswählen. z.B. 10tes Jahr ist 2001

library(ergm)
library(statnet)
load("ergmWS.RData")

G <- NetGraphYearSimple[[10]]

my.ergm.formula <- formula(G ~ edges +
                              mutual +
                              gwidegree(0.9, fixed = F) +
                              gwodegree(0.9, fixed = F) + 
                              gwesp(0.9, fixed = F) +
                              gwdsp(0.9, fixed = F) +
                              nodeicov("ext_cinc") + nodeocov("ext_cinc") +
                              nodeicov("ext_conflict") + nodeocov("ext_conflict") +
                              nodeicov("ext_gdp") + nodeocov("ext_gdp") +
                              nodeicov("ext_cinc") + nodeocov("ext_cinc") +
                              nodeifactor("Continent") + nodeofactor("Continent")+
                              absdiff("ext_polity") +
                              edgecov(AAlliance[[10]]) + 
                              edgecov(ADirectCont[[10]]) 
                              )
summary.statistics(my.ergm.formula)
my.ergm.fit <- ergm(my.ergm.formula)

save.image("myErgmWs.RData")