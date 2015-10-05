

library(ergm)
library(statnet)
load("ergmWS.RData")
#ein Jahr zum testen auswählen. z.B. 5tes Jahr ist 1996
G <- NetGraphYearSimple[[5]]

my.ergm.formula <- formula(G ~ edges +
                              mutual +
                              gwidegree(0.2, fixed = T) +
                              gwodegree(0.2, fixed = T) + 
                              gwesp(0.2, fixed = T) +
                              gwdsp(0.2, fixed = T) +
                              nodeicov("ext_cinc") + nodeocov("ext_cinc") +
                              nodeicov("ext_conflict") + nodeocov("ext_conflict") +
                              nodeicov("ext_gdp") + nodeocov("ext_gdp") +
                              nodeifactor("Continent") + nodeofactor("Continent")+
                              absdiff("ext_polity"),
                           seed = 1
                           )
summary.statistics(my.ergm.formula)
my.ergm.fit3 <- ergm(my.ergm.formula, verbose = T, interval = 10000)
save.image("myErgmWs3.RData")
my.ergm.gof3 <- gof.ergm(my.ergm.fit)
save.image("myErgmWs3.RData")