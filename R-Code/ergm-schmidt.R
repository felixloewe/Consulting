source("R-Code/read-in.R")

# Modell von Christian Schmidt

# edges+ gwodegree(1, fixed=F)+idegree(1)+dsp(0)+esp(0)+
# edgecov(defense agreement) + edgecov(direct contiguity) +
# nodeicov(GDP) + nodeocov(GDP)+ nodeicov(CINC) + nodeocov(CINC) +
# edgecov(democracy score) + nodeicov(intra-state conflict) +
# edgecov(path dependency)

# Erster Versuch ohne externe Einflussfaktren

schmidt.endo <- formular()