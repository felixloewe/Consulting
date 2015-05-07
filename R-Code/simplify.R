# Datensatz vereinfachen
# - Loops und multiple Kanten werden entfernt
# - Die Attribute Value und ValueMil werden summiert. Year wird übernommen
# Ausgabe
# - GraphYearSimple


GraphYearSimple <- list()
for(i in 1:20) {
GraphYearSimple[[i]] <- simplify(GraphYear[[i]], 
                                 edge.attr.comb = list(Year = "first", Value = "sum", ValueMil = "sum",
                                                       alliance = "first", polity = "first", directcont = "first", "ignore" ))
}

