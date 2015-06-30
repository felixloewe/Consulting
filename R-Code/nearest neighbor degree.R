#avergage nearest neighbor plot

for (i in 1:20){
#degree sequenz:
deg <- degree(GraphYearSimple[[i]])

#avergage nearest neighbor degree sequenz
ann <- graph.knn(GraphYearSimple[[i]])$knn

#plot aufruf
plot(log(deg), log(ann), xlim = c(1,6), ylim = c(1,6), xlab = "Log Degree", ylab = "Log Average Neighbor Degree")
title(paste( "Jahr",1991 + i))
}

#gut erkennbar zum beispiel im jahr 1998
windows(6,6)
#degree sequenz:
deg <- degree(GraphYearSimple[[7]])

#avergage nearest neighbor degree sequenz
ann <- graph.knn(GraphYearSimple[[7]])$knn

#plot aufruf
plot(log(deg), log(ann), xlim = c(1,6), ylim = c(1,6), xlab = "Log Degree", ylab = "Log Average Neighbor Degree")



savePlot(filename = "Bericht/Grafiken/and" , type = "png")
