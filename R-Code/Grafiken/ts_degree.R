install.packages("ggplot2")
library("ggplot2")
indegreeYear <- lapply(GraphYearSimple, degree, mode = "in")
outdegreeYear <- lapply(GraphYearSimple, degree, mode = "out")

windows(height = 7, width = 9)
par(mfrow = c(2,1), mar = c(4,3,2,1))
boxplot(indegreeYear,  names = 1992:2011, main = "In-Degree", col = "skyblue1", las = 2, cex.axis = 1.2)
grid(lwd = 1)
boxplot(outdegreeYear, names = 1992:2011, main = "Out-Degree", col = "skyblue1", las = 2, cex.axis = 1.2)
grid(lwd = 1)
savePlot(filename = "Bericht/Grafiken/ts_degree", type = "png")



max(indegreeYear[[18]])


sum_bigval <- vector(length = 20)
sum_smallval <- vector(length = 20)
for( i in 1:20){
bigval <- E(GraphYearSimple[[i]])$ValueMil[E(GraphYearSimple[[i]])$ValueMil > quantile(E(GraphYearSimple[[i]])$ValueMil, probs = 0.99)]
smallval <- E(GraphYearSimple[[i]])$ValueMil[E(GraphYearSimple[[i]])$ValueMil <= quantile(E(GraphYearSimple[[i]])$ValueMil, probs = 0.99)]
sum_bigval[i] <- sum(bigval)
sum_smallval[i] <- sum(smallval)
}
sum_val <- matrix(c(sum_bigval,
            sum_smallval), nrow = 2)
windows(height = 4, width = 8)
par(mfrow = c(1,1))
barplot(sum_val, names = 1992:2011, ylim = c(0,5000), ylab = "Millionen Dollar", xlab = "Jahr", las = 2, col = c("skyblue1","royalblue3"))
grid(lwd = 1)
legend("topleft", c("Summierte Werte > 0.99-Quantil", "Summierte Werte <= 0.99-Quantil"), col = c("skyblue1","royalblue3"), pch = 15)
savePlot("Bericht/Grafiken/ts_value", type = "png")
