# Zeitreihen Degree-Werte plotten
breaks <- c(0,1,100,1000,max(degree(Gr))) # Klassen
xAchse <- year
yAchse <- c(0,250)
colors_in <- c("#b2e2e2", "#66c2a4", "#238b45", "#006d2c") # Farben
colors_out <- c("#b3cde3", "#8c96c6", "#8856a7", "#810f7c") # Farben
legend_in <- c("In-Deg 0", "In-Deg 1-100", "In-Deg 100-1000", "In-Deg > 1000")
legend_out <- c("Out-Deg 0", "Out-Deg 1-100", "Out-Deg 100-1000", "Out-Deg > 1000")

get.degree <- function(graph, mode = mode){
  table(cut(degree(graph, mode = mode), breaks = breaks, right = F))
}

in_degree_ts <- sapply(GraphYear, FUN = get.degree, USE.NAMES = F, mode = "in")
out_degree_ts <- sapply(GraphYear, FUN = get.degree, USE.NAMES = F, mode = "out")

windows(width=10, height=6)
par(mfrow = c(1,2), lwd = 3, xpd = TRUE)
matplot(xAchse, t(in_degree_ts), type = "l", col = colors, ylim = yAchse,
        lwd = 3,
        main = "In-Degree-Entwicklung 1992 - 2012",
        xlab = "Jahr",
        ylab = "Anzahl Länder")
legend("topright", inset = 0, legend = legend_in, col = colors_in, 
       lty = 1:4,
       lwd = 3,
       cex = 0.7)

matplot(xAchse, t(out_degree_ts), type = "l", col = colors_out, ylim = yAchse,
        lwd = 3,
        main = "Out-Degree-Entwicklung 1992 - 2012",
        xlab = "Jahr",
        ylab = "Anzahl Länder")
legend("topright", inset = 0, legend = legend_out, col = colors_out, 
       lty = 1:4,
       lwd = 3,
       cex = 0.7)