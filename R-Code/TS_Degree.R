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
par(xpd = F)
grid(lwd = 1)
legend("topright", inset = 0, legend = legend_in, col = colors_out, 
       lty = 1:4,
       lwd = 3,
       cex = 0.7,
       bg = "white")

matplot(xAchse, t(out_degree_ts), type = "l", col = colors_out, ylim = yAchse,
        lwd = 3,
        main = "Out-Degree-Entwicklung 1992 - 2012",
        xlab = "Jahr",
        ylab = "Anzahl Länder")
par(xpd = F)
grid(lwd = 1)
legend("topright", inset = 0, legend = legend_out, col = colors_out, 
       lty = 1:4,
       lwd = 3,
       cex = 0.7,
       bg = "white")


# Teilnehmer und Dichte-Zeitreihe
par(mfrow = c(1,2), lwd = 1, xpd = F)
col_neutral <- "#2c7fb8"
col_points <- "grey"
xlim <- c(1950,2015)

plot(xAchse, vertices, type = "l", col = col_neutral, ylim = c(0,250),
        lwd = 3,
        main = "Teilnehmer-Entwicklung 1992 - 2012",
        xlab = "Jahr",
        ylab = "Anzahl")
grid(lwd = 1)
points(xAchse, vertices, pch = 21, col = col_points, bg = col_points)

plot(xAchse, trans, type = "l", col = col_neutral, ylim = c(0,1),
     lwd = 3,
     main = "Dichte-Entwicklung 1992 - 2012",
     xlab = "Jahr",
     ylab = "Dichte")
grid(lwd = 1)
points(xAchse, trans, pch = 21, col = col_points, bg = col_points)


# Einseitiger Handel
adj_ts <- sapply(GraphYear, get.adjacency, type = c("both"), sparse = F, edges = F, names = F)
adj_ts <- as.matrix(adj_ts)
symmetric_ts <- sapply(adj_ts, FUN = function(matrix) sum(matrix == t(matrix)))

get.tradecount <- function(Graph){
  E(Graph)
}
