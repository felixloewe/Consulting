## Zeitreihen Degree-Werte plotten

# Klassen
breaks <- c(0,1,100,1000,max(degree(Graph)))
xAchse <- year
yAchse <- c(0,250)
colors_in <- c("#b2e2e2", "#66c2a4", "#238b45", "#006d2c") # Import-Farben
colors_out <- c("#b3cde3", "#8c96c6", "#8856a7", "#810f7c") # Export-Farben
legend_in <- c("In-Deg 0", "In-Deg 1-100", "In-Deg 100-1000", "In-Deg > 1000")
legend_out <- c("Out-Deg 0", "Out-Deg 1-100", "Out-Deg 100-1000", "Out-Deg > 1000")

get.degree <- function(graph, mode = mode){
  table(cut(degree(graph, mode = mode), breaks = breaks, right = F))
}

in_degree_ts <- sapply(GraphYear, FUN = get.degree, USE.NAMES = F, mode = "in")
out_degree_ts <- sapply(GraphYear, FUN = get.degree, USE.NAMES = F, mode = "out")

windows(width = 10, height = 6)
par(mfrow = c(1,2), lwd = 3, xpd = TRUE)
matplot(xAchse, t(in_degree_ts), type = "l", col = colors_in, ylim = yAchse,
        lwd = 3,
        main = "In-Degree-Entwicklung 1992 - 2012",
        xlab = "Jahr",
        ylab = "Anzahl Länder")
par(xpd = F)
grid(lwd = 1)
legend("topright", inset = 0, legend = legend_in, col = colors_in, 
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

get.tradecount <- function(Matrix){
  one <- 0
  both <- 0
  for(i in 1:ncol(Matrix)){
    for(j in 1:nrow(Matrix)){
      if(Matrix[i,j] >= 1 & Matrix[j,i] >= 1) both <- both+1
      else if(Matrix[i,j] >= 1 & Matrix[j,i] == 0 | Matrix[i,j] == 0 & Matrix[j,i] >= 1) one <- one+1
      }
    }
  return(list(one = one/2, both = both/2))
}

symmetric_ts <- sapply(adj_ts, FUN = get.tradecount)
