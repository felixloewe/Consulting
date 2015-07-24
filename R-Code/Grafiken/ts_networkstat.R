## Zeitreihen deskriptiver Netzwerkstatistiken erstellen und plotten

## Ausgabe:
# vert_ts (Knotenanzahl = Anzahl Länder)
# edg_ts (Kantenanzahl = Anzahl Waffenlieferungen)
# trans_ts (Transitivität = Liefere ich Waffen an die Exportländer meiner Exportländer?)
# dens_ts (Dichte = Dichte des Netzwerkes. Gemessen an der Anzahl vorhandener Kanten geteilt durch die Anzahl möglicher Kanten.)
# diameter_ts (Durchmesser = Länge der längsten Exportkette)
# valmil_ts (Waffenwert = Gesamtwert gelieferter Waffen in Mil. US-Dollar)

vert_ts <- sapply(GraphYear, vcount)
edg_ts <- sapply(GraphYear, ecount)
trans_ts <- sapply(GraphYear, transitivity)
dens_ts <- sapply(GraphYear, function(Graph) graph.density(simplify(Graph), loops = F)) # aktualisierte Dichte
diameter_ts <- sapply(GraphYear, diameter)
valmil_ts <- sapply(GraphYear, function(Graph) sum(E(Graph)$ValueMil))

# To-Do: Zeitreihen-Plot-Funktion
# To-Do: Farben standardisieren
# To-Do: Grafiken gleich interpretieren (am besten LaTeX)

# To-Do: inhaltlicher Vergleich mit Großwaffen-Handel

windows(height = 8, width = 12)

par(mfrow = c(2,2), oma = c(0,0,0,0), cex = 1.2)
plot(ts(valmil_ts, start = 1992),
     lwd = 2,
     type = "o",
     main = "Handelswerte in Mio. USD",
     ylab = "",
     xlab = "",
     col = "skyblue1"
     )
grid(lwd = 1)

plot(ts(vert_ts, start = 1992),
     lwd = 2,
     type = "o",
     main = "Anzahl Knoten",
     ylab = "",
     xlab = "",
     col = "skyblue1"
     )
grid(lwd = 1)

plot(ts(edg_ts, start = 1992),
     lwd = 2,
     type = "o",
     main = "Anzahl Kanten",
     ylab = "",
     xlab = "",
     col = "skyblue1"
     )
grid(lwd = 1)

#plot(ts(trans_ts,start = 1992), type = "o", main = "Zeitreihe der Transitivität", ylab = "", xlab = "")
#grid(lwd = 1)

plot(ts(dens_ts, start = 1992),
     lwd = 2,
     type = "o",
     main = "Dichte",
     ylab = "",
     xlab = "",
     col = "skyblue1"
     )
grid(lwd = 1)

#plot(ts(diameter_ts, start = 1992), type = "o")
#grid(lwd = 1)

savePlot("Bericht/Grafiken/ts_descriptives", type = "png")
