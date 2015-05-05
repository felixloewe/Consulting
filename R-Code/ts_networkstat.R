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

par(mfrow = c(3,2))
plot.ts(valmil_ts, ylim = c(0, 6000))




