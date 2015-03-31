# Handelsvolumen-Zeitreihen für die Top-Importeure und -Exporteure plotten

# Ausgabe:
# - summil_exp_perland : Eportvolumen pro Land über alle Jahre in Millionen
# - summil_imp_perland : Importvolumen pro Land über alle Jahre in Millionen
# - top5exp_names: Vektor mit Namen der 5 Top Exporteure nach Handelswert über alle Jahre summiert
# - top5imp_names: Vektor mit Namen der 5 Top Importeure nach Handelswert über alle Jahre summiert
# - valmil_import_ts: Matrix mit Export Zeitreihen aller Länder
# - valmil_export_t: Matrix mit Import Zeitreihen aller Länder
# - export_ts5: Matrix mit Export Zeitreihen Top 5 Länder
# - import_ts5: Matrix mit Import Zeitreihen Top 5 Länder

# - Grafik mit 3 Zeitreihen (Top-Exporteure, Top-Importeure, Top-Importeure ohne USA)

# Vektor mit Namen der 5 TOp Importeure/Exporteure nach Handelswert erstellen
summil_exp_perland <- aggregate(MADdata$Value, list(Land = MADdata$Reporter_Name), sum)
summil_imp_perland <- aggregate(MADdata$Value, list(Land = MADdata$Partner_Name), sum)

top5exp_names <- as.character(summil_exp_perland[order(-summil_exp_perland$x),"Land"])[1:5]
top5imp_names <- as.character(summil_imp_perland[order(-summil_imp_perland$x),"Land"])[1:5]


# Zeitreihen der Handelsvolumen
valmil_import_ts <- aggregate(MADdata$ValueMil, list(Land = MADdata$Partner_Name, Jahr = MADdata$Year), sum)
valmil_export_ts <- aggregate(MADdata$ValueMil, list(Land = MADdata$Reporter_Name, Jahr = MADdata$Year), sum)

# Matrix mit Top-Zeitreihen
export_ts5 <- matrix(nrow = 20, ncol = 5)
for (i in 1:5) {
  land <- top5exp_names[i]
  export_ts5[,i] <- valmil_export_ts[valmil_export_ts == land,]$x
}

import_ts5 <- matrix(nrow = 20, ncol = 5)
for (i in 1:5) {
  land <- top5imp_names[i]
  import_ts5[,i] <- valmil_import_ts[valmil_import_ts == land,]$x
}


# Plot Aufruf



windows(width = 10, height = 12)

par(mfrow = c(3,1))
matplot(export_ts5,
    type = "l",
     ylim = c(1, 1000), 
     col = 1:5,
     lty = 1:5,
     lwd = 2,
     xlab = "Year",
     ylab = "Mil. Dollar",
     main = "Export-Zeitreihe der 5 Top-Exporteure",
    xaxt = "n"    )
axis(1, at = 1:20, labels = Year)    
grid(lwd = 1)
legend("top", top5exp_names, col = 1:5, lty = 1:5, lwd = 2, bg = "white")

matplot(import_ts5,
        type = "l",
        ylim = c(1, 2000), 
        col = 1:5,
        lty = 1:5,
        lwd = 2,
        xlab = "Year",
        ylab = "Mil. Dollar",
        main = "Import-Zeitreihe der 5 Top-Importeure",
        xaxt = "n"    )
axis(1, at = 1:20, labels = Year)    
grid(lwd = 1)
legend("top", top5imp_names, col = 1:5, lty = 1:5, lwd = 2, bg = "white")


matplot(import_ts5[,-1],
        type = "l",
        ylim = c(1, 300), 
        col = 2:5,
        lty = 2:5,
        lwd = 2,
        xlab = "Year",
        ylab = "Mil. Dollar",
        main = "Import-Zeitreihe der 5 Top-Importeure ohne USA",
        xaxt = "n"    )
axis(1, at = 1:20, labels = Year)    
grid(lwd = 1)
legend("top", top5imp_names[-1], col = 2:5, lty = 2:5, lwd = 2, bg = "white")
