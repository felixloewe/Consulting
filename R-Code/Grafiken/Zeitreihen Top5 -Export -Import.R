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
summil_exp_perland <- aggregate(MADdata$Value, list(Land = MADdata$Reporter_Name, code = MADdata$Reporter_Code), sum)
summil_imp_perland <- aggregate(MADdata$Value, list(Land = MADdata$Partner_Name, code = MADdata$Partner_Code), sum)

top5exp_names <- as.character(summil_exp_perland[order(-summil_exp_perland$x),"Land"])[1:5]
top5imp_names <- as.character(summil_imp_perland[order(-summil_imp_perland$x),"Land"])[1:5]


# Zeitreihen der Handelsvolumen
valmil_import_ts <- aggregate(MADdata$ValueMil, list(Land = MADdata$Partner_Name, Jahr = MADdata$Year, code = MADdata$Partner_Code), sum)
valmil_export_ts <- aggregate(MADdata$ValueMil, list(Land = MADdata$Reporter_Name, Jahr = MADdata$Year, code = MADdata$Reporter_Code), sum)

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


Year <- 1992:2011
# Plot Aufruf



windows(width = 9, height = 6)

par(mfrow = c(1,1), mar = c(2,5,3,1))

matplot(export_ts5,
        type = "l",
        ylim = c(1, 1000), 
        col = 1:5,
        lty = 1:5,
        lwd = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylab = "Mil. Dollar",
        main = "Export-Zeitreihe der 5 Top-Exporteure",
        cex.main = 1.5,
        xaxt = "n"    )        
axis(1, at = 1:20, labels = Year, las = 1, cex.axis = 1.5)    
grid(lwd = 1)
legend("top", top5exp_names, col = 1:5, lty = 1:5, lwd = 3, bg = "white", cex = 1.4)

savePlot("Bericht/Grafiken/ts_topsexp", type = "png")

windows(width = 9, height = 12)

par(mfrow = c(2,1), mar = c(2,5,3,1))
matplot(import_ts5,
        type = "l",
        ylim = c(1, 2000), 
        col = c(1,3,6,7,8),
        lty = c(1,3,6,7,8),
        lwd = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylab = "Mil. Dollar",
        main = "Import-Zeitreihe der 5 Top-Importeure",
        cex.main = 1.5,
        xaxt = "n"    )
axis(1, at = 1:20, labels = Year, las = 1, cex.axis = 1.5)    
grid(lwd = 1)
legend("topleft", top5imp_names, col = c(1,3,6,7,8), lty = c(1,3,6,7,8), lwd = 3, bg = "white", cex = 1.4)


matplot(import_ts5[,-1],
        type = "l",
        ylim = c(1, 300),
        col = c(3,6,7,8),
        lty = c(3,6,7,8),
        lwd = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        cex.main = 1.5,
        ylab = "Mil. Dollar",
        main = "Import-Zeitreihe der 5 Top-Importeure ohne USA",
        
        xaxt = "n" )
axis(1, at = 1:20, labels = Year, las = 1, cex.axis = 1.5)    
grid(lwd = 1)

savePlot("Bericht/Grafiken/ts_topsexp", type = "png")


Top5ImportGDP <- GDP[c("United States", "Germany", "France", "Canada", "United Kingdom"),as.character(1992:2011)]
Top5ExportGDP <- GDP[c("United States", "Italy", "Germany", "Brazil", "Austria"),as.character(1992:2011)]

exportrel_ts5 <- matrix(nrow = 20, ncol = 5)
importrel_ts5 <- matrix(nrow = 20, ncol = 5)
for(i in 1:5){
  for(j in 1:20){
    exportrel_ts5[j,i] <-  export_ts5[j,i] / Top5ExportGDP[i,j]
    importrel_ts5[j,i] <-  import_ts5[j,i] / Top5ImportGDP[i,j]    
  }
}



# Plot Aufruf



windows(width = 15, height = 12)

par(mfrow = c(2,1))

matplot(exportrel_ts5,
        type = "l",
        #ylim = c(1, 1000), 
        col = 1:5,
        lty = 1:5,
        lwd = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylab = "Mil. Dollar",
        main = "Export-Zeitreihe der 5 Top-Importeure",
        cex.main = 1.5,
        xaxt = "n"    )        
axis(1, at = 1:20, labels = Year, las = 2, cex.axis = 1.5)    
grid(lwd = 1)
legend("top", top5exp_names, col = 1:5, lty = 1:5, lwd = 3, bg = "white", cex = 1.4)

matplot(importrel_ts5,
        type = "l",
        #ylim = c(1, 2000), 
        col = c(1,3,6,7,8),
        lty = c(1,3,6,7,8),
        lwd = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylab = "Mil. Dollar",
        main = "Import-Zeitreihe der 5 Top-Importeure",
        cex.main = 1.5,
        xaxt = "n"    )
axis(1, at = 1:20, labels = Year, las = 2, cex.axis = 1.5)    
grid(lwd = 1)
legend("top", top5imp_names, col = c(1,3,6,7,8), lty = c(1,3,6,7,8), lwd = 3, bg = "white", cex = 1.4)


savePlot("Bericht/Grafiken/ts_tops", type = "png")
