# Funktionen zur Erstellung der Top-Export- bzw. Top-Import-Tabelle nach Waffenwert
# für ein ausgewähltes Jahr 

# Ausgabe: -Top-Export(Year)
#          -Top-Import(Year)

Top_Export <- function(Year){
  top_export <- aggregate(MADdata[MADdata$Year == Year,]$Value,
                          list(Land = MADdata[MADdata$Year == Year,]$Reporter_Name),
                          sum)
  top_export <- top_export[order(-top_export$x),]
  top_export
}


Top_Import <- function(Year){
  top_import <- aggregate(MADdata[MADdata$Year == Year,]$Value,
                          list(Land = MADdata[MADdata$Year == Year,]$Partner_Name),
                          sum)
  top_import <- top_import[order(-top_import$x),]
  top_import
}
