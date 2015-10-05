# Kontinente / Regionen animieren

# Kontinente
for(i in 1:length(Year)){
  
  # Dateiname
  file_name <- paste("Bericht/Grafiken/Cont_Ani/cont", i, ".pdf", sep = "")
  
  # PDF-Window
  pdf(file_name, width = 6, height = 7)
  
  Continent.plot(i+1991, groups = "Continent")
  dev.off()
}

# Regionen
for(i in 1:length(Year)){
  
  # Dateiname
  file_name <- paste("Bericht/Grafiken/Reg_Ani/reg", i, ".pdf", sep = "")
  
  # PDF-Window
  pdf(file_name, width = 6, height = 7)
  
  Continent.plot(i+1991, groups = "Region")
  dev.off()
}