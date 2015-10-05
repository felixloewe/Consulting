loopID <- which(MADdata$Reporter_Code == MADdata$Partner_Code)
Loops <- MADdata[loopID,c("Reporter_Name", "Partner_Name", "Year", "Value", "PRIO_Weapons_Code" )]
install.packages("xtable")
library(xtable)

xtable(Loops)
