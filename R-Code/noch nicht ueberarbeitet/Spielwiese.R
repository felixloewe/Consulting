#############Zusammenhang zwischen loops und IsMirror?
MADdata$loop <- 0
MADdata[as.character(Reporter_Name)==as.character(Partner_Name),]$loop <- 1
MADdata[MADdata$loop==1,]
table(MADdata$loop, MADdata$IsMirror, dnn = c("loop", "IsMirror"))
table(MADdata$IsMirror)
