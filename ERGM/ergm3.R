load("Z:/myErgmWs3.RData")

install.packages("latticeExtra")
library(latticeExtra)
summary(my.ergm.fit3)
mcmc.diagnostics(my.ergm.fit3, vars.per.page = 2)
gof3 <- gof(my.ergm.fit3, GOF = ~distance + espartners + dspartners + idegree + odegree, verbose = T)

windows(width = 13, height = 10)

par(mfrow = c(1,1), cex = 1)
plot(gof3, cex.axis = 1, main = "")
