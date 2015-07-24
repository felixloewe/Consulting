load("Z:/myErgmWs3.RData")


summary(my.ergm.fit3)
mcmc.diagnostics(my.ergm.fit3)
gof3 <- gof(my.ergm.fit3, GOF = ~distance + espartners + dspartners + idegree + odegree, verbose = T)


par(mfrow = c(3,2), cex = 1.2)
plot(gof3)
