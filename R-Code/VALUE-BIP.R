val_expRel <- list()
val_impRel <- list()

######Handelswerte in Relation zu GDP setzen.

for(y in 1:20){
  temp_e <- vector(length = nrow(salw[[y]]))
  temp_i <- vector(length = nrow(salw[[y]]))
  for(c in 1:nrow(salw[[y]])){
    temp_e[c] <- sum(salw[[y]][c,])
    temp_i[c] <- sum(salw[[y]][,c])
  }
  val_expRel[[y]] <- temp_e/GDP[,y]
  val_impRel[[y]] <- temp_i/GDP[,y]
}

sum_val_expRel <- vector(length = nrow(salw[[y]]))
sum_val_impRel <- vector(length = nrow(salw[[y]]))

for(i in 1:20){
  sum_val_expRel <- sum_val_expRel + val_expRel[[i]]
  sum_val_impRel <- sum_val_impRel + val_impRel[[i]]
}


round(sum_val_expRel[order(sum_val_expRel)])
# Top 10

# 1 & China & 114735 \\
# 2 & Brazil & 53225 \\
# 3 & Italy & 48862 \\
# 4 & Spain & 40822 \\
# 5 & Germany & 38039 \\ 
# 6 & Turkey & 36174 \\
# 7 & South Korea & 29131 \\
# 8 & United States & 26539 \\
# 9 & India & 24615 \\
# 10 & Austria & 23149 \\


round(sum_val_impRel[order(sum_val_impRel)])

# Top 5
# 1 &Tanzania &54562 \\
# 2 &Thailand &49636\\
# 3 &India &32416\\
# 4 &Pakistan &30290\\
# 5 &South Korea &27208\\
# 6 &China &25402\\
# 7 &Indonesia &24268\\
# 8 &Kenya &22907\\
# 9 &Malaysia &22330\\
# 10 &Bukina Faso &22183\\

names_exp <- c("China", "Brazil", "Italy", "Spain", "Germany")
names_imp <- c("Tanzania", "Thailand", "India", "Pakistan", "South Korea")

imp5 <- matrix(nrow = 20, ncol = 5)
colnames(imp5) <- names_imp
rownames(imp5) <- 1992:2011

for(y in 1:20){
  imp5[y,] <- val_impRel[[y]][names_imp]
}


exp5 <- matrix(nrow = 20, ncol = 5)
colnames(exp5) <- names_exp
rownames(exp5) <- 1992:2011

for(y in 1:20){
  exp5[y,] <- val_expRel[[y]][names_exp]
}




#######plot


windows(width = 9, height = 6)

par(mfrow = c(1,1), mar = c(2,5,3,1))

matplot(imp5,
        type = "l",
        #ylim = c(1, 1000), 
        col = 1:5,
        lty = 1:5,
        lwd = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylab = "Val/BIP",
        main = "Export-Zeitreihe der 5 Top-Exporteure",
        cex.main = 1.5,
        xaxt = "n"    )        
axis(1, at = 1:20, labels = Year, las = 1, cex.axis = 1.5)    
grid(lwd = 1)
legend("top", names_imp, col = 1:5, lty = 1:5, lwd = 3, bg = "white", cex = 1.4)