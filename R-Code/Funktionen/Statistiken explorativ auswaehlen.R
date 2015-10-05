Aj <- as.matrix(A)
Aj[which(Aj >= 2)] <- 1

Y <- vector()

get.stat <- function(stat, Adj){
  k <- 1
  out <- vector()
  for (i in 1:ncol(Adj)){
    for(j in c(1:nrow(Adj))[-i]){
      Aj[i,j] <- 0
      null <- Aj[i,j]
      Aj[i,j] <- 1
      one <- Aj[i,j]
      out[k] <- stat(one) - stat(null) 
      k <- k+1
    }
  }
out
}

get.Y <- function(Adj){
  k <- 1
  Y <- vector()
  for (i in 1:ncol(Adj)){
    for(j in c(1:nrow(Adj))[-i]){
      Y[k] <- Adj[i,j]
      k <- k+1
    }
  }
  Y
}

Y1 <- get.Y(Aj)
Y1



####### statisticen zum ausprobieren :
# asymmetric, cycle(k), degrange, dsp, esp, istar, ostar, odegreepopularity, transitive,triangle



#################### braucht ca 107 minuten

get.stat2 <- function(Adj){
  k <- 1
  out <- matrix(ncol = 13, nrow = nrow(Adj)^2-nrow(Adj))
  for (i in 1:ncol(Adj)){
    for(j in c(1:nrow(Adj))[-i]){
      Aj[i,j] <- 0
      null <- Aj
      Aj[i,j] <- 1
      one <- Aj
      Graph0 <- network::as.network(null, directed = T)
      Graph1 <- network::as.network(one, directed = T)
      formula0 <- formula(Graph0 ~ asymmetric +
                              cycle(2) +
                              idegrange(from = 1, to = 10) +
                              odegrange(from = 1, to = 10) +
                              dsp(1) +
                              esp(1) +
                              istar(2) + 
                              istar(3) +
                              ostar(2) +
                              ostar(3) +
                              odegreepopularity +
                              transitive +
                              triangle)
      formula1 <- formula(Graph1 ~ asymmetric +
                              cycle(2) +
                              idegrange(from = 1, to = 10) +
                              odegrange(from = 1, to = 10) +
                              dsp(1) +
                              esp(1) +
                              istar(2) + 
                              istar(3) +
                              ostar(2) +
                              ostar(3) +
                              odegreepopularity +
                              transitive +
                              triangle)
      change <- summary.statistics(formula1) - summary.statistics(formula0)
      
      out[k,] <- change
      colnames(out) <-  names(summary.statistics(formula1))
      k <- k+1
      message(cat("Zeile", k))
    }
    
  }
  out
}

test1 <- get.stat2(Aj)
test1


