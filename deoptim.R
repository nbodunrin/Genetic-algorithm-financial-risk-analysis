
install.packages("stockPortfolio")
install.packages("DEoptim")

library(stockPortfolio)
library(DEoptim)
 tickers <- c("WDC" ,"WY" , "WHR" , "WMB" , "WEC" , "XEL" , "XRX" , "XLNX" ,"ZION" ,"MMM" ,
               "ABT", "ADBE" , "AMD" , "AET" , "AFL" , "APD" , "ARG" ,"AA" , "AGN" ,
               "ALTR" , "MO" , "AEP" , "AXP" , "AIG" , "AMGN" , "APC" ,"ADI" , "AON" ,
               "APA", "AAPL" , "AMAT" ,"ADM" , "T" , "ADSK","M","FTR","SO","FE","SPY", 
              "EFA")
 

AssetsData<-getReturns(tickers, start="2006-1-01", end="2016-3-30", freq="monthly")
dataset <- data.frame(stockname = c(tickers),  Stockaverage = c(v))
Assetreturns<-as.numeric(summary(AssetsData))
v=Assetreturns



obj <- function(x) { 
    neww <- x/sum(x)
    portfoliomean <- dataset$Stockaverage %*% neww
    covmatrix <- cov(AssetsData$R)
    stdev<- sqrt(sum(neww * colSums((covmatrix * neww))))
    sharpe <- (portfoliomean / stdev)
    return(-sharpe)
  }
    
 #If we want to be able to reproduce our works
 #later it would be nice if we would get the
 #same numbers
 #if we want the same random numbers
 #we set the seed to have the same result
set.seed(13)

out <- DEoptim(fn = obj,
                 lower = rep(0, 40),
                 upper = rep(1, 40),DEoptim.control(NP = 400, itermax = 200, trace = TRUE,CR= 0.9,F=0.8))

out$optim$bestval
out$optim$bestmem
sum(out$optim$bestmem)
DEbest=out$optim$bestmem
DEnormbest<- DEbest/sum(DEbest)
sum(DEnormbest)
Doptimalmean <- dataset$Stockaverage%*%DEnormbest
covmatrix <- cov(AssetsData$R)
Doptimaldev<-sqrt(sum(DEnormbest*colSums((covmatrix*DEnormbest))))
Doptimalsharpe <- Doptimalmean / Doptimaldev
#Doptimalweight <- data.frame(stockname = c(tickers),  Stockaverage = c(v), normalised =c(normbest))
DEoptimalportfolio <- data.frame(DEoptimalmean=c(Doptimalmean), DEoptimaldev =c(Doptimaldev), DEoptimalsharpe=c(Doptimalsharpe))
DEoptimalportfolio


plot(x=NULL,y=NULL, xlim=range(0.01080,0.0115),
     ylim=range(0.00184,0.00185), type = "n",xlab = "Risk", ylab = "Return")
points(optimaldev, optimalmean,pch = 25 , bg = "red")
