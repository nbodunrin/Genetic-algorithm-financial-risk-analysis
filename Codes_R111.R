install.packages("quantmod")
install.packages("genalg")
install.packages("rgp")

library("quantmod")
library("genalg")
library("rgp")

GSG<-getSymbols("GSG",src="yahoo", from = as.Date("201-01-01"),auto.assign = FALSE)
chartSeries(GSG, subset="last 3 months",line.type = "l", 
            TA=c(addBBands()), theme = chartTheme("white", up.col='Grey',dn.col="Black"), major.ticks="months") 

addEMA(n = 10, wilder = FALSE, ratio=NULL, on = 1,with.col = Cl, overlay = TRUE, col = "black")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "red")
addRSI(maType = "EMA",wilder = TRUE)

dim(GSG)
op <- coredata(GSG$GSG.Open)[,1]
cp <- coredata(GSG$GSG.Close)[,1]
hp <- coredata(GSG$GSG.High)[,1]
lp <- coredata(GSG$GSG.Low)[,1]
adjp<-coredata(GSG$GSG.Adjusted)[,1]
rsi1<-as.numeric(unlist(RSI(cp,n=14,MaType="SMA")))
ema1<-as.numeric(unlist(EMA(cp,n=10,wilder=FALSE,ratio=NULL)))

OP<-op[1:200]
HP<-hp[1:200]
LP<-lp[1:200]
CP<-cp[1:200]
ADJP<-adjp[1:200]
rsi14<-rsi1[1:200]
ema10<-ema1[1:200]


trainingdatahp <- hp[1:190]
testdatahp <- hp[191:200]

trainingdatahp1 <- hp[1:200]



trainingdatalp <- lp[1:190]
testdatalp <- lp[191:200]

trainingdatalp1 <- lp[1:200]

######We use GA to find the best indicators######
evalFunc <- function(x) {
  bind <- as.vector(t(cbind(OP,CP,LP,ADJP,rsi14, ema10))*x)
  binding = bind[bind > 0]
  inputVariableSet <- inputVariableSet("x1", "x2", "x3", "x4")
  functionSet1 <- functionSet("+", "*", "-", "/", "sin", "cos","exp", "^", "sqrt")
  constantFactorySet1 <- constantFactorySet(function() rnorm(1))
 
  fitnessFn <- function(f){
    fn.result = NULL
    for(i in 1:46) {fn.result[i] = do.call(f, as.list(binding[i:(i+3)]))}
    fitness = rmse(fn.result, hp[5:50])
    return (if (is.na(fitness)) Inf else fitness)}
 
  gpResult <- geneticProgramming(functionSet=functionSet1,
                                    inputVariables=inputVariableSet,
                                    constantSet=constantFactorySet1,
                                    fitnessFunction=fitnessFn,
                                    stopCondition=makeTimeStopCondition(30))
  best <- gpResult$population[[which.min(gpResult$fitnessValues)]]
  
  predictions <- trainingdatahp[47:50]
  
  for(i in 1:7)
  predictions[i+4] <- do.call(best, as.list(predictions[i:(i+3)]))
  dev=rmse(testdatahp[1:7], predictions[5:11])
  return(dev)
}


######Run GA Model######
GAmodel <- rbga.bin(size = 6, popSize = 5, iters = 2, mutationChance = 0.01, evalFunc=evalFunc, verbose=TRUE)

getBest <- function(model){
  minEval=min(model$evaluations)
  index= model$evaluations == minEval
  bestCount = sum(rep(1, model$popSize)[index]);
  if (bestCount > 1)
    return(model$population[index, ][1,])
  else
    return(model$population[index, ])
}

best2<-getBest(GAmodel)
best2
dataset<-data.frame(parameters=c("LP","CP","LP","ADJP","rsi14", "ema10"), gene=best2)
dataset[best2 == 1, ]




######TEST#####
inputVariableSet <- inputVariableSet("x1", "x2", "x3", "x4")
functionSet <- functionSet("+", "*", "-", "/", "sin", "cos","exp","^","sqrt")
constantFactorySet1 <- constantFactorySet(function() rnorm(1))

ln <- as.vector(t(cbind(CP)))

fitnessFn1 <- function(f){
  fn.result = NULL
  for(i in 1:46) {fn.result[i] = do.call(f, as.list(ln[i:(i+3)]))}
  fitness = mse(fn.result, hp[5:50])
  return (if (is.na(fitness)) Inf else fitness)}

gpResult1 <- geneticProgramming(functionSet=functionSet,
                               inputVariables=inputVariableSet,
                               constantSet=constantFactorySet1,
                               fitnessFunction=fitnessFn1,
                               stopCondition=makeTimeStopCondition(30))

best1 <- gpResult1$population[[which.min(gpResult1$fitnessValues)]]
best1

trainingResults<-NULL
for(i in 1:200)
  trainingResults[i]<-do.call(best1,as.list(trainingdatahp1[i:(i+3)]))

trainingResults

trainingResults - trainingdatahp1[5:199]

predictions<-trainingdatahp1[197:200]
for(i in 1)
  predictions[i+4]<-do.call(best1,as.list(predictions[i:(i+3)]))

predictions[5]
#testdatahp1[1]

#rmse(testdatahp1[1], predictions[5])
GSG.df = data.frame(date=time(GSG), coredata(GSG))
y<-as.ts(Hi(GSG))[1:201]
y
x<-1:201
plot(x[1:200],y[1:200],ylab="LP",xlab="x", ylim=c(30,60), type="l", col = "black", lwd = 2)
lines(x[5:200],trainingResults[5:200], col = "blue", lwd = 2)
points(x[201],predictions[5] , pch = 19 , col = "red")




x[5:200],
x<-1:300  
plot(xlim=c(0,300), ylim=c(0,200),type="l", mydata,  col = "black", lwd = 2)
lines(x[8:290], trainingResults[8:290], col = "blue", lwd = 2)
lines(x[291:300], predictions[8:17], col = "green", lwd = 2)
