set.seed(20150815)

lambda <- 1/5
n <- 40
nbSim <- 1000

expMatrix <- matrix(rexp(n*nbSim, lambda), nbSim)
expMeans <- apply(expMatrix, 1, mean)
expVars <- apply(expMatrix, 1, var)
rnormVal <- rnorm(1000, 1/lambda, (1/lambda^2)/n)

expMeansFrm <- data.frame(meanSim=c(expMeans, rnormVal), factor = factor(rep(1 : 2, rep(length(nbSim), 2))))

g <- ggplot(expMeansFrm, aes(x=meanSim, color=factor))
g <- g + geom_histogram(aes(y=..density..), alpha = .30, binwidth=0.2, colour = "blue", linetype="blank")
g <- g + geom_density(size = .5, alpha = .2, colour="blue")
g <- g + geom_vline(xintercept = 1/lambda, size = .5, color = "red")
g <- g + geom_vline(xintercept = 0, size = .5, color = "black")
g <- g + geom_hline(yintercept = 0, size = .5, color = "black")
g <- g + ggtitle("Figure 1 - Sample mean distribution of an exponential distribution")
g

expVarsFrm <- data.frame(varSim=expVars)

g <- ggplot(expVarsFrm, aes(x=varSim))
g <- g + geom_histogram(aes(y=..density..), alpha = .30, binwidth=2, colour = "blue", linetype="blank")
g <- g + geom_density(size = .5, alpha = .2, colour="blue")
g <- g + geom_vline(xintercept = 1/lambda^2, size = .5, color = "red")
g <- g + geom_vline(xintercept = 0, size = .5, color = "black")
g <- g + geom_hline(yintercept = 0, size = .5, color = "black")
g <- g + ggtitle("Figure 2 - Sample variance distribution of an exponential distribution")
g

smplMean <- mean(expMeans)
smplVar <- mean(expVars)

## Testing the variance formula on one row
varNum <- 0
for(i in 1:40)
  varNum <- varNum + (expMatrix[1,i] - mean(expMatrix[1,]))^2
varNum / (40 - 1)
var(expMatrix[1,])

meanTh <- 1/lambda
sDevTh <- 1/lambda
varTh <- 1/lambda^2
cfunc <- function(x) (mean(x) - 1/lambda) * sqrt(n) * lambda
expMatrixNormFrm <- data.frame(
  meanSimNorm = apply(expMatrix, 1, cfunc)
)

g <- ggplot(expMatrixNormFrm, aes(x=meanSimNorm))
g <- g + geom_histogram(aes(y=..density..), alpha = .30, binwidth=0.2, colour = "blue", linetype="blank")
g <- g + geom_density(size = .5, alpha = .2, colour="blue")
g <- g + stat_function(fun = dnorm, size = 1)
g <- g + geom_vline(xintercept = 0, size = .5, color = "black")
g <- g + geom_hline(yintercept = 0, size = .5, color = "black")
g <- g + ggtitle("Figure 1 - Sample mean distribution of an exponential distribution")
g

hist(rnorm(10000, 1/lambda, (1/lambda^2)/n))

var(expMeans)

