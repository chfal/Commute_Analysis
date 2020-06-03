## Simulate shifted gamma distribution
alpha <- 10 # shape parameter
beta <- 3  # scale parameter 
c <- 35    # shift parameter
n <- 100   # sample size
set.seed(2020) # for reproducibility

################# 
# Shifted gamma
################# 

## Negative log-likelihood function
## (theta[1] = alpha, theta[2] = beta, theta[3] = c)
nLL <- function(theta) {-sum(dgamma(total_time-theta[3], shape=theta[1], 
                                    scale=theta[2], log=TRUE))}

## Initial estimates
ybar <- mean(total_time)
s2 <- var(total_time)
c0 <- min(total_time) - 1e-4
beta0 <- s2 / (ybar - c0)
alpha0 <- (ybar - c0)^2 / s2

## Optimization
mlefit <- optim(c(alpha0,beta0,c0),nLL)
alphahat <- mlefit$par[1]
betahat <- mlefit$par[2]
chat <- mlefit$par[3]

# ## Find MLE for standard (non-shifted) gamma distribution
# # install.packages("fitdistrplus")
library(fitdistrplus)
mlefit2 <- fitdist(total_time,"gamma")
alphahat2 <- coef(mlefit2)[1]
betahat2 <- coef(mlefit2)[2]

# ## Compare results
hist(total_time, freq=FALSE, xlim=c(0,80), ylim=c(0,0.08))
curve(dgamma(x-c,shape=alpha,scale=beta), add=TRUE, lwd=1.5, n=401)
curve(dgamma(x-chat,shape=alphahat,scale=betahat), add=TRUE,
      col=2, lwd=1.5, n=401)
curve(dgamma(x,shape=alpha,scale=beta), add=TRUE, col=3, lwd=1.5, n=401)
legend("topright", col=1:3, lty=1, lwd=1.5, cex=0.75,
       c("Shifted gamma (truth)","Shifted gamma (MLE)","Gamma (MLE)"))


##################### 
# Shifted chi square
#####################

## theta[1] = df, theta[2] = shift
c0 <- min(total_time) - 1e-4
nu <- 1:12
nnu <- length(nu)
mlefit2 <- vector("list",nnu)
nLL2 <- function(theta,nu) {-sum(dchisq(y-theta, df=nu))}

for (i in 1:length(nu)) {
  mlefit2[[i]] <- optimize(nLL2, interval=c(0,c0), nu=nu[i])
}

objective <- sapply(mlefit2, "[[", "objective")

plot(nu,objective, type="b",
     xlab="Degrees of freedom", ylab="Negative Log-Likelihood")

best <- which.min(objective)
cbest <- mlefit2[[best]]$minimum

pchisq(best/chat, df=1, lower.tail=FALSE)


squares <- function(x){
  x^2}
