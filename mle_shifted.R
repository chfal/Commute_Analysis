total_time <- c(60, 60, 48, 42, 50, 54, 56, 57, 47, 55, 53, 51, 49, 57, 57, 66, 48, 56, 48, 66, 58, 57, 48, 53, 54, 50, 59, 55, 58, 45, 49, 56, 57, 57, 48, 51, 45, 55, 51, 63, 62, 59, 52, 58, 60, 53, 55, 54, 45, 71, 55, 67, 61, 55, 48, 72, 55, 62, 53, 72, 60, 59, 76, 60, 62, 67, 56, 55, 47, 57, 46, 69, 53, 59, 49, 69, 64, 50, 51, 63, 61, 57, 48, 51, 50, 53, 55, 57)

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
nLL_gamma <- function(theta) {-sum(dgamma(total_time-theta[3], shape=theta[1], scale=theta[2], log=TRUE))}

## Initial estimates
ybar <- mean(total_time)
s2 <- var(total_time)
c0 <- min(total_time) - 1e-4
beta0 <- s2 / (ybar - c0)
alpha0 <- (ybar - c0)^2 / s2

## Optimization
mlefit <- optim(c(alpha0,beta0,c0),nLL_gamma)
alphahat <- mlefit$par[1]
betahat <- mlefit$par[2]
chat <- mlefit$par[3]


c0 <- min(total_time) - 1e-4
nu <- 1:50
nnu <- length(nu)
mlefit2 <- vector("list",nnu)
nLL2 <- function(theta,nu) {-sum(dchisq(total_time-theta, df=nu, log=TRUE))}

for (i in 1:length(nu)) {
  mlefit2[[i]] <- optimize(nLL2, interval=c(0,c0), nu=nu[i])
}

objective <- sapply(mlefit2, "[[", "objective")


best <- which.min(objective)


plot(nu,objective, type="b",
     xlab="Degrees of freedom", ylab="Negative Log-Likelihood")

hist(total_time)
curve(dchisq(x-c0,df=23), add=T, col="blue")
