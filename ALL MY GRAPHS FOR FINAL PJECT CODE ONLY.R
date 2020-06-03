hist(total_time, main="Total Time", breaks="FD", freq=FALSE, ylim =c(0,.07), xlim=c(0,80), xlab="time (minutes)")
curve(dnorm(x, mean=55.931, sd=6.906261), from=0, to=80, add=TRUE, col="red",lwd=2)
curve(dgamma(x, shape=68.2686038, scale=.8192905), from=0, to=80, add=TRUE, col="blue", lwd=2)
curve(dweibull(x, shape=8.043449, scale=59.057884), from=0, to=80, add=TRUE, col="green", lwd=2)
legend(x="topleft",legend=c("gamma","normal", "weibull"), lty=1, col=c("blue", "red","green"))



hist(total_time, freq=FALSE, xlim=c(0,80), ylim=c(0,0.08), main="Total Time on Shifted/NonShifted Gamma Distribution", xlab="time (minutes)")
curve(dgamma(x-chat,shape=alphahat,scale=betahat), add=TRUE,
      col="blue", lwd=2, n=401)
curve(dgamma(x,shape=alpha,scale=beta), add=TRUE, col="red", lwd=1.5, n=401)
legend("topleft", col=c("blue","red"), lty=1, lwd=2, cex=0.75,
       c("Shifted gamma (MLE)","Non-shifted gamma (MLE)"))


hist(total_time, freq=FALSE, xlim=c(0,80), ylim=c(0,0.08), main="Total Time on Shifted/NonShifted Gamma Distribution", xlab="time (minutes)")
curve(dgamma(x-chat,shape=alphahat,scale=betahat), add=TRUE,
      col="blue", lwd=2, n=401)
curve(dgamma(x,shape=alpha,scale=beta), add=TRUE, col="red", lwd=1.5, n=401)
legend("topleft", col=c("blue","red"), lty=1, lwd=2, cex=0.75,
       c("Shifted gamma (MLE)","Non-shifted gamma (MLE)"))


plot(nu,objective, type="b", col="purple", xlab="Degrees of freedom", ylab="Negative Log-Likelihood", main="Degrees of Freedom versus Log-Likelihood Function Value")

hist(total_time, xlim=c(0,80), ylim=c(0,.08), freq=FALSE, main="Total time on Chi-Square Distribution", xlab="Time (minutes)")
curve(dchisq(x-32.91041,df=23), from=0, to=80, add=T, col="turquoise", lwd=2)
legend("topleft", col="turquoise", lty=1, lwd=2, cex=0.75,("chi-square"))


hist(total_time, freq=FALSE, xlim=c(0,80), ylim=c(0,.08), main="Shifted Gamma vs. Shifted Chi-Square", xlab="time (minutes)")
curve(dchisq(x-32.91041,df=23), add=T, col="turquoise", lwd=2)
curve(dgamma(x-chat,shape=alphahat,scale=betahat), add=TRUE,
      col="blue", lwd=2)
legend("topleft", col=c("turquoise","blue"),lty=1, lwd=2,cex=.75, c("shifted chi-square","shifted gamma"))


plotlm <- lm(total_time ~ green_line, commute_2)
plot(commute_2$green_line,commute_2$total_time, main = "Effect of Green Line on Total Time", xlab="green line time (minutes)", ylab="total time (minutes)")
abline(plotlm, col="red")
plot(plotlm, which=1:2)
summary(plotlm)


plotlm2 <- lm(total_time~wait_green, commute_2)
plot(commute_2$wait_green,commute_2$total_time, main = "Effect of Waiting for the Green Line on Total Time", xlab="waiting for green  time (minutes)", ylab="total time (minutes)")
abline(plotlm2, col="blue")
plot(plotlm2, which=1:2)
summary(plotlm2)
