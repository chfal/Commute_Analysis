library(tidyverse)
library(knitr)
library(patchwork)
library(stats4)
library(EnvStats)
library(corrplot)
#read data, clean it
commute <-read_csv("commute3.csv")
positions <- c(15:22)
commute_2 <- commute %>%
  select("destination", positions)
commute_3 <- commute %>%
  select(positions) %>%
  gather(key = "segment", value= "time")


#error: legend will not appear with color codes, how to add?
#other error: which probability distribution to use?
total_time <- as.vector(commute_2$total_time)
egamma(total_time)
enorm(total_time)
eweibull(total_time)
hist(total_time, main="Total Time", breaks="FD", freq=FALSE, ylim =c(0,.07))
curve(dnorm(x, mean=55.931, sd=6.906261), from=40, to=80, add=TRUE, col="red",lwd=2)
curve(dgamma(x, shape=68.2686038, scale=.8192905), from=40, to=80, add=TRUE, col="blue", lwd=2)
curve(dweibull(x, shape=8.043449, scale=59.057884), from=40, to=80, add=TRUE, col="green", lwd=2)
legend(x="topright",legend=c("gamma","normal", "weibull"))

#confidence intervals: not sure what is the best way to make because they are not normally distributed.

