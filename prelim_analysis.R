library(tidyverse)
setwd("C:/Users/chfal/OneDrive - University of Massachusetts Boston/COMPSTAT/Final_Project/")
commute_3 <-read_csv("commute3.csv")


commute_mean <- vector("numeric",ncol(commute_4))

for(i in ncol(commute_4)){
  commute_mean[i] <- mean(commute_4[[i]], na.rm=T)
}
commute_mean

commute_variance <- vector("double",ncol(commute_4))
for(i in names(commute_4)){
  commute_variance[i] <- sd(commute_4[[i]], na.rm=T)
}
commute_variance


for(i in ncol(commute_4)){
  commute_shapiro[i] <- shapiro.test(commute_4[[i]])
}

commute_shapiro <- shapiro.test()

apply(commute_4, 2, shapiro.test())

for(i in ncol(commute_4)){}
# 2) If not normal, what probability distributions can I use to model each segment of my commute? Using the MLE and MoM, I hope to find probability distributions for each segment of the commute. I doubt they are all normally distributed, however; they might likely all be very right-skewed because although there is theoretically a minimum time needed to commute to school, due to delays, the commute could be hours long. Therefore, these longer commute times will pull out the durations, making the distributions abnormal. I would like to find the parameters for the probability distributions.


# 3) Is the difference between the length of the commute depending on the directionality (i.e., school to home versus home to school)? During my exploratory data analysis, I noticed that box plots created of distances had very different spreads. I would hope to test this with an F-test for variances between two populations.

shapiro.test(commute_2$total_time) # not normal
shapiro.test(commute_2$red_line) # not normal
shapiro.test(commute_2$mbta_total) # not normal
shapiro.test(commute_2$wait_green) # not normal

# 4) What segments of the commute have the largest contribution to the overall length? This would deal with the method of partial correlation to see which parts of the commute have the largest proportional increase to the overall length of the journey I have.


# 5) How long can I expect my commute to be? Using the data, I would like to create confidence intervals for each segment.