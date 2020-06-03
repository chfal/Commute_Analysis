setwd("C:/Users/chfal/OneDrive - University of Massachusetts Boston/COMPSTAT/Final_Project/")

library(tidyverse)

#read data, clean it
commute <-read_csv("commute3.csv")
commute_2 <- commute %>%
  select("destination", positions)
positions <- c(15:22)
commute_3 <- commute %>%
  select(positions) %>%
  gather(key = "segment", value= "time")

# 1) What is the average length of each section of the commute? This would involve calculating the sample mean and variance for each segment. I would also like to know whether these are normal or not, and present histograms and the results of Shapiro-Wilk tests for each segment.

#mean
mean_time <- commute_3 %>%
  group_by(segment) %>%
  summarize(mean= mean(time, na.rm=T))

mean_time



#standard deviation
stdev_time <- commute_3 %>% 
  group_by(segment)%>%
  summarize(stdev_time = sd(time, na.rm=T))

stdev_time

#shapiro test: absolutely NONE of my variables are normal, yay!
shapiro_result <- lapply(commute_2[,2:9], shapiro.test)

p_values_shapiro <- c(1.455e-07,.0006423,2.2e-12,.0009328,.03963,1.08e-10,5.959e-12,9.928e-06)

segment <- c("green_line","mbta_total","red_line","time_wo_walk","total_time","wait_green","wait_red","walk_or_bike")

shapiro_data_frame <- data.frame(segment, p_value)


table_1 <- full_join(stdev_time, mean_time, by="segment")
table_1 <- full_join (table_1, shapiro_data_frame, by="segment")

#plot
commute_3 %>%
  group_by(segment) %>%
  ggplot(aes(time, fill=segment)) +
  geom_histogram() +
  facet_wrap(facets="segment")

# 2) If not normal, what probability distributions can I use to model each segment of my commute? Using the MLE and MoM, I hope to find probability distributions for each segment of the commute. I doubt they are all normally distributed, however; they might likely all be very right-skewed because although there is theoretically a minimum time needed to commute to school, due to delays, the commute could be hours long. Therefore, these longer commute times will pull out the durations, making the distributions abnormal. I would like to find the parameters for the probability distributions.


# 3) Is the difference between the length of the commute depending on the directionality (i.e., school to home versus home to school)? During my exploratory data analysis, I noticed that box plots created of distances had very different spreads. I would hope to test this with an F-test for variances between two populations.
ggplot(commute_2, aes(total_time)) +
  geom_histogram() +
  facet_wrap('destination')


# 4) What segments of the commute have the largest contribution to the overall length? This would deal with the method of partial correlation to see which parts of the commute have the largest proportional increase to the overall length of the journey I have.


# 5) How long can I expect my commute to be? Using the data, I would like to create confidence intervals for each segment.kki


#linear regression
lm(green_line ~ red_line, commute_2)

plot(commute_2$green_line,commute_2$red_line)
#seems to have very little effect on the two variables


#seems to have a stronger effect
lm(total_time ~ green_line, commute_2)
plotlm <- lm(total_time ~ green_line, commute_2)

plot(commute_2$green_line,commute_2$total_time)
abline(plotlm)
plot(plotlm)
