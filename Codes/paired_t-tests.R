library(tidyverse)
library(cars)

# load data
benthic_cover <- read.csv("Data/output/benthic_cover.csv", head = T)

# summarize data
cover10 <- benthic_cover[,c("Island", "Site_Number", "LIT_Number", "Year", "delta_ccov", "delta_algae")] %>% 
  filter(Year == 2010) %>%
  rename(delta_ccov10 = delta_ccov,
         delta_algae10 = delta_algae) %>%
  select(-Year)

cover16 <- benthic_cover[,c("Island", "Site_Number", "LIT_Number", "Year", "delta_ccov", "delta_algae")] %>% 
  filter(Year == 2016) %>%
  rename(delta_ccov16 = delta_ccov,
         delta_algae16 = delta_algae) %>%
  select(-Year)

cover <- cover10 %>%
  left_join(cover16) %>%
  group_by(Island) %>%
  summarise_at(c("delta_ccov10", "delta_ccov16", "delta_algae10", "delta_algae16"), mean)

# check for normality
qqPlot(cover$delta_ccov10)
qqPlot(cover$delta_ccov16)
qqPlot(cover$delta_algae10)
qqPlot(cover$delta_algae16)

# this is small sample size, so also comopare mean, median, and sd
compare_dists <- function(data){
  var1 <- mean(data)
  var2 <- median(data)
  var3 <- sd(data)
  cat("Mean =", var1, "Median =", var2, "SD =", var3)
}

compare_dists(cover$delta_ccov10)
compare_dists(cover$delta_ccov16)
compare_dists(cover$delta_algae10)
compare_dists(cover$delta_algae16)

# test hypothesis that mean change in coral cover differed between the two bleaching events 
t.test(cover$delta_ccov10, cover$delta_ccov16, paired = TRUE, alternative = "two.sided")
# test hypothesis that mean change in algal cover differed between the two bleaching events
t.test(cover$delta_algae10, cover$delta_algae16, paired = TRUE, alternative = "two.sided")
