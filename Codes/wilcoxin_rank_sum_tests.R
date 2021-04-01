library(tidyverse)

# load data
benthic_cover <- read.csv("Data/output/benthic_cover.csv", head = T)

# mean differences in coral and algal cover 
cover05 <- benthic_cover[,c("Island", "Site_Number", "LIT_Number", "Year", "total_ccov", "Algae")] %>% 
  filter(Year == 2005) %>%
  rename(total_ccov05 = total_ccov,
         Algae05 = Algae) %>%
  select(-Year)

cover17 <- benthic_cover[,c("Island", "Site_Number", "LIT_Number", "Year", "total_ccov", "Algae")] %>% 
  filter(Year == 2017) %>%
  rename(total_ccov17 = total_ccov,
         Algae17 = Algae) %>%
  select(-Year)

cover <- cover05 %>%
  left_join(cover17) %>%
  group_by(Island) %>%
  summarise_at(c("total_ccov05", "total_ccov17", "Algae05", "Algae17"), mean)
  
# test hypothesis that mean coral cover decreases from the start to end of study period 
wx_coral <- wilcox.test(cover$total_ccov05, cover$total_ccov17, paired = TRUE , alternative = "greater")
# test hypothesis that mean algal cover increases from the start to end of study period 
wx_algae <- wilcox.test(cover$Algae05, cover$Algae17, paired = TRUE, alternative = "less") 
