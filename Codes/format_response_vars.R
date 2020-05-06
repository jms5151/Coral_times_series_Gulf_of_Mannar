# format composition data for regression models -------------------------

# load data
composition <- read.csv("Data/GoM_data_2005_to_2017_Checked.csv", head = T)

# group time periods pre/post bleaching events
composition$Bleaching_split_2010 <- ifelse(composition$Year <= 2009, "pre-2010", "post-2010")
composition$Bleaching_split_2016 <- ifelse(composition$Year < 2016, "pre-2016", "post-2016")

# total coral cover
composition$total_ccov <- rowSums(composition[,c("ACB", "CE", "ACD", "ACF", "ACE", "CM", "CS", "CB", "CF", "CE")])

ccov <- composition %>%
  group_by(Island, Year) %>%
  summarize(average_total_ccov = mean(total_ccov)) %>%
  mutate(prior_ccov = lag(average_total_ccov), delta_ccov = average_total_ccov - lag(average_total_ccov)) %>%
  filter(Year == 2010 | Year == 2011 | Year == 2016 | Year == 2017)

# Soft cover
soft_cov <- composition %>%
  group_by(Island, Bleaching_split_2010) %>%
  summarize(average_soft_cov = mean(Soft.coral)) %>%
  mutate(post_soft_cov = lag(average_soft_cov), delta_soft_cov = post_soft_cov - average_soft_cov) %>%
  filter(Bleaching_split_2010 == "pre-2010")

# ACE cover
ACE_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_ACE_cov = mean(ACE)) %>%
  mutate(post_ACE_cov = lag(average_ACE_cov), delta_ACE_cov = post_ACE_cov - average_ACE_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# Algae cover
Algae_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_algae_cov = mean(Algae)) %>%
  mutate(post_algae_cov = lag(average_algae_cov), delta_algae_cov = post_algae_cov - average_algae_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# CCA cover
CCA_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_CCA_cov = mean(CCA)) %>%
  mutate(post_CCA_cov = lag(average_CCA_cov), delta_CCA_cov = post_CCA_cov - average_CCA_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# CS cover
CS_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_CS_cov = mean(CS)) %>%
  mutate(post_CS_cov = lag(average_CS_cov), delta_CS_cov = post_CS_cov - average_CS_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# CB cover
CB_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_CB_cov = mean(CB)) %>%
  mutate(post_CB_cov = lag(average_CB_cov), delta_CB_cov = post_CB_cov - average_CB_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# ACT cover
ACT_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_ACT_cov = mean(ACT)) %>%
  mutate(post_ACT_cov = lag(average_ACT_cov), delta_ACT_cov = post_ACT_cov - average_ACT_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# ACD cover
ACD_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_ACD_cov = mean(ACD)) %>%
  mutate(post_ACD_cov = lag(average_ACD_cov), delta_ACD_cov = post_ACD_cov - average_ACD_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# CE cover
CE_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_CE_cov = mean(CE)) %>%
  mutate(post_CE_cov = lag(average_CE_cov), delta_CE_cov = post_CE_cov - average_CE_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# CF cover
CF_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_CF_cov = mean(CF)) %>%
  mutate(post_CF_cov = lag(average_CF_cov), delta_CF_cov = post_CF_cov - average_CF_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# ACB cover
ACB_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_ACB_cov = mean(ACB)) %>%
  mutate(post_ACB_cov = lag(average_ACB_cov), delta_ACB_cov = post_ACB_cov - average_ACB_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")

# CM cover
CM_cov <- composition %>%
  group_by(Island, Bleaching_split_2016) %>%
  summarize(average_CM_cov = mean(CM)) %>%
  mutate(post_CM_cov = lag(average_CM_cov), delta_CM_cov = post_CM_cov - average_CM_cov) %>%
  filter(Bleaching_split_2016 == "pre-2016")
