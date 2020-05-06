# combine covariate data --------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyr)
library(vegan)

# load data
composition <- read.csv("Data/GoM_data_2005_to_2017_Checked.csv", head = T, stringsAsFactors = F)
covariates <- read.csv("Data/BleachingImpacts_FinalModelingVariables_05OCT2018.csv", head = T, stringsAsFactors = F)
chla_max <- read.csv("Data/GoM_chla_mmm_2010_2016.csv", head = T, stringsAsFactors = F)
dhw_max <- read.csv("Data/GoM_max_dhw_2010_2016.csv", head = T, stringsAsFactors = F)
bleaching <- read.csv("Data/coral_bleaching_gom.csv", head = T, stringsAsFactors = F)

# create new variables
covariates$water_clarity <- covariates$secchi.disc..m..avg..2005.2017/covariates$Max_Depth_m
covariates$Pop10k_decay <- covariates$Pop10k/covariates$dist_coast^2
covariates$human_impact_change <- covariates$human.impact.rating.scale...1.low..2.med..3.high..2003.2005 - covariates$human.impact.rating.scale...1.low..2.med..3.high..2017
covariates$inorganics <- rowMeans(covariates[,c("inorgan", "X13_inorgan")])

# format covariate data
covariates <- covariates[c(1:21),c("Island_name",
                                   "avg..sedimentation.rate.2005.17..mg.cm2.day.",
                                   "Fish.density..avg.250m2.",
                                   "human.impact.rating.scale...1.low..2.med..3.high..2003.2005",
                                   "ocean_p",
                                   "water_clarity",                           
                                   "Pop10k_decay",
                                   "human_impact_change",
                                   "inorganics")]

colnames(covariates)[1:5] <- c("Island", "Sedimentation_rate", "Fish_density", "Human_impact_score_initial", "Ocean_pollution")

covariates$Island <- gsub(" Island", "", covariates$Island)
covariates$Island[covariates$Island == "Pulivinichalli"] <- "Puluvinichalli"

# chl-a data
chla_max_wide <- chla_max %>% spread(key=Year, value=MMM_Chla)
colnames(chla_max_wide)[2:3] <- paste0("MMM_Chla_", colnames(chla_max_wide)[2:3])

# dhw data
dhw_max_wide <- dhw_max %>% spread(key=Year, value=Max_DHW)
colnames(dhw_max_wide)[2:3] <- paste0("Max_DHW_", colnames(dhw_max_wide)[2:3])

# survey data
community_composition <- composition %>%
  group_by(Island, Year) %>%
  summarize_all(funs(mean))

community_composition <- community_composition[,c( "Island", "Year", "ACB", "ACT", "ACD", 
                                                         "ACE", "CM", "CS", "CB", "CF", 
                                                         "CE", "Soft.coral", "Algae", "CCA")]


years <- c(2009, 2010, 2015, 2016)
community_composition2 <- data.frame("Island" = unique(community_composition$Island))

for (i in years){
  x <- subset(community_composition, Year == i)
  y <- x[3:14]
  community_composition2$x <- diversity(y, "simpson")
  colnames(community_composition2)[ncol(community_composition2)] <- paste0("diversity_", i)
}
  
# bleaching
bleaching$total_bleached <- rowSums(bleaching[,c("ACB_Bleached", "ACT_Bleached",
                                                 "ACD_Bleached", "ACF_Bleached", "ACE_Bleached",
                                                 "CM_Bleached", "CS_Bleached", "CB_Bleached",
                                                 "CF_Bleached", "CE_Bleached")])
bleaching_composition <- bleaching %>%
  group_by(Island, Year) %>%
  summarize(average_bleaching = mean(total_bleached)) %>%
  spread(key = Year, value = average_bleaching)

colnames(bleaching_composition)[2:3] <- paste0("Mean_bleaching_", colnames(bleaching_composition)[2:3])

# concatenate all covariates
covariates_final <- covariates %>% 
  left_join(chla_max_wide, by="Island") %>%
  left_join(dhw_max_wide, by="Island") %>%
  left_join(community_composition2, by="Island") %>%
  left_join(bleaching_composition, by="Island")

# save data
write.csv(covariates_final, "Data/concatenated_covariates.csv", row.names=F)

# visualize pairwise correlations
library(ggcorrplot)
corr <- round(cor(covariates_final[,c(2:13)]), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)
