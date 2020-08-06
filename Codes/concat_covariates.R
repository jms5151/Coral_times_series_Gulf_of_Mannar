# combine covariate data --------------------------------
rm(list=ls()) #remove previous variable assignments

# load library 
library(tidyverse)

# load data
covariates <- read.csv("Data/BleachingImpacts_FinalModelingVariables_05OCT2018.csv", head = T, stringsAsFactors = F)
chla_max <- read.csv("Data/GoM_chla_mmm_2010_2016.csv", head = T, stringsAsFactors = F)
dhw_max <- read.csv("Data/GoM_max_dhw_2010_2016.csv", head = T, stringsAsFactors = F)
bleaching <- read.csv("Data/coral_bleaching_gom.csv", head = T, stringsAsFactors = F)
water_clarity <- read.csv("Data/site_Sechi_reading_and_depth.csv", head = T, stringsAsFactors = F)

# create new variables
# covariates$water_clarity <- covariates$secchi.disc..m..avg..2005.2017/covariates$Max_Depth_m
covariates$Pop10k_decay <- covariates$Pop10k/covariates$dist_coast^2
covariates$inorganics <- rowMeans(covariates[,c("inorgan", "X13_inorgan")])

# format covariate data
covariates <- covariates[c(1:21),c("Island_name",
                                   "avg..sedimentation.rate.2005.17..mg.cm2.day.",
                                   "Fish.density..avg.250m2.",
                                   "ocean_p",
                                   # "water_clarity",                           
                                   "Pop10k_decay",
                                   "inorganics")]

colnames(covariates)[1:4] <- c("Island", 
                               "Sedimentation_rate", 
                               "Fish_density", 
                               "Ocean_pollution")

# format island names
covariates$Island <- gsub(" Island", "", covariates$Island)
covariates$Island[covariates$Island == "Pulivinichalli"] <- "Puluvinichalli"

# water clarity
# format data
colnames(water_clarity) <- c("Island", "Site_Number", "Year", "Secchi_disc", "Max_depth")

water_clarity$Site_Number <- gsub("St ", "", water_clarity$Site_Number)
water_clarity$Site_Number <- as.integer(water_clarity$Site_Number)
water_clarity$water_clarity <- water_clarity$Secchi_disc/water_clarity$Max_depth

water_clarity$Island <- gsub("Manoli ", "Manoli", water_clarity$Island)
water_clarity$Island <- gsub("Thalaiyari", "Thalayari", water_clarity$Island)

# bleaching
bleaching$total_bleached <- rowSums(bleaching[,c("ACB_Bleached", 
                                                 "ACT_Bleached",
                                                 "ACD_Bleached", 
                                                 "ACF_Bleached", 
                                                 "ACE_Bleached",
                                                 "CM_Bleached", 
                                                 "CS_Bleached", 
                                                 "CB_Bleached",
                                                 "CF_Bleached", 
                                                 "CE_Bleached")])

bleaching$total_ccov <- rowSums(bleaching[,c("total_bleached",
                                             "ACB_Non.bleached",
                                             "ACT_Non.bleached",
                                             "ACD_Non.bleached",
                                             "ACF_Non.bleached",
                                             "ACE_Non.bleached",
                                             "CM_Non.bleached",
                                             "CS_Non.bleached",
                                             "CB_Non.bleached",
                                             "CF_Non.bleached",
                                             "CE_Non.bleached")])

bleaching$percent_bleached <- bleaching$total_bleached/bleaching$total_ccov

# subset data
bleaching <- bleaching[,c("LIT", "Station", "Island", "Year", "percent_bleached")]
colnames(bleaching)[1:2] <- c("LIT_Number", "Site_Number")
  
# concatenate all covariates
covariates_final <- covariates %>% 
  left_join(chla_max, by = c("Island")) %>%
  left_join(dhw_max, by = c("Island", "Year")) %>%
  left_join(bleaching, by = c("Island", "Year")) %>%
  left_join(water_clarity, by = c("Island", "Site_Number", "Year"))

# save data
write.csv(covariates_final, "Data/output/concatenated_covariates.csv", row.names=F)
