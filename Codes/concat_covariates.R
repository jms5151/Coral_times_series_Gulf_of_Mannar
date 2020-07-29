# combine covariate data --------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
covariates <- read.csv("Data/BleachingImpacts_FinalModelingVariables_05OCT2018.csv", head = T, stringsAsFactors = F)
chla_max <- read.csv("Data/GoM_chla_mmm_2010_2016.csv", head = T, stringsAsFactors = F)
dhw_max <- read.csv("Data/GoM_max_dhw_2010_2016.csv", head = T, stringsAsFactors = F)
bleaching <- read.csv("Data/coral_bleaching_gom.csv", head = T, stringsAsFactors = F)

# create new variables
covariates$water_clarity <- covariates$secchi.disc..m..avg..2005.2017/covariates$Max_Depth_m
covariates$Pop10k_decay <- covariates$Pop10k/covariates$dist_coast^2
covariates$inorganics <- rowMeans(covariates[,c("inorgan", "X13_inorgan")])

# format covariate data
covariates <- covariates[c(1:21),c("Island_name",
                                   "avg..sedimentation.rate.2005.17..mg.cm2.day.",
                                   "Fish.density..avg.250m2.",
                                   "ocean_p",
                                   "water_clarity",                           
                                   "Pop10k_decay",
                                   "inorganics")]

colnames(covariates)[1:4] <- c("Island", "Sedimentation_rate", "Fish_density", "Ocean_pollution")

# format island names
covariates$Island <- gsub(" Island", "", covariates$Island)
covariates$Island[covariates$Island == "Pulivinichalli"] <- "Puluvinichalli"

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

# subset data
bleaching <- bleaching[,c("LIT", "Station", "Island", "Year", "total_bleached")]
colnames(bleaching)[1:2] <- c("LIT_Number", "Site_Number")
  
# combine 2010/2016 data and duplicate for 2011/2017 recovery models
covariates2 <- chla_max %>%
  left_join(dhw_max, by = c("Island", "Year")) %>%
  left_join(bleaching, by = c("Island", "Year"))

covariates2_dup <- covariates2
covariates2_dup$Year <- ifelse(covariates2_dup$Year == 2010, 2011, 2017)
covariates2 <- rbind(covariates2, covariates2_dup)

# concatenate all covariates
covariates_final <- covariates %>% 
  left_join(covariates2, by = "Island")

# save data
write.csv(covariates_final, "Data/output/concatenated_covariates.csv", row.names=F)
