# regression models ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(betareg)

# load data
covariates <- read.csv("Data/concatenated_covariates.csv", head = T)
source("Codes/format_response_vars.R")

# center, scale, and/or covariates
scaled_covariates <- covariates
scaled_covariates$Pop10k_decay <- log(scaled_covariates$Pop10k_decay)
scaled_covariates[,c(2,3,5:7,9:19)] <- lapply(scaled_covariates[,c(2,3,5:7,9:19)], scale)

# coral mortality 2010 -------------------------------------------------
ccov_mortality_2010 <- merge(ccov_mortality_2010, scaled_covariates, by = "Island")
ccov_mortality_2010$delta_ccov_beta <- (ccov_mortality_2010$delta_ccov+100)/200

# VIF for full model
full_mod_ccov_mortality_2010 <- betareg(delta_ccov_beta ~ Sedimentation_rate 
                    + Fish_density 
                    # + Ocean_pollution
                    + water_clarity 
                    + Pop10k_decay 
                    # + inorganics
                    + MMM_Chla_2010
                    + Max_DHW_2010
                    + diversity_2009
                    + Mean_bleaching_2010
                    , data = ccov_mortality_2010)

sort(vif(full_mod_ccov_mortality_2010))

# final model
ccov_mortality_2010_select <- betareg(delta_ccov_beta ~ Fish_density + MMM_Chla_2010 + Max_DHW_2010 + Mean_bleaching_2010, data = ccov_mortality_2010)
summary(ccov_mortality_2010_select)

# coral mortality 2016 -------------------------------------------------
ccov_mortality_2016 <- merge(ccov_mortality_2016, scaled_covariates, by = "Island")
ccov_mortality_2016$delta_ccov_beta <- (ccov_mortality_2016$delta_ccov+100)/200

# VIF for full model
full_mod_ccov_mortality_2016 <- betareg(delta_ccov_beta ~ Sedimentation_rate 
                              + Fish_density 
                              # + Ocean_pollution
                              + water_clarity 
                              + Pop10k_decay 
                              # + inorganics
                              + MMM_Chla_2016
                              + Max_DHW_2016
                              + diversity_2015
                              + Mean_bleaching_2016
                              , data = ccov_mortality_2016)

sort(vif(full_mod_ccov_mortality_2016))

# final model
ccov_mortality_2016_select <- betareg(delta_ccov_beta ~ MMM_Chla_2016 + Max_DHW_2016, data = ccov_mortality_2016)
summary(ccov_mortality_2016_select)

# coral recovery 2011 -------------------------------------------------
ccov_recovery_2011 <- merge(ccov_recovery_2011, scaled_covariates, by = "Island")
ccov_recovery_2011$delta_ccov_beta <- (ccov_recovery_2011$delta_ccov+100)/200

# VIF for full model
full_mod_ccov_recovery_2011 <- betareg(delta_ccov_beta ~ Sedimentation_rate 
                    + Fish_density 
                    # + Ocean_pollution
                    + water_clarity 
                    + Pop10k_decay 
                    # + inorganics
                    + MMM_Chla_2010
                    + Max_DHW_2010
                    + diversity_2009
                    + Mean_bleaching_2010
                    , data = ccov_recovery_2011)

sort(vif(full_mod_ccov_recovery_2011))

# final model
ccov_recovery_2011_select <- betareg(delta_ccov_beta ~ diversity_2009 + Mean_bleaching_2010, data = ccov_recovery_2011)
summary(ccov_recovery_2011_select)

# coral recovery 2017 -------------------------------------------------
ccov_recovery_2017 <- merge(ccov_recovery_2017, scaled_covariates, by = "Island")
ccov_recovery_2017$delta_ccov_beta <- (ccov_recovery_2017$delta_ccov+100)/200

# VIF for full model
full_mod_ccov_recovery_2017 <- betareg(delta_ccov_beta ~ Sedimentation_rate 
                             + Fish_density 
                             # + Ocean_pollution
                             + water_clarity 
                             + Pop10k_decay 
                             # + inorganics
                             + MMM_Chla_2016
                             + Max_DHW_2016
                             + diversity_2015
                             + Mean_bleaching_2016
                             , data = ccov_recovery_2017)

sort(vif(full_mod_ccov_recovery_2017))

# final model
ccov_recovery_2017_select <- betareg(delta_ccov_beta ~ Sedimentation_rate + Fish_density + Pop10k_decay + MMM_Chla_2016 + Max_DHW_2016 + Mean_bleaching_2016, data = ccov_recovery_2017)
summary(ccov_recovery_2017_select)

# Algae "recovery" 2010 ------------------------------------------------------
Algae_recovery_2010 <- merge(Algae_recovery_2010, scaled_covariates, by = "Island")
Algae_recovery_2010$delta_algae_cov_beta <- (Algae_recovery_2010$delta_algae_cov+100)/200

# VIF for full model
full_mod_Algae_recovery_2010 <- betareg(delta_algae_cov_beta ~ Sedimentation_rate 
                          + Fish_density 
                          # + Ocean_pollution
                          + water_clarity 
                          + Pop10k_decay 
                          # + inorganics
                          + MMM_Chla_2010
                          + Max_DHW_2010
                          + diversity_2009
                          + Mean_bleaching_2010
                          , data = Algae_recovery_2010)

sort(vif(full_mod_Algae_recovery_2010))

# final model
Algae_recovery_2010_select <- betareg(delta_algae_cov_beta ~ Sedimentation_rate + diversity_2009, data = Algae_recovery_2010)
summary(Algae_recovery_2010_select)

# Algae "recovery" 2016 ------------------------------------------------------
Algae_recovery_2016 <- merge(Algae_recovery_2016, scaled_covariates, by = "Island")
Algae_recovery_2016$delta_algae_cov_beta <- (Algae_recovery_2016$delta_algae_cov+100)/200

# VIF for full model
full_mod_Algae_recovery_2016 <- betareg(delta_algae_cov_beta ~ Sedimentation_rate 
                                   + Fish_density 
                                   # + Ocean_pollution
                                   + water_clarity 
                                   + Pop10k_decay 
                                   # + inorganics
                                   + MMM_Chla_2016
                                   + Max_DHW_2016
                                   + diversity_2015
                                   + Mean_bleaching_2016
                                   , data = Algae_recovery_2016)

sort(vif(full_mod_Algae_recovery_2016))

# final model
Algae_recovery_2016_select <- betareg(delta_algae_cov_beta ~ Pop10k_decay + Max_DHW_2016 + diversity_2015, data = Algae_recovery_2016)
summary(Algae_recovery_2016_select)

# Soft coral ---------------------------------------------------
soft_coral <- merge(soft_cov, scaled_covariates, by = "Island")
soft_coral$delta_soft_cov_beta <- (soft_coral$delta_soft_cov+100)/200

# VIF for full model
full_mod_soft_coral <- betareg(delta_soft_cov_beta ~ Sedimentation_rate 
                             + Fish_density
                             # + Ocean_pollution
                             + water_clarity 
                             + Pop10k_decay 
                             # + inorganics 
                             + MMM_Chla_2010
                             + Max_DHW_2010
                             + diversity_2009
                             + Mean_bleaching_2010
                             , data = soft_coral)

sort(vif(full_mod_soft_coral))

# final model
soft_coral_select <- betareg(delta_soft_cov_beta ~ Fish_density + water_clarity + Mean_bleaching_2010, data = soft_coral)
summary(soft_coral_select)

# ACE -------------------------------------------------------------
ACE <- merge(ACE_cov, scaled_covariates, by = "Island")
ACE$delta_ACE_cov_beta <- (ACE$delta_ACE_cov+100)/200

# VIF for full model
full_mod_ACE <- betareg(delta_ACE_cov_beta ~ Sedimentation_rate 
                          + Fish_density 
                          # + Ocean_pollution
                          + water_clarity
                          + Pop10k_decay 
                          # + inorganics
                          + MMM_Chla_2016
                          + Max_DHW_2016
                          + diversity_2015
                          + Mean_bleaching_2016
                          , data = ACE)

sort(vif(full_mod_ACE))

# final model
ACE_select <- betareg(delta_ACE_cov_beta ~ water_clarity + Pop10k_decay + Max_DHW_2016 + diversity_2015 + Mean_bleaching_2016, data = ACE)
summary(ACE_select)

# Algae -------------------------------------------------------------
algae <- merge(Algae_cov, scaled_covariates, by = "Island")
algae$delta_algae_cov_beta <- (algae$delta_algae_cov+100)/200

# VIF for full model
full_mod_algae <- betareg(delta_algae_cov_beta ~ Sedimentation_rate 
                   + Fish_density 
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + inorganics
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = algae)

sort(vif(full_mod_algae))

# final model
algae_select <- betareg(delta_algae_cov_beta ~ water_clarity + Pop10k_decay + Max_DHW_2016 + diversity_2015, data = algae)
summary(algae_select)

# CCA -------------------------------------------------------------
CCA <- merge(CCA_cov, scaled_covariates, by = "Island")
CCA$delta_CCA_cov_beta <- (CCA$delta_CCA_cov+100)/200

# VIF for full model
full_mod_CCA <- betareg(delta_CCA_cov_beta ~ Sedimentation_rate 
                     + Fish_density 
                     # + Ocean_pollution
                     + water_clarity
                     + Pop10k_decay 
                     # + inorganics # this can be kept in if DHW is removed
                     + MMM_Chla_2016
                     + Max_DHW_2016
                     + diversity_2015
                     + Mean_bleaching_2016
                     , data = CCA)

sort(vif(full_mod_CCA))

# final model
CCA_select <- betareg(delta_CCA_cov_beta ~ 1, data = CCA)
summary(CCA_select)

# CS -------------------------------------------------------------
CS <- merge(CS_cov, scaled_covariates, by = "Island")
CS$delta_CS_cov_beta <- (CS$delta_CS_cov+100)/200

# VIF for full model
full_mod_CS <- betareg(delta_CS_cov_beta ~ Sedimentation_rate 
                   + Fish_density 
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + inorganics 
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = CS)

sort(vif(full_mod_CS))

# final model
CS_select <- betareg(delta_CS_cov_beta ~ Sedimentation_rate + Fish_density + water_clarity, data = CS)
summary(CS_select)

# CB -------------------------------------------------------------
CB <- merge(CB_cov, scaled_covariates, by = "Island")
CB$delta_CB_cov_beta <- (CB$delta_CB_cov+100)/200

# VIF for full model
full_mod_CB <- betareg(delta_CB_cov_beta ~ Sedimentation_rate 
                  + Fish_density 
                  # + Ocean_pollution
                  + water_clarity
                  + Pop10k_decay 
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = CB)

sort(vif(full_mod_CB))

# final model
CB_select <- betareg(delta_CB_cov_beta ~ Sedimentation_rate + Fish_density + water_clarity + Pop10k_decay + MMM_Chla_2016 + diversity_2015, data = CB)
summary(CB_select)

# ACT -------------------------------------------------------------
ACT <- merge(ACT_cov, scaled_covariates, by = "Island")
ACT$delta_ACT_cov_beta <- (ACT$delta_ACT_cov+100)/200

# VIF for full model
full_mod_ACT <- betareg(delta_ACT_cov_beta ~ Sedimentation_rate 
                  + Fish_density 
                  # + Ocean_pollution
                  + water_clarity
                  + Pop10k_decay 
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = ACT)

sort(vif(full_mod_ACT))

# final model
ACT_select <- betareg(delta_ACT_cov_beta ~ Sedimentation_rate + Fish_density + water_clarity + diversity_2015, data = ACT)
summary(ACT_select)

# ACD -------------------------------------------------------------
ACD <- merge(ACD_cov, scaled_covariates, by = "Island")
ACD$delta_ACD_cov_beta <- (ACD$delta_ACD_cov+100)/200

# VIF for full model
full_mod_ACD <- betareg(delta_ACD_cov_beta ~ Sedimentation_rate 
                   + Fish_density 
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + inorganics # this could be changed with DHW
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = ACD)

sort(vif(full_mod_ACD))

# final model
ACD_select <- betareg(delta_ACD_cov_beta ~ Sedimentation_rate + Max_DHW_2016 + diversity_2015, data = ACD)
summary(ACD_select)

# CE -------------------------------------------------------------
CE <- merge(CE_cov, scaled_covariates, by = "Island")
CE$delta_CE_cov_beta <- (CE$delta_CE_cov+100)/200

# VIF for full model
full_mod_CE <- betareg(delta_CE_cov_beta ~ Sedimentation_rate 
                   + Fish_density 
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + inorganics
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = CE)

sort(vif(full_mod_CE))

# final model
CE_select <- betareg(delta_CE_cov_beta ~ Sedimentation_rate + Pop10k_decay + Max_DHW_2016, data = CE)
summary(CE_select)

# CF -------------------------------------------------------------
CF <- merge(CF_cov, scaled_covariates, by = "Island")
CF$delta_CF_cov_beta <- (CF$delta_CF_cov+100)/200

# VIF for full model
full_mod_CF <- betareg(delta_CF_cov_beta ~ Sedimentation_rate 
                  + Fish_density 
                  # + Ocean_pollution
                  + water_clarity
                  + Pop10k_decay 
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = CF)

sort(vif(full_mod_CF))

# final model
CF_select <- betareg(delta_CF_cov_beta ~ water_clarity + Pop10k_decay + MMM_Chla_2016 + Max_DHW_2016 + diversity_2015, data = CF)
summary(CF_select)

# ACB -------------------------------------------------------------
ACB <- merge(ACB_cov, scaled_covariates, by = "Island")
ACB$delta_ACB_cov_beta <- (ACB$delta_ACB_cov+100)/200

# VIF for full model
full_mod_ACB <- betareg(delta_ACB_cov_beta ~ Sedimentation_rate 
                  + Fish_density 
                  # + Ocean_pollution
                  + water_clarity
                  + Pop10k_decay 
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = ACB)

sort(vif(full_mod_ACB))

# final model
ACB_select <- betareg(delta_ACB_cov_beta ~ Sedimentation_rate + Fish_density + Pop10k_decay + MMM_Chla_2016 + Max_DHW_2016, data = ACB)
summary(ACB_select)

# CM -------------------------------------------------------------
CM <- merge(CM_cov, scaled_covariates, by = "Island")
CM$delta_CM_cov_beta <- (CM$delta_CM_cov+100)/200

# VIF for full model
full_mod_CM <- betareg(delta_CM_cov_beta ~ Sedimentation_rate 
                   + Fish_density 
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + inorganics
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = CM)

sort(vif(full_mod_CM))

# final model
CM_select <- betareg(delta_CM_cov_beta ~ diversity_2015, data = CM)
summary(CM_select)

