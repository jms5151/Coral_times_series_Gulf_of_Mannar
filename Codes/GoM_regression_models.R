# regression models ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(ggplot2)
library(tidyverse)
library(car)
library(MASS)

# load data
covariates <- read.csv("Data/concatenated_covariates.csv", head = T)
source("Codes/format_response_vars.R")

# format bleaching covariates
bleaching1_covariates <- covariates[, grepl("Island|2009|2010", names(covariates))]
bleaching1_covariates$Year <- 2010
colnames(bleaching1_covariates) <- gsub("_2009|_2010", "", colnames(bleaching1_covariates))

bleaching2_covariates <- covariates[, grepl("Island|2015|2016", names(covariates))]
bleaching2_covariates$Year <- 2016
colnames(bleaching2_covariates) <- gsub("_2015|_2016", "", colnames(bleaching2_covariates))

bleaching_covariates <- rbind(bleaching1_covariates, bleaching2_covariates)
colnames(bleaching_covariates)[4:5] <- c("pre_bleaching_diversity", "post_bleaching_diversity") 

# coral mortality -------------------------------------------------
# combine covariates with mortality data
bleaching1_ccov_mortality <- ccov %>% filter(Year == 2010)
bleaching2_ccov_mortality <- ccov %>% filter(Year == 2016)

ccov_mortality <- rbind(bleaching1_ccov_mortality, bleaching2_ccov_mortality)
ccov_mortality <- merge(ccov_mortality, covariates[,c(1:9)], by = "Island")
ccov_mortality <- merge(ccov_mortality, bleaching_covariates, by = c("Island", "Year"))

# scale covariates
ccov_mortality_scaled <- ccov_mortality
ccov_mortality_scaled[,c(4,6:18)] <- lapply(ccov_mortality_scaled[,c(4,6:18)], scale)
  
# VIF for full model
full_mod_ccov_mortality <- lm(delta_ccov ~ prior_ccov 
                    + Sedimentation_rate 
                    + Fish_density 
                    # + Human_impact_score_initial 
                    # + Ocean_pollution
                    + water_clarity 
                    + Pop10k_decay 
                    + human_impact_change
                    + inorganics 
                    + MMM_Chla
                    + Max_DHW
                    + pre_bleaching_diversity
                    + Mean_bleaching
                    , data = ccov_mortality_scaled)

sort(vif(full_mod_ccov_mortality))

# model selection
summary(full_mod_ccov_mortality)
ccov_mod_mortality_select <- stepAIC(full_mod_ccov_mortality, direction = 'backward')
summary(ccov_mod_mortality_select)

# coral recovery -------------------------------------------------
bleaching_covariates$Year <- ifelse(bleaching_covariates$Year == 2010, 2011, 2017)

# combine covariates with recovery data
bleaching1_ccov_recovery <- ccov %>% filter(Year == 2011)
bleaching2_ccov_recovery <- ccov %>% filter(Year == 2017)

ccov_recovery <- rbind(bleaching1_ccov_recovery, bleaching2_ccov_recovery)
ccov_recovery <- merge(ccov_recovery, covariates[,c(1:9)], by = "Island")
ccov_recovery <- merge(ccov_recovery, bleaching_covariates, by = c("Island", "Year"))

# scale covariates
ccov_recovery_scaled <- ccov_recovery
ccov_recovery_scaled[,c(4,6:18)] <- lapply(ccov_recovery_scaled[,c(4,6:18)], scale)

# VIF for full model
full_mod_ccov_recovery <- lm(delta_ccov ~ prior_ccov 
                    + Sedimentation_rate 
                    + Fish_density 
                    # + Human_impact_score_initial
                    # + Ocean_pollution
                    + water_clarity 
                    + Pop10k_decay 
                    + human_impact_change
                    + inorganics 
                    + MMM_Chla
                    + Max_DHW
                    + pre_bleaching_diversity
                    + Mean_bleaching
                    , data = ccov_recovery_scaled)

sort(vif(full_mod_ccov_recovery))

# model selection
summary(full_mod_ccov_recovery)
ccov_mod_recovery_select <- stepAIC(full_mod_ccov_recovery, direction = 'backward')
summary(ccov_mod_recovery_select)

# Soft coral ---------------------------------------------------
soft_coral <- merge(soft_cov, covariates, by = "Island")

# scale covariates
soft_coral_scaled <- soft_coral
soft_coral_scaled[,c(4,6:23)] <- lapply(soft_coral_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_soft_coral <- lm(delta_soft_cov ~ average_soft_cov 
                             + Sedimentation_rate 
                             + Fish_density 
                             + Human_impact_score_initial
                             # + Ocean_pollution
                             + water_clarity 
                             + Pop10k_decay 
                             # + human_impact_change
                             # + inorganics # either this of DHW needs to be removed
                             + MMM_Chla_2010
                             + Max_DHW_2010
                             + diversity_2009
                             + Mean_bleaching_2010
                             , data = soft_coral_scaled)

sort(vif(full_mod_soft_coral))

# model selection
summary(full_mod_soft_coral)
soft_coral_mod_select <- stepAIC(full_mod_soft_coral, direction = 'backward')
summary(soft_coral_mod_select)

# ACE -------------------------------------------------------------
ACE <- merge(ACE_cov, covariates, by = "Island")

# scale covariates
ACE_scaled <- ACE
ACE_scaled[,c(4,6:23)] <- lapply(ACE_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_ACE <- lm(delta_ACE_cov ~ average_ACE_cov 
                          + Sedimentation_rate 
                          + Fish_density 
                          + Human_impact_score_initial
                          # + Ocean_pollution
                          # + water_clarity 
                          + Pop10k_decay 
                          # + human_impact_change
                          # + inorganics
                          + MMM_Chla_2016
                          + Max_DHW_2016
                          + diversity_2015
                          + Mean_bleaching_2016
                          , data = ACE_scaled)

sort(vif(full_mod_ACE))

# model selection
summary(full_mod_ACE)
ACE_mod_select <- stepAIC(full_mod_ACE, direction = 'backward')
summary(ACE_mod_select)

# Algae -------------------------------------------------------------
algae <- merge(Algae_cov, covariates, by = "Island")

# scale covariates
algae_scaled <- algae
algae_scaled[,c(4,6:23)] <- lapply(algae_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_algae <- lm(delta_algae_cov ~ average_algae_cov # can keep this in if diversity is removed 
                   + Sedimentation_rate 
                   + Fish_density 
                   + Human_impact_score_initial
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + human_impact_change
                   # + inorganics
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   # + diversity_2015
                   + Mean_bleaching_2016
                   , data = algae_scaled)

sort(vif(full_mod_algae))

# model selection
summary(full_mod_algae)
algae_mod_select <- stepAIC(full_mod_algae, direction = 'backward')
summary(algae_mod_select)

# CCA -------------------------------------------------------------
CCA <- merge(CCA_cov, covariates, by = "Island")

# scale covariates
CCA_scaled <- CCA
CCA_scaled[,c(4,6:23)] <- lapply(CCA_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_CCA <- lm(delta_CCA_cov ~ average_CCA_cov 
                     + Sedimentation_rate 
                     + Fish_density 
                     # + Human_impact_score_initial
                     # + Ocean_pollution
                     + water_clarity
                     + Pop10k_decay 
                     + human_impact_change
                     + inorganics # this can be kept in if DHW is removed
                     + MMM_Chla_2016
                     # + Max_DHW_2016
                     + diversity_2015
                     + Mean_bleaching_2016
                     , data = CCA_scaled)

sort(vif(full_mod_CCA))

# model selection
summary(full_mod_CCA)
CCA_mod_select <- stepAIC(full_mod_CCA, direction = 'backward')
summary(CCA_mod_select)

# CS -------------------------------------------------------------
CS <- merge(CS_cov, covariates, by = "Island")

# scale covariates
CS_scaled <- CS
CS_scaled[,c(4,6:23)] <- lapply(CS_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_CS <- lm(delta_CS_cov ~ average_CS_cov 
                   + Sedimentation_rate 
                   + Fish_density 
                   # + Human_impact_score_initial
                   # + Ocean_pollution
                   # + water_clarity
                   + Pop10k_decay 
                   + human_impact_change
                   + inorganics 
                   + MMM_Chla_2016
                   # + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = CS_scaled)

sort(vif(full_mod_CS))

# model selection
summary(full_mod_CS)
CS_mod_select <- stepAIC(full_mod_CS, direction = 'backward')
summary(CS_mod_select)

# CB -------------------------------------------------------------
CB <- merge(CB_cov, covariates, by = "Island")

# scale covariates
CB_scaled <- CB
CB_scaled[,c(4,6:23)] <- lapply(CB_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_CB <- lm(delta_CB_cov ~ average_CB_cov 
                  + Sedimentation_rate 
                  + Fish_density 
                  + Human_impact_score_initial
                  # + Ocean_pollution
                  # + water_clarity
                  + Pop10k_decay 
                  # + human_impact_change
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = CB_scaled)

sort(vif(full_mod_CB))

# model selection
summary(full_mod_CB)
CB_mod_select <- stepAIC(full_mod_CB, direction = 'backward')
summary(CB_mod_select)

# ACT -------------------------------------------------------------
ACT <- merge(ACT_cov, covariates, by = "Island")

# scale covariates
ACT_scaled <- ACT
ACT_scaled[,c(4,6:23)] <- lapply(ACT_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_ACT <- lm(delta_ACT_cov ~ average_ACT_cov 
                  + Sedimentation_rate 
                  + Fish_density 
                  + Human_impact_score_initial
                  # + Ocean_pollution
                  # + water_clarity
                  + Pop10k_decay 
                  # + human_impact_change
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = ACT_scaled)

sort(vif(full_mod_ACT))

# model selection
summary(full_mod_ACT)
ACT_mod_select <- stepAIC(full_mod_ACT, direction = 'backward')
summary(ACT_mod_select)

# ACD -------------------------------------------------------------
ACD <- merge(ACD_cov, covariates, by = "Island")

# scale covariates
ACD_scaled <- ACD
ACD_scaled[,c(4,6:23)] <- lapply(ACD_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_ACD <- lm(delta_ACD_cov ~ average_ACD_cov 
                   + Sedimentation_rate 
                   + Fish_density 
                   + Human_impact_score_initial
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + human_impact_change
                   + inorganics # this could be changed with DHW
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = ACD_scaled)

sort(vif(full_mod_ACD))

# model selection
summary(full_mod_ACD)
ACD_mod_select <- stepAIC(full_mod_ACD, direction = 'backward')
summary(ACD_mod_select)

# CE -------------------------------------------------------------
CE <- merge(CE_cov, covariates, by = "Island")

# scale covariates
CE_scaled <- CE
CE_scaled[,c(4,6:23)] <- lapply(CE_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_CE <- lm(delta_CE_cov ~ average_CE_cov 
                   + Sedimentation_rate 
                   + Fish_density 
                   + Human_impact_score_initial
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   # + human_impact_change
                   # + inorganics 
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = CE_scaled)

sort(vif(full_mod_CE))

# model selection
summary(full_mod_CE)
CE_mod_select <- stepAIC(full_mod_CE, direction = 'backward')
summary(CE_mod_select)

# CF -------------------------------------------------------------
CF <- merge(CF_cov, covariates, by = "Island")

# scale covariates
CF_scaled <- CF
CF_scaled[,c(4,6:23)] <- lapply(CF_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_CF <- lm(delta_CF_cov ~ average_CF_cov 
                  + Sedimentation_rate 
                  + Fish_density 
                  + Human_impact_score_initial
                  # + Ocean_pollution
                  # + water_clarity
                  + Pop10k_decay 
                  # + human_impact_change
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = CF_scaled)

sort(vif(full_mod_CF))

# model selection
summary(full_mod_CF)
CF_mod_select <- stepAIC(full_mod_CF, direction = 'backward')
summary(CF_mod_select)

# ACB -------------------------------------------------------------
ACB <- merge(ACB_cov, covariates, by = "Island")

# scale covariates
ACB_scaled <- ACB
ACB_scaled[,c(4,6:23)] <- lapply(ACB_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_ACB <- lm(delta_ACB_cov ~ average_ACB_cov 
                  + Sedimentation_rate 
                  + Fish_density 
                  + Human_impact_score_initial
                  # + Ocean_pollution
                  + water_clarity
                  + Pop10k_decay 
                  # + human_impact_change
                  # + inorganics
                  + MMM_Chla_2016
                  + Max_DHW_2016
                  + diversity_2015
                  + Mean_bleaching_2016
                  , data = ACB_scaled)

sort(vif(full_mod_ACB))

# model selection
summary(full_mod_ACB)
ACB_mod_select <- stepAIC(full_mod_ACB, direction = 'backward')
summary(ACB_mod_select)

# CM -------------------------------------------------------------
CM <- merge(CM_cov, covariates, by = "Island")

# scale covariates
CM_scaled <- CM
CM_scaled[,c(4,6:23)] <- lapply(CM_scaled[,c(4,6:23)], scale)

# VIF for full model
full_mod_CM <- lm(delta_CM_cov ~ average_CM_cov 
                   + Sedimentation_rate 
                   + Fish_density 
                   # + Human_impact_score_initial
                   # + Ocean_pollution
                   + water_clarity
                   + Pop10k_decay 
                   + human_impact_change
                   # + inorganics
                   + MMM_Chla_2016
                   + Max_DHW_2016
                   + diversity_2015
                   + Mean_bleaching_2016
                   , data = CM_scaled)

sort(vif(full_mod_CM))

# model selection
summary(full_mod_CM)
CM_mod_select <- stepAIC(full_mod_CM, direction = 'backward')
summary(CM_mod_select)

