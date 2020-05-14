# regression models ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(ggplot2)
library(tidyverse)
library(car)
library(MASS)
library(betareg)

# load data
covariates <- read.csv("Data/concatenated_covariates.csv", head = T)
source("Codes/format_response_vars.R")

# format bleaching and chl covariates
bleaching1_covariates <- covariates[, grepl("Island|2009|2010", names(covariates))]
bleaching1_covariates$Year <- 2010
colnames(bleaching1_covariates) <- gsub("_2009|_2010", "", colnames(bleaching1_covariates))

bleaching2_covariates <- covariates[, grepl("Island|2015|2016", names(covariates))]
bleaching2_covariates$Year <- 2016
colnames(bleaching2_covariates) <- gsub("_2015|_2016", "", colnames(bleaching2_covariates))

bleaching_covariates <- rbind(bleaching1_covariates, bleaching2_covariates)
colnames(bleaching_covariates)[4:5] <- c("pre_bleaching_diversity", "post_bleaching_diversity") 

bleaching_scaled_covariates <- bleaching_covariates
bleaching_scaled_covariates[,c(2:6)] <- lapply(bleaching_scaled_covariates[,c(2:6)], scale)

# center, scale, and/or covariates
scaled_covariates <- covariates
scaled_covariates$Pop10k_decay <- log(scaled_covariates$Pop10k_decay)
scaled_covariates[,c(2,3,5:7,9:19)] <- lapply(scaled_covariates[,c(2,3,5:7,9:19)], scale)

# coral mortality -------------------------------------------------
ccov_mortality <- merge(ccov_mortality, scaled_covariates[,c(1:9)], by = "Island")
ccov_mortality <- merge(ccov_mortality, bleaching_scaled_covariates, by = c("Island", "Year"))

# VIF for full model
full_mod_ccov_mortality <- lm(delta_ccov ~ Sedimentation_rate 
                    + Fish_density 
                    # + Ocean_pollution
                    + water_clarity 
                    + Pop10k_decay 
                    + inorganics
                    + MMM_Chla
                    + Max_DHW
                    + pre_bleaching_diversity
                    + Mean_bleaching
                    , data = ccov_mortality)

sort(vif(full_mod_ccov_mortality))

# model selection
ccov_mod_mortality_select <- stepAIC(full_mod_ccov_mortality, direction = 'backward')
summary(ccov_mod_mortality_select)

# coral recovery -------------------------------------------------
ccov_recovery <- merge(ccov_recovery, scaled_covariates[,c(1:9)], by = "Island")
ccov_recovery$Year <- ifelse(ccov_recovery$Year == 2011, 2010, 2016)
ccov_recovery <- merge(ccov_recovery, bleaching_scaled_covariates, by = c("Island", "Year"))

# VIF for full model
full_mod_ccov_recovery <- lm(delta_ccov ~ Sedimentation_rate 
                    + Fish_density 
                    # + Ocean_pollution
                    + water_clarity 
                    + Pop10k_decay 
                    + inorganics
                    + MMM_Chla
                    + Max_DHW
                    + pre_bleaching_diversity
                    + Mean_bleaching
                    , data = ccov_recovery)

sort(vif(full_mod_ccov_recovery))

# model selection
ccov_mod_recovery_select <- stepAIC(full_mod_ccov_recovery, direction = 'backward')
summary(ccov_mod_recovery_select)

# Algae "recovery" ------------------------------------------------------
Algae_recovery <- merge(Algae_recovery, scaled_covariates, by = "Island")
Algae_recovery <- merge(Algae_recovery, bleaching_scaled_covariates, by = c("Island", "Year"))

# VIF for full model
full_mod_Algae_recovery <- lm(delta_algae_cov ~ Sedimentation_rate 
                          + Fish_density 
                          # + Ocean_pollution
                          + water_clarity 
                          + Pop10k_decay 
                          + inorganics
                          + MMM_Chla
                          + Max_DHW
                          + pre_bleaching_diversity
                          + Mean_bleaching
                          , data = Algae_recovery)

sort(vif(full_mod_Algae_recovery))

# model selection
Algae_recovery_mod_select <- stepAIC(full_mod_Algae_recovery, direction = 'backward')
summary(Algae_recovery_mod_select)

# Soft coral ---------------------------------------------------
soft_coral <- merge(soft_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_Algae_recovery <- lm(delta_soft_cov ~ Sedimentation_rate 
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

sort(vif(full_mod_Algae_recovery))

# model selection
Algae_recovery_mod_select <- stepAIC(full_mod_Algae_recovery, direction = 'backward')
summary(Algae_recovery_mod_select)

# ACE -------------------------------------------------------------
ACE <- merge(ACE_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_ACE <- lm(delta_ACE_cov ~ Sedimentation_rate 
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

# model selection
ACE_mod_select <- stepAIC(full_mod_ACE, direction = 'backward')
summary(ACE_mod_select)

# Algae -------------------------------------------------------------
algae <- merge(Algae_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_algae <- lm(delta_algae_cov ~ Sedimentation_rate 
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

# model selection
algae_mod_select <- stepAIC(full_mod_algae, direction = 'backward')
summary(algae_mod_select)

# CCA -------------------------------------------------------------
CCA <- merge(CCA_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_CCA <- lm(delta_CCA_cov ~ Sedimentation_rate 
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

# model selection
CCA_mod_select <- stepAIC(full_mod_CCA, direction = 'backward')
summary(CCA_mod_select)

# CS -------------------------------------------------------------
CS <- merge(CS_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_CS <- lm(delta_CS_cov ~ Sedimentation_rate 
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

# model selection
CS_mod_select <- stepAIC(full_mod_CS, direction = 'backward')
summary(CS_mod_select)

# CB -------------------------------------------------------------
CB <- merge(CB_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_CB <- lm(delta_CB_cov ~ Sedimentation_rate 
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

# model selection
CB_mod_select <- stepAIC(full_mod_CB, direction = 'backward')
summary(CB_mod_select)

# ACT -------------------------------------------------------------
ACT <- merge(ACT_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_ACT <- lm(delta_ACT_cov ~ Sedimentation_rate 
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

# model selection
ACT_mod_select <- stepAIC(full_mod_ACT, direction = 'backward')
summary(ACT_mod_select)

# ACD -------------------------------------------------------------
ACD <- merge(ACD_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_ACD <- lm(delta_ACD_cov ~ Sedimentation_rate 
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

# model selection
ACD_mod_select <- stepAIC(full_mod_ACD, direction = 'backward')
summary(ACD_mod_select)

# CE -------------------------------------------------------------
CE <- merge(CE_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_CE <- lm(delta_CE_cov ~ Sedimentation_rate 
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

# model selection
CE_mod_select <- stepAIC(full_mod_CE, direction = 'backward')
summary(CE_mod_select)

# CF -------------------------------------------------------------
CF <- merge(CF_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_CF <- lm(delta_CF_cov ~ Sedimentation_rate 
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

# model selection
CF_mod_select <- stepAIC(full_mod_CF, direction = 'backward')
summary(CF_mod_select)

# ACB -------------------------------------------------------------
ACB <- merge(ACB_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_ACB <- lm(delta_ACB_cov ~ Sedimentation_rate 
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

# model selection
ACB_mod_select <- stepAIC(full_mod_ACB, direction = 'backward')
summary(ACB_mod_select)

# CM -------------------------------------------------------------
CM <- merge(CM_cov, scaled_covariates, by = "Island")

# VIF for full model
full_mod_CM <- lm(delta_CM_cov ~ Sedimentation_rate 
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

# model selection
CM_mod_select <- stepAIC(full_mod_CM, direction = 'backward')
summary(CM_mod_select)

