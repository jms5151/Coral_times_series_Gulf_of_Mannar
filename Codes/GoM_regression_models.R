# regression models ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(glmmTMB)
library(TMB)

# load data
covariates <- read.csv("Data/output/concatenated_covariates.csv", head = T)
benthic_cover <- read.csv("Data/output/benthic_cover.csv", head = T)

# combine data
gom_df <- merge(benthic_cover, covariates, by = c("LIT_Number", "Site_Number", "Island", "Year"))

# center, scale, and/or transform covariates
gom_df$Pop10k_decay <- log(gom_df$Pop10k_decay)
covars <- c(
            # "lagged_ACB", "lagged_ACT", "lagged_ACD", "lagged_ACF", "lagged_ACE", 
            # "lagged_CM", "lagged_CS", "lagged_CB", "lagged_CF", "lagged_CE", 
            "lagged_total_ccov", "lagged_prop_acroporid_cov",
            "lagged_CCA", "lagged_Algae", "total_bleached", "water_clarity", 
            "Pop10k_decay", "MMM_Chla", "inorganics", "Fish_density")
gom_df[, covars] <- lapply(gom_df[, covars], scale)

# subset
mortality <- subset(gom_df, Year == 2010 | Year == 2016)
recovery <- subset(gom_df, Year == 2011 | Year == 2017)

# format response variables
mortality$delta_ccov_beta <- ((mortality$delta_ccov * -1) + 100) / 200
recovery$delta_ccov_beta <- (recovery$delta_ccov + 100) / 200
recovery$delta_algae_beta <- (recovery$delta_algae + 100) / 200

# coral mortality model ---------------------------------------------------
coral_mortality_mod <- glmmTMB(delta_ccov_beta ~ total_bleached
                               + lagged_total_ccov
                               + lagged_prop_acroporid_cov
                               + lagged_CCA
                               # + lagged_Algae
                               + Fish_density
                               ## + water_clarity
                               # + Pop10k_decay
                               + MMM_Chla
                               # + inorganics
                               + (1|Island/Site_Number/LIT_Number),
                               data = mortality,
                               beta_family(link = "logit"))

summary(coral_mortality_mod)

# coral recovery model ---------------------------------------------------
coral_recovery_mod <- glmmTMB(delta_ccov_beta ~ total_bleached
                              + lagged_total_ccov
                              + lagged_prop_acroporid_cov
                              # + lagged_CCA
                              # + lagged_Algae
                              + Fish_density
                              ## + water_clarity
                              # + Pop10k_decay
                              # + MMM_Chla
                              # + inorganics
                              + (1|Island/Site_Number/LIT_Number),
                               data = recovery,
                               beta_family(link = "logit"))

summary(coral_recovery_mod)

# algal growth model ---------------------------------------------------
algae_mod <- glmmTMB(delta_algae_beta ~ total_bleached
                     # + lagged_total_ccov
                     + lagged_prop_acroporid_cov
                     # + lagged_CCA
                     + lagged_Algae
                     + Fish_density
                     ## + water_clarity
                     # + Pop10k_decay
                     + MMM_Chla
                     + inorganics
                     + (1|Island/Site_Number/LIT_Number),
                     data = recovery,
                     beta_family(link = "logit"))

summary(algae_mod)