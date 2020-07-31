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
# gom_df[,c("lagged_total_ccov", "lagged_Algae", "Fish_density")] <- lapply(gom_df[,c("lagged_total_ccov", "lagged_Algae", "Fish_density")], log)
covars <- c("lagged_total_ccov", "lagged_Algae", "percent_bleached"
            , "Pop10k_decay", "MMM_Chla", "inorganics", "Fish_density", "Max_DHW")
gom_df[, covars] <- lapply(gom_df[, covars], scale)

# subset
benthic_change <- subset(gom_df, Year == 2010 | Year == 2016)

# format response variables
benthic_change$delta_ccov_beta <- (benthic_change$delta_ccov + 100) / 200 
benthic_change$delta_algae_beta <- (benthic_change$delta_algae + 100) / 200

# coral model ---------------------------------------------------
coral_mod <- glmmTMB(delta_ccov_beta ~ percent_bleached 
                     # + Max_DHW
                     + lagged_total_ccov
                     + Fish_density
                     # + Pop10k_decay
                     + MMM_Chla
                     # + inorganics
                     + (1|Island/Site_Number/LIT_Number),
                     data = benthic_change,
                     beta_family(link = "logit"))

summary(coral_mod)

# predict with model and compare with observations
benthic_change$ypred <- predict(coral_mod, benthic_change)
benthic_changeLm <- lm(benthic_change$ypred~benthic_change$delta_ccov_beta)
plot(benthic_change$delta_ccov_beta, benthic_change$ypred, pch=16, xlab = "Observed", ylab = "Predicted", main = "Coral")
abline(benthic_changeLm)
legend("topleft", legend = bquote(italic(R)^2 == .(format(summary(benthic_changeLm)$r.squared, digits = 2))), bty = 'n') 

# algae model ---------------------------------------------------
algae_mod <- glmmTMB(delta_algae_beta ~ #percent_bleached
                     + Max_DHW
                     + lagged_Algae
                     # + Fish_density
                     # + Pop10k_decay
                     + MMM_Chla
                     + inorganics
                     + (1|Island/Site_Number/LIT_Number),
                     data = benthic_change,
                     beta_family(link = "logit"))

summary(algae_mod)

# predict with model and compare with observations
benthic_change$ypred2 <- predict(algae_mod, benthic_change)
benthic_changeLm2 <- lm(benthic_change$ypred2~benthic_change$delta_algae_beta)
plot(benthic_change$delta_algae_beta, benthic_change$ypred2, pch=16, xlab = "Observed", ylab = "Predicted", main = "Algae")
abline(benthic_changeLm2)
legend("topleft", legend = bquote(italic(R)^2 == .(format(summary(benthic_changeLm2)$r.squared, digits = 2))), bty = 'n') 
