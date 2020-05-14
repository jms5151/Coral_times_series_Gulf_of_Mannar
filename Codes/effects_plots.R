# partial effects plots ----------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(effects)

# load models
source("Codes/GoM_regression_models.R")

# coral mortality
effx1 <- effect("Mean_bleaching", ccov_mod_mortality_select, partial.residuals=T)

plot(effx1, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Coral mortality", xlab = "Mean bleaching (%)", main = "", pch=21)

effx2 <- effect("MMM_Chla", ccov_mod_mortality_select, partial.residuals=T)

plot(effx2, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Coral mortality", xlab = "Productivity (chlorophyll-a)", main = "", pch=21)

# coral recovery
effx3 <- effect("Sedimentation_rate", ccov_mod_recovery_select, partial.residuals=T)

plot(effx3, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Coral recovery", xlab = "Sedimentation rate", main = "", pch=21)

effx4 <- effect("Fish_density", ccov_mod_recovery_select, partial.residuals=T)

plot(effx4, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Coral recovery", xlab = "Fish density", main = "", pch=21)

effx5 <- effect("inorganics", ccov_mod_recovery_select, partial.residuals=T)

plot(effx5, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Coral recovery", xlab = "Inorganics", main = "", pch=21)

effx6 <- effect("Max_DHW", ccov_mod_recovery_select, partial.residuals=T)

plot(effx6, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Coral recovery", xlab = "Max degree heating weeks", main = "", pch=21)

# algae recovery
effx7 <- effect("Mean_bleaching_2010", Algae_recovery_mod_select, partial.residuals=T)

plot(effx7, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Increase in algal cover", xlab = "Mean bleaching (%)", main = "", pch=21)

effx8 <- effect("water_clarity", Algae_recovery_mod_select, partial.residuals=T)

plot(effx8, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Increase in algal cover", xlab = "Water clarity", main = "", pch=21)

effx9 <- effect("Fish_density", Algae_recovery_mod_select, partial.residuals=T)

plot(effx9, smooth.residuals=F, residuals.pch = 16, residuals.color = "darkblue"
     , ylab = "Increase in algal cover", xlab = "Fish density", main = "", pch=21)
