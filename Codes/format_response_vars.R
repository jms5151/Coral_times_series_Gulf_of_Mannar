# format composition data for regression models -------------------------
# load library
library(tidyverse)

# load data
composition <- read.csv("Data/GoM_data_2005_to_2017_Checked.csv", head = T)

# total coral cover
coral_types <- c("ACB", "ACT", "ACD", "ACF", "ACE", "CM", "CS", "CB", "CF", "CE")
composition$total_ccov <- rowSums(composition[,coral_types])

# reorder 
composition <- composition[order(composition$LIT_Number, composition$Site_Number, composition$Island, composition$Year),]

# calculate lags, first year of each transect will be wrong with this method
changeFun <- function(vec_name) {vec_name - lag(vec_name)}
composition[, c("delta_ccov", "delta_algae")] <- lapply(composition[, c("total_ccov", "Algae")], changeFun)

lagFun <- function(vec_name) {lag(vec_name)}
benthic_types <- c("total_ccov", "Algae")
lag_benthic_types <- paste0("lagged_", benthic_types)
composition[, lag_benthic_types] <- lapply(composition[, benthic_types], lagFun)

# save data
write.csv(composition, "Data/output/benthic_cover.csv", row.names=F)
