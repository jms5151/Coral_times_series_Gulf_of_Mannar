# Extract DHW and Chl-a data for bleaching years ----------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(raster)
library(ncdf4)
library(tidyverse)

# load site data
gom_coords <- read.csv("Data/GoM_GPS_coordinates.csv", head=T, stringsAsFactors = F)

# degree heating week data ------------------------------------------------------
# ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1/nc/v1.0/annual
# create new file to store daily temperautre data 
newFileName <- "Data/GoM_max_dhw_2010_2016.csv"
sst.df <- data.frame(matrix(ncol=3, nrow=0))
colnames(sst.df) <- c("Island", "Year", "Max_DHW")
write.csv(sst.df, newFileName, row.names = F)

# extract daily temperature data for all sites from coralTemp and save in "sst_persian_gulf.csv"
ncFiles <- list.files("Data/DHW/")

for (j in 1:length(ncFiles)){ 
  ncFileName <- paste0("Data/DHW/", ncFiles[j])
  ncTempBrick <- brick(ncFileName)
  surveySST <- extract(ncTempBrick, cbind(gom_coords$Longitude, gom_coords$Latitude))
  temp.df <- data.frame("Island"=gom_coords$Island, "Year"=substr(ncFiles[j],20,23), "Max_DHW"=surveySST[,])
  write.table(temp.df, file=newFileName, row.names = F, sep = ",", col.names = !file.exists(newFileName), append = T)
}

# chlorophyll-a data ------------------------------------------------------------
# https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/Monthly/4km/chlor_a/

# replace lat/lon for some Islands
gom_coords$Longitude[gom_coords$Island == "Pullivasal"] <- 79.190392
gom_coords$Latitude[gom_coords$Island == "Pullivasal"] <- 9.230677
 
gom_coords$Longitude[gom_coords$Island == "Manoliputi"] <- 79.167717
gom_coords$Latitude[gom_coords$Island == "Manoliputi"] <- 9.208859 

gom_coords$Longitude[gom_coords$Island == "Manoli"] <- 79.128234
gom_coords$Latitude[gom_coords$Island == "Manoli"] <- 9.205270 

gom_coords$Longitude[gom_coords$Island == "Upputhanni"] <- 78.494096
gom_coords$Latitude[gom_coords$Island == "Upputhanni"] <- 9.083069 
 
# list chlorophyll-a data files
chl_files <- list.files("Data/Chla/")
chl.df <- data.frame()

# data frame of chl-a values
for (k in 1:length(chl_files)){ 
  ncFileName <- paste0("Data/Chla/", chl_files[k])
  ncChlBrick <- brick(ncFileName)
  surveyChl <- extract(ncChlBrick, cbind(gom_coords$Longitude, gom_coords$Latitude))
  chl_tmp_df <- data.frame("Island"=gom_coords$Island, "Year"=substr(chl_files[k],2,5), "MMM_Chla"=surveyChl[,])
  chl.df <- rbind(chl.df, chl_tmp_df)
}

# summarize max chl-a value by island and bleaching year
chla_max <- chl.df %>%
  group_by(Island, Year) %>%
  summarize(MMM_Chla = max(MMM_Chla, na.rm=T))

# save data
write.csv(chla_max, "Data/GoM_chla_mmm_2010_2016.csv", row.names = F)
