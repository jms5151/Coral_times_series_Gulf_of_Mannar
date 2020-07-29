# community analyses --------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(ggplot2)
library(tidyverse)
library(vegan)
library(indicspecies)
library(reshape2)

# load data
composition <- read.csv("Data/GoM_data_2005_to_2017_Checked.csv", head = T, stringsAsFactors = F)

# by grouped time period
composition$group <- ifelse(composition$Year <= 2009, "2005-2009", NA)
composition$group <- ifelse(composition$Year > 2009 & composition$Year <=2015, "2010-2015", composition$group)
composition$group <- ifelse(composition$Year > 2015, "2016-2017", composition$group)

# NMDS plots by island 
coral_types <- c("ACB", "ACT", "ACD", "ACF", "ACE", "CM", "CS", "CB", "CF", "CE")
islands <- unique(composition$Island)

par(mfrow = c(5, 5), mar=c(2,2,2,2))

for (i in islands){
  xdf <- subset(composition, Island == i)
  com = xdf[,coral_types]
  m_com = as.matrix(com)
  genera_comp_NMDS = metaMDS(m_com)
  treat = xdf$group
  ordiplot(genera_comp_NMDS, main = i, xlim=c(-1.5, 1.5), ylim = c(-1, 1), xlab = "", ylab = "")
  ordihull(genera_comp_NMDS, groups = treat, draw = "polygon", col = c("grey90", "lightblue", "orange"), label=F)
}

# ANOSIM - comparing coral communities among time blocks within islands
ano.df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(ano.df) <- c("Island", "Anosim_R", "Significance")
write.csv(ano.df, "Results/Anosim_results.csv", row.names = F)

for (j in islands){
  xdf <- subset(composition, Island == j)
  com = xdf[,coral_types]
  m_com = as.matrix(com)
  ano = anosim(m_com, grouping=xdf$group, distance = "bray", permutations = 9999)
  ano.tmp <- data.frame(j, round(ano$statistic, 4), ano$signif)
  write.table(ano.tmp, file = "Results/Anosim_results.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}
