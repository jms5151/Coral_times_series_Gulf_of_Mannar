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

# PERMANOVA - comparing coral communities among time blocks within islands
perm.df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(perm.df) <- c("Island", "factor", "DF", "SumOSqs", "R2", "F", "PrF")
filename <- "Results/PERMANOVA_results.csv"
write.csv(perm.df, filename, row.names = F)

for (j in islands){
  xdf <- subset(composition, Island == j)
  com = xdf[,coral_types]
  m_com = as.matrix(com)
  distMat <- vegdist(m_com, method="bray")
  x = adonis2(distMat ~ group + Site_Number/LIT_Number, data = xdf)
  perm.tmp <- data.frame(j, c("group", "site", "site:lit", "residual", "total"), x$Df, x$SumOfSqs, x$R2, x$F, x$`Pr(>F)`)
  write.table(perm.tmp, file = filename, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}

# Indicator species analysis by island
# https://jkzorz.github.io/2019/07/02/Indicator-species-analysis.html
inv.df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(inv.df) <- c("s.2005-2009", "s.2010-2015", "s.2016-2017", "index", "stat", "p.value", "Coral_type", "Island")
fileName <- paste0("Results/ISA_coral_types.csv")
write.csv(inv.df, fileName, row.names = F)

for (k in islands){
  xdf <- subset(composition, Island == k)
  com = xdf[,coral_types]
  inv = multipatt(com, xdf$group, func = "r.g", control = how(nperm = 9999))
  inv.df <- as.data.frame(inv$sign, colnames = TRUE)
  inv.df$Coral_type <- row.names(inv.df)
  inv.df$Island <- k
  write.table(inv.df, file = fileName, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}

