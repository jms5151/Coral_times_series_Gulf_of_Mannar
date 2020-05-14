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

# aggregate by island & year
island_year_df <- composition[,c(3,21,5:20)]
island_year_df <- island_year_df %>%
  group_by(group, Island, Year) %>%
  summarize_all(funs(mean))

# make community matrix - extract columns with abundance information
com = island_year_df[,4:18]
m_com = as.matrix(com)

# visualize data -------------------------------------------
genera_comp_NMDS = metaMDS(m_com)
stressplot(genera_comp_NMDS)

treat = island_year_df$group
ordiplot(genera_comp_NMDS)
ordihull(genera_comp_NMDS, groups = treat, draw = "polygon", col = c("grey90", "lightblue", "orange"), label=F)
text(x=-0.14, y=0.4, label="2005-2009")
text(x=0, y=0.05, label="2010-2015")
text(x=0.08, y=-0.23, label="2016-2017")

# ordiplot(genera_comp_NMDS)
# ordiellipse(genera_comp_NMDS,groups=treat,draw="polygon",col=c("grey90", "lightblue", "orange"),label=F)

# ANOSIM ----------------------------------------------------
# https://jkzorz.github.io/2019/06/11/ANOSIM-test.html
ano = anosim(m_com, grouping=island_year_df$group, distance = "bray", permutations = 9999)
ano
# ANOSIM statistic R:  0.19 
# Significance: 1e-04

# Indicator species analysis --------------------------------
# https://jkzorz.github.io/2019/07/02/Indicator-species-analysis.html
inv = multipatt(com, island_year_df$group, func = "r.g", control = how(nperm=9999))
summary(inv)

# visualize results
sub_pc = island_year_df[,c("group", "Soft.coral", "ACE", "Algae", "CCA", "CS", "CB", "ACT", "ACD", "CE", "CF", "ACB", "CM")]
sub_pc_m = melt(sub_pc, id = "group")

ggplot(sub_pc_m, aes(x = variable, y = value, fill = as.factor(group))) + 
  geom_boxplot(colour = "black", position = position_dodge(0.5)) +
  geom_vline(xintercept = c(1.5,5.5), colour = "grey85", size = 1.2) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 10, face = "bold"), legend.position = "right",
        axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
        axis.text.y = element_text(face = "bold", size = 12, colour = "black"), 
        axis.title.y = element_text(face = "bold", size = 14, colour = "black"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        legend.key=element_blank()) + 
  labs(x= "", y = "Relative Abundance (%)", fill = "group") + 
  scale_fill_manual(values = c("grey90", "lightblue", "orange")) #+
  # geom_text(x=0.95, y=70, label="2005-2009") +
  # geom_text(x=3.5, y=70, label="2016-2017") +
  # geom_text(x=9, y=70, label="2005-2015") 
  
