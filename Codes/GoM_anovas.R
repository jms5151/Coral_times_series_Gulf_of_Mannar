# load data
composition <- read.csv("Data/GoM_data_2005_to_2017_Checked.csv", head = T)
coral_types <- c("ACB", "ACT", "ACD", "ACF", "ACE", "CM", "CS", "CB", "CF", "CE")

# calculate total coral cover
composition$total_ccov <- rowSums(composition[,coral_types])

# subset by year and merge together
y05 <- composition[,c("Island", "Site_Number", "LIT_Number", "Year", "total_ccov", "Algae")] %>% filter(Year == 2005) 
y17 <- composition[,c("Island", "Site_Number", "LIT_Number", "Year", "total_ccov", "Algae")] %>% filter(Year == 2017) 

colnames(y05)[4:6] <- paste0(colnames(y05)[4:6], "_05")
colnames(y17)[4:6] <- paste0(colnames(y17)[4:6], "_17")

x <- merge(y05, y17, by = c("LIT_Number", "Site_Number", "Island"))

# calculate differences between 2005 and 2017
x$ccov_diff <- x$total_ccov_17 - x$total_ccov_05
x$algae_diff <- x$Algae_17 - x$Algae_05

# conduct ANOVAs
cor_aov <- aov(ccov_diff ~ Island, data = x)) # if nesting, /Site_Number/LIT_Number
summary(cor_aov)

alg_aov <- aov(algae_diff ~ Island, data = x) # if nesting, /Site_Number/LIT_Number
summary(alg_aov)

# Tukeys post-hoc test to look at pairwise difference among groups
coral_tukey <- TukeyHSD(x=cor_aov, x$Island, conf.level=0.95)
algae_tukey <- TukeyHSD(x=alg_aov, x$Island, conf.level=0.95)
