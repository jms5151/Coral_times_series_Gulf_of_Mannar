# Codes to analyze coral community composition and health in response to two bleaching  in the Gulf of Mannar, India.

# Data preparation
remotely_sensed_data_extraction.R : extract SST and Chl-a data for each island and bleaching year
concat_covariates.R : concatenate and format covariates for regression models
format_response_vars.R : format benthic composition data to calculate change in cover of different benthic groups

# Analyses
GoM_community_analyses.R : conduct community composition analyses (ANOSIM and Indicator Species Analysis) and visualize data 
GoM_regression_models.R : create beta regression models to identify environmental drivers of change in cover of different benthic groups
