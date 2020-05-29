# Codes to analyze coral community composition and health in response to two bleaching events in the Gulf of Mannar, India.

# Data preparation
remotely_sensed_data_extraction.R : extract SST and Chl-a data for each island and bleaching year <br>
concat_covariates.R : concatenate and format covariates for regression models <br>
format_response_vars.R : format benthic composition data to calculate change in cover of different benthic groups <br>

# Analyses
GoM_community_analyses.R : conduct community composition analyses (ANOSIM and Indicator Species Analysis) and visualize data  <br>
GoM_regression_models.R : create beta regression models to identify environmental drivers of change in cover of different benthic groups
