# Analyze coral community composition and health in response to two bleaching events in the Gulf of Mannar, India.

This work supports the following study: 
Raj KD, Aeby GS, Mathews G, Williams GJ, Caldwell JM, Laju RL, Bharath MS, Kumar PD, Arasamuthu A, Asir NG, Wedding LM. Coral reef resilience differs among islands within the Gulf of Mannar, southeast India, following successive coral bleaching events. Coral Reefs. 2021 Aug;40(4):1029-44.
https://link.springer.com/article/10.1007/s00338-021-02102-0

# Data preparation
remotely_sensed_data_extraction.R : extract SST and Chl-a data for each island and bleaching year <br>
concat_covariates.R : concatenate and format covariates for regression models <br>
format_response_vars.R : format benthic composition data to calculate change in cover of different benthic groups <br>

# Analyses
GoM_anovas.R: conduct ANOVAs on overall benthic changes between 2005 and 2017 <br>
GoM_community_analyses.R : conduct community composition analyses (PERMANOVA and Indicator Species Analysis) and visualize data  <br>
GoM_regression_models.R : create beta regression models to identify environmental drivers of change in coral and algae cover  <br>
paired_t-tests.R : compare mean change in coral and algal cover due to two bleaching events
