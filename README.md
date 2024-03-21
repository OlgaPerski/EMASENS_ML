# EMASENS_ML

Repository to accompany the paper: "Supervised Machine Learning to Predict Smoking Lapses from Ecological Momentary Assessments and Sensor Data: Implications for Just-in-Time Adaptive Intervention Development"

## Included data

`analytic_sample_1.rds` contains 6,124 observations of 98 variables.

`analytic_sample_2.rds` contains 4,839 observations of 188 variables.

## Included scripts

All scripts for data cleaning and pre-processing have been included for transparency. For ethical reasons, however, the raw data are not included. Data analysis that is reproducible starts at script `18_ML_analyses_group_no_sensor.R` which reads in `analytic_sample_1.rds`. Prior to running this, please run `1_load_packages.R`

`1_load_packages.R` loads the packages required for the analyses.

`18_ML_analyses_group_no_sensor.R` trains and tests the group-level models (does not include the sensor data).

`19_ML_analyses_group_individual_no_sensor.R` trains on the group minus the individual of interest and is subsequently tested on the individual of interest (loops through all individuals with sufficient data; does not include the sensor data).

`20_ML_analyses_individual_no_sensor.R` trains on a proportion of each individual's data and is subsequently tested on the remaining proportion of the individual's data (loops through all individuals with sufficient data; does not include the sensor data).

`21_ML_analyses_hybrid_no_sensor.R` trains on the group plus a proportion of each individual's data and is subsequently tested on the remaining proportion of the individual's data (loops through all individuals with sufficient data; does not include the sensor data).

`22_ML_analyses_group_sensor.R` trains and tests the group-level models (includes the sensor data).

`23_ML_analyses_group_individual_sensor.R` trains on the group minus the individual of interest and is subsequently tested on the individual of interest (loops through all individuals with sufficient data; includes the sensor data).

`24_ML_analyses_individual_sensor.R` trains on a proportion of each individual's data and is subsequently tested on the remaining proportion of the individual's data (loops through all individuals with sufficient data; includes the sensor data).

`25_ML_analyses_hybrid_sensor.R` trains on the group plus a proportion of each individual's data and is subsequently tested on the remaining proportion of the individual's data (loops through all individuals with sufficient data; includes the sensor data).

`26_ML_sensitivity_analyses_group.R` trains and tests the group-level models (does not include the sensor data and uses the alternative outcome variable - cravings).

`27_ML_sensitivity_analyses_group_individual.R` trains on the group minus the individual of interest and is subsequently tested on the individual of interest (loops through all individuals with sufficient data; does not include the sensor data and uses the alternative outcome variable - cravings).

`28_ML_sensitivity_analyses_individual.R` trains on a proportion of each individual's data and is subsequently tested on the remaining proportion of the individual's data (loops through all individuals with sufficient data; does not include the sensor data and uses the alternative outcome variable - cravings).

`29_ML_sensitivity_analyses_hybrid.R` trains on the group plus a proportion of each individual's data and is subsequently tested on the remaining proportion of the individual's data (loops through all individuals with sufficient data; does not include the sensor data and uses the alternative outcome variable - cravings).
