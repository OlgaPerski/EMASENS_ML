# EMASENS_ML

Repository to accompany the paper (submitted): Supervised Machine Learning to Predict Smoking Lapses from Ecological Momentary Assessments and Sensor Data: Implications for Just-in-Time Adaptive Intervention Development

## Included data

`analytic_sample_1.rds` contains 6,124 observations of 98 variables.

`analytic_sample_2.rds` contains 4,839 observations of 188 variables.

## Included scripts

All scripts for data cleaning and pre-processing have been included for transparency. For ethical reasons, however, the raw data are not included. Data analysis that is reproducible starts at script `18_ML_analyses_group_no_sensor.R` which reads in `analytic_sample_1.rds`. Prior to running this, please run `1_load_packages.R`
