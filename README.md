# APCAPS Project
[![Dependencies RStudio](https://github.com/MarianaGlezU/APCAPS-project/actions/workflows/r.yml/badge.svg)](https://github.com/MarianaGlezU/APCAPS-project/actions/workflows/r.yml)
## Overview
Understanding the impact of cardiometabolic diseases and diagnosis status on mental health in rural India is crucial for estimating the burden of such diseases. This research will explore the relationship between cardiometabolic diseases on whether the risk of developing depression is affected by diagnosis status. We hypothesize that people with diagnosed cardiometabolic disease have a higher incidence of depression compared to those with disease based on screening or without either condition.

I analyzed data of 3233 participants from [”Andhra Pradesh Children and Parents Study”](https://www.lshtm.ac.uk/research/centres-projects-groups/apcaps), a prospective inter-generational ongoing cohort research, from the 3rd (2012-12)  and 4th  follow-up (2020-22) waves. The incidence risk of depression in different diagnosis status groups was calculated. Logistic regression analysis was conducted to examine the association between diagnosis status and the risk of developing depression, while controlling for potential confounders (age, sex, occupation, education, marital status, socioeconomic status, alcohol, and tobacco consumption, sleeping patters, and dietary habits).

## Analysis plan
The structure of the analysis starts by selecting the exposure and covariates from the 3rd cohort dataset. They first had to be recoded as new variables, according to their definition and/or parameters previously selected for this research question. Secondly, I created the outcome variable of depression from the 4th cohort dataset, after the PHQ-9 questionnaire (where there is a minimum score of zero and a maximum score of 27). For the purpose of this research, I use a cut-off score of 10 as it has a sensitivity of 87.1% and specificity of 79.7% for detecting depression among Indian population. 
I merged the two datasets for a complete case analysis for a 10-year period comparative analysis. Starting with a descriptive analysis of the population, followed by a univariate logistic regression, and ending with a multivariate logistic regression with a forward selection approach.

## Project Description
* Descriptive Multivar Analysis: 
 In this folder you can find the step by step analysis of the project. Starting by merging both datasets by partiicpant ID, so only including
 participants who had been present in both cohort (2010-12 and 2020-22) and who had no missing information as it was a complete case analysis.
 A univariate analysis was first performed prior to a stepwise multivariate logistic model. 
* Exposure Variables:
 Only using the third cohort, I chose, regrouped and created the exposure variables for the project along with the covariates. 
* Outcome Variables
 Only using questions from the PHQ-9 (Patient Health Questionnaire) in the fourth cohort and creating a new variable to determine if participants had (or not) 
 depression after a cut-off of 10. 
* APCAPS Data Procesing Package:
 This package was created specfically for this project but it can also be used for managing other projects using apcaps datasets. It can be used to build 
 data pipleines for other apcaps projects.
  
## Prerequisites
To run the projects the following libraries are required:
```
library(skimr)
library(janitor)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(haven)
library(sjPlot)
library(gtsummary)
library(devtools)
```

## Installation: APCAPS Data Processing Package

To install the Data Processing Package to build data pipelines you can use RStudio dev tools, running the following command:
```
install_github("MarianaGlezU/APCAPS-project/package_apcaps")
```
## Analysis variables
Provide context for variables and their relevance in the analysis:

|Variable | Description|
|:-------------|:-------------|
| `diagnosis_status`                           |Categorizes population into diagnosed, undiagnosed or neither regarding cardiometabolic disease
| `mayor_depression`                      |Participants who reported having a score ≥ 10 in the PHQ-9 Depression Test questionnaire.
| `ageGroups`                     |Created with DOB and divided into (18-44, 45-64 and 65+)
| `sex`                    |Cemale or male
| `occupation_category`                    |Student/training, housework, employed and unemployed
| `edu_level`                          |None (no formal education and illiterate), primary, secondary, and higher education
| `maritalStatus_Cat`                    |Never married, currently married, and widowed/divorce or separated
| `tertilesofNSLI`                |The Standard of Living Index was grouped into tertiles, to measure socioeconomic level
| `alcohol_consump`            |Combining frequency intake of  beer, wine, local and branded spirits considering if ever they had as positive
| `smokeYN`                    |If ever  smoked tobacco on regular basis; former (<6 months) and current were categorized as yes and never as no
| `sleep_cat`                    |Hours a day they slept on average, <6, 6-8 or >8 hours
| `intake_category` |Average consumption, portion frequency, and frequency of consumption per every fruit (16) and vegetable (24) in the FFQ.  

## License

This project is licensed under the MIT License. See the [LICENSE](https://github.com/git/git-scm.com/blob/main/MIT-LICENSE.txt) file for more details.

## Contributing

Contributions are welcome! If you have any suggestions or improvements, feel free to submit a pull request or open an issue.

Steps to Contribute:
* Fork the repository.
* Clone your fork to your local machine.
* Create a new branch for your feature or bug fix.
* Make your changes and commit them with descriptive messages.
* Push your changes to your fork.
* Open a pull request on the main repository.
