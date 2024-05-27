# APCAPS Project
[![Dependencies RStudio](https://github.com/MarianaGlezU/APCAPS-project/blob/main/.github/workflows/r.yml/badge.svg)](https://github.com/MarianaGlezU/APCAPS-project/blob/main/.github/workflows/r.yml)
## Overview
Understanding the impact of cardiometabolic diseases and diagnosis status on mental health in rural India is crucial for estimating the burden of such diseases. This research will explore the relationship between cardiometabolic diseases on whether the risk of developing depression is affected by diagnosis status. We hypothesize that people with diagnosed cardiometabolic disease have a higher incidence of depression compared to those with disease based on screening or without either condition.

I analyzed data of 3233 participants from [”Andhra Pradesh Children and Parents Study”](https://www.lshtm.ac.uk/research/centres-projects-groups/apcaps), a prospective inter-generational ongoing cohort research, from the 3rd (2012-12)  and 4th  follow-up (2020-22) waves. The incidence risk of depression in different diagnosis status groups was calculated. Logistic regression analysis was conducted to examine the association between diagnosis status and the risk of developing depression, while controlling for potential confounders (age, sex, occupation, education, marital status, socioeconomic status, alcohol, and tobacco consumption, sleeping patters, and dietary habits).

## Analysis plan
The structure of the analysis starts by selecting the exposure and covariates from the 3rd cohort dataset. They first had to be recoded as new variables, according to their definition and/or parameters previously selected for this research question. Secondly, I created the outcome variable of depression from the 4th cohort dataset, after the PHQ-9 questionnaire (where there is a minimum score of zero and a maximum score of 27). For the purpose of this research, I use a cut-off score of 10 as it has a sensitivity of 87.1% and specificity of 79.7% for detecting depression among Indian population. 
I merged the two datasets for a complete case analysis for a 10-year period comparative analysis. Starting with a descriptive analysis of the population, followed by a univariate logistic regression, and ending with a multivariate logistic regression with a forward selection approach.

## Project Description
* Descriptive Multivar Analysis: 
 
* Exposure Variables:

* Outcome Variables

  
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

 
