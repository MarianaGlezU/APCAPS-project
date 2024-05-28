#APCAPS Project package
#This package contains functions to build data processing pipelines for
# APCAPS datasets.

#To install see requirements on readme.


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#This function creates a new column based on the date of birth of participants.
#As analysis can be performed later in the research and not based on their
#current age.

DoB_to_Age <- function(dataframe,date_of_birth="Date_of_Birth", Age_from_DoB="Age_from_DoB", DoB_format="%Y-%m-%d") {
  dataframe[[Age_from_DoB]] <- as.Date(dataframe[[date_of_birth]], format = "%Y-%m-%d")
  dataframe[[Age_from_DoB]] <- as.integer(difftime(Sys.Date(), dataframe[[date_of_birth]], units = "days") / 365.25)
  return(dataframe)
}

#When talking about fasting glucose to determine if a participant can be
#diagnosed with diabetes (type 2) a cut of 126 is used. This functions allows
#that.
fbs_diabetes <- function(dataframe,fasting_glucose="Blood_glucose", fasting_blood_glucose="Fasting_blood_glucose") {
  dataframe[[fasting_blood_glucose]] <- ifelse(dataframe[[fasting_glucose]] >= 126, TRUE, FALSE)
  return(dataframe)
}

#portions of fruits and vegetables are frequently used as indicators of an
#adequate diet. This function uses average consumption, portion frequency,
#and frequency of consumption of every fruit and vegetable in the FFQ.
compute_greens_daily <- function(dataframe,averg_consump,consump_freq,total_freq,greens_daily_consump){
  dataframe[[greens_daily_consump]] <- datafram[[averg_consump]] * datafram[[consump_freq]] /
    (case_when(dataframe[[total_freq]] == 1 ~ 1,
               dataframe[[total_freq]] == 2 ~ 7,
               dataframe[[total_freq]] == 3 ~ 30,
               dataframe[[total_freq]] == 4 ~ 365))
  return(dataframe)
}

#For high blood pressure, in accordance with guidelines, the latter two
#readings (from 3) of systolic and diastolic blood pressure are used to
#determine the average reading.
avg_systolic <- function(dataframe,systolic_bp_2="systolicBP2", systolic_bp_3="systolicBP3",avg_systolic_bp) {
   dataframe[[avg_systolicBP]] <- ifelse(is.na(dataframe[[systolicBP2]]) | is.na(dataframe[[systolicBP3]]),
                                   NA,
                                   rowMeans(dataframe[, c(systolic_bp_2, systolic_bp_3)], na.rm = TRUE))
   return(dataframe)
}

avg_diastolic <- function(dataframe,diastolic_bp_2="diastolicBP2", diastolic_bp_3="diastolicBP3",avg_diastolic_bp) {
  dataframe[[avg_diastolicBP]] <- ifelse(is.na(dataframe[[diastolicBP2]]) | is.na(dataframe[[diastolicBP3]]),
                                        NA,
                                        rowMeans(dataframe[, c(diastolic_bp_2, diastolic_bp_3)], na.rm = TRUE))
  return(dataframe)
}

#Regarding the PHQ-9 (depression), each question is assigned a score ranging
#from zero (not at all) to three (nearly every day). The questionnaire has a
#total of 9 questions this function modified the scores ranging from 0-3
#instead of 1-4

phq9 <- function(dataframe, dep_little_int="depression_little_interest") {
  dataframe[[dep_little_int]] <- ifelse(dataframe[[dep_little_int]] == "4",3,
                                             ifelse(dataframe[[dep_little_int]] == "3",2,
                                                    ifelse(dataframe[[dep_little_int]] == "2",1,
                                                           ifelse(dataframe[[dep_little_int]] == "1",0, NA))))
  return(dataframe)
}



