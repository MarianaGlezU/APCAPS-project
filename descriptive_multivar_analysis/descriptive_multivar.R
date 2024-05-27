# Loading packages
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


setwd("/Users/marianagonzalez/Desktop/Code")
fu3_complete <- read_csv("fu3_complete1.csv")
fu4_complete <- read_csv("fu4_complete.csv")

########################  MERGE  ###################################
#Get unique list name
one_dataframe1 <- data.frame(union(fu3_complete$participantID, fu4_complete$participantID))
colnames(one_dataframe1)<-"participantID"
#Get full dataset
fu4_complete$participantID <- as.numeric(fu4_complete$participantID)
fu3_complete$participantID <- as.numeric(fu3_complete$participantID)
one_dataframe1$participantID <- as.numeric(one_dataframe1$participantID)
combined_data1<- one_dataframe1%>%full_join(fu3_complete,by="participantID")%>%
  full_join(fu4_complete,by="participantID")

fu3fu4_filtered1 <- combined_data1[complete.cases(combined_data1$tertilesofNSLI.x, combined_data1$diagnosis_status.x, combined_data1$sex.x, combined_data1$sleep_cat.x,
                                                 combined_data1$alcohol_consump.x, combined_data1$intake_category.x, combined_data1$mayor_depression), ]

# Save the merged data 
write.csv(fu3fu4_filtered1, file = "fu3fu4_filtered1.csv", row.names = FALSE)

##descriptive analysis####
tbl_summary_frequencies2 <-
  fu3fu4_filtered1 %>%
  select(dm_diagnosis.x, hta_diagnosis.x, dm_or_hta1.x,
         hta_screening2.x, dm_screening3.x, screening1,
         none_all.x) %>%
  tbl_summary(
    type = all_continuous2() ~ "continuous2",
    label = list(dm_diagnosis.x = "Previously been diagnosed with Diabetes",
                 hta_diagnosis.x = "Previously been diagnosed with Hypertension",
                 dm_or_hta1.x = "Previously been diagnosed with Diabetes or Hypertension",
                 hta_screening2.x = "Screened positive for Hypertension without previous diagnosis",
                 dm_screening3.x = "Screened positive for Diabetes without previous diagnosis",
                 screening1 = "Screened positive for Diabetes or Hypertension without previous diagnosis",
                 none_all.x = "No previous diagnosis and no screenign for Diabetes and Hypertension")
  )
View(tbl_summary_frequencies2)
##descriotive analysis with depression####
tbl_summary_depression <-
  fu3fu4_filtered1 %>%
  select(dm_diagnosis.x, hta_diagnosis.x, dm_or_hta1.x,
         hta_screening2.x, dm_screening3.x, screening1,
         none_all.x, mayor_depression) %>%
  tbl_summary(
    by = mayor_depression,
    type = all_continuous2() ~ "continuous2",
    label = list(dm_diagnosis.x = "Previously been diagnosed with Diabetes",
                 hta_diagnosis.x = "Previously been diagnosed with Hypertension",
                 dm_or_hta1.x = "Previously been diagnosed with Diabetes or Hypertension",
                 hta_screening2.x = "Screened positive for Hypertension without previous diagnosis",
                 dm_screening3.x = "Screened positive for Diabetes without previous diagnosis",
                 screening1 = "Screened positive for Diabetes or Hypertension without previous diagnosis",
                 none_all.x = "No previous diagnosis and no screenign for Diabetes and Hypertension")
  )
View(tbl_summary_depression)
tbl_summary_depression
##population####

tbl_uvreg_by_depression <- fu3fu4_filtered1 %>%
  select(
    occupation_category.x, sex.x, smokeYN.x, edu_level.x, tertilesofNSLI.x, ageGroups.x,
    maritalStatus_Cat.x, sleep_cat.x, alcohol_consump.x, intake_category.x, mayor_depression
  ) %>%
  tbl_summary(
    by = mayor_depression,
    type = all_continuous2() ~ "continuous2",
    label = list(
      occupation_category.x = "Occupation Category",
      sex.x = "Sex",
      smokeYN.x = "Smoking Status",
      edu_level.x = "Education Level",
      tertilesofNSLI.x = "NSLI Tertiles",
      ageGroups.x = "Age Groups",
      maritalStatus_Cat.x = "Marital Status",
      sleep_cat.x = "Sleep Category",
      alcohol_consump.x = "Alcohol Consumption",
      intake_category.x = "Intake Category"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test",
      all_categorical() ~ "chisq.test"
    ),
    test.args = list(
      all_tests("t.test") ~ list(var.equal = TRUE)
    )
  )

summary(tbl_uvreg_by_depression)
print(tbl_uvreg_by_depression)

####models ####
# Logistic Regression
fu3fu4_filtered1$diagnosed_group_recoded1 <- ifelse(fu3fu4_filtered1$diagnosed == "Not Diagnosed", 0, 1)
fu3fu4_filtered1$sex_recoded1 <- ifelse(fu3fu4_filtered1$sex.x == "Male", 0, 1)
fu3fu4_filtered1$depression_mayor_recoded1 <- ifelse(fu3fu4_filtered1$mayor_depression == "No", 0, 1)
fu3fu4_filtered1$undiagnosed_group_recoded1 <- ifelse(fu3fu4_filtered1$undiagnosed == "Not undiagnosed", 0, 1)
fu3fu4_filtered1$without_group_recoded1 <- ifelse(fu3fu4_filtered1$none_all.x == "Without either", 1, 0)


# Convert diagnosis_status to a factor
fu3fu4_filtered1$diagnosis_status.x <- factor(fu3fu4_filtered1$diagnosis_status.x, levels = c("Undiagnosed", "Diagnosed","Neither"))

# Set "neither" as the baseline category
fu3fu4_filtered1$diagnosis_status.x <- relevel(fu3fu4_filtered1$diagnosis_status.x, ref = "Neither")


model1_crude <- glm(depression_mayor_recoded1 ~ diagnosis_status.x, family = binomial(
  link = "logit"),
  data = fu3fu4_filtered1) 
summary(model1_crude)
exp(coefficients(model1_crude))
exp(confint(model1_crude))

# Fit the logistic regression model with the updated baseline category
model10_crude <- glm(depression_mayor_recoded1 ~ diagnosis_status.x, family = binomial(link = "logit"), data = fu3fu4_filtered1)
summary(model10_crude)
exp(coefficients(model10_crude))
exp(confint(model10_crude))

model2_age_sex <- glm(depression_mayor_recoded1 ~ diagnosis_status.x + ageGroups.x + sex.x, family = binomial(
  link = "logit"),
  data = fu3fu4_filtered1) 
summary(model2_age_sex)
exp(coefficients(model2_age_sex))
exp(confint(model2_age_sex))

#"maritalStatus_Cat", "occupation_category", "smokeYN", "edu_level", "tertilesofNSLI",
#"sleep_cat", "alcohol_consump", "intake_category", "sex", "ageGroups"

model3_age_sex_sociodemo <- glm(depression_mayor_recoded1 ~ diagnosis_status.x + ageGroups.x + sex.x +
                                  edu_level.x + occupation_category.x + maritalStatus_Cat.x + tertilesofNSLI.x, family = binomial(
                                    link = "logit"),
                                data = fu3fu4_filtered1) 
summary(model3_age_sex_sociodemo)
exp(coefficients(model3_age_sex_sociodemo))
exp(confint(model3_age_sex_sociodemo))

model4_age_sex_sociodema_behb <- glm(depression_mayor_recoded1 ~ diagnosis_status.x + ageGroups.x + sex.x + tertilesofNSLI.x +
                                       edu_level.x + occupation_category.x + maritalStatus_Cat.x+
                                       smokeYN.x + alcohol_consump.x + intake_category.x + sleep_cat.x, 
                                     family = binomial(
                                       link = "logit"),
                                     data = fu3fu4_filtered1) 
summary(model4_age_sex_sociodema_behb)
exp(coefficients(model4_age_sex_sociodema_behb))
exp(confint(model4_age_sex_sociodema_behb))


model4_age_sex_sociodema_behb %>%
  model_parameters(exponentiate = TRUE) %>%
  plot()



