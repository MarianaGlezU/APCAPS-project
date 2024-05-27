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


setwd("/Users/marianagonzalez/Desktop/Code")

# Read in dataset
library(haven)
apcaps_fu4_mariana <- read_dta("apcaps_fu4_mariana2.dta")
View(apcaps_fu4_mariana)
##NEW DATA BASE: ONLY WITH VARIABLES NEEDED FROM FU4##
new_FU4 <-data.frame(apcaps_fu4_mariana$participantid,
                     apcaps_fu4_mariana$depression_little_interest, apcaps_fu4_mariana$depression_better_dead_hurtingse,
                     apcaps_fu4_mariana$depression_failure_yourself, apcaps_fu4_mariana$depression_feeling_depressed,
                     apcaps_fu4_mariana$depression_feeling_tired, apcaps_fu4_mariana$depression_moving_slowly_fidgety, 
                     apcaps_fu4_mariana$depression_poor_apetite_overeati, apcaps_fu4_mariana$depression_trouble_concentrating,
                     apcaps_fu4_mariana$depression_trouble_sleeping, apcaps_fu4_mariana$anxiety_depression,
                     apcaps_fu4_mariana$fbs)

View(new_FU4)

new_FU4 <-new_FU4%>%rename(participantID=apcaps_fu4_mariana.participantid,
                           depression_little_interest=apcaps_fu4_mariana.depression_little_interest,
                           depression_feeling_depressed=apcaps_fu4_mariana.depression_feeling_depressed, 
                           depression_trouble_sleeping=apcaps_fu4_mariana.depression_trouble_sleeping,
                           depression_feeling_tired=apcaps_fu4_mariana.depression_feeling_tired,
                           depression_poor_apetite_overeati=apcaps_fu4_mariana.depression_poor_apetite_overeati,
                           depression_failure_yourself=apcaps_fu4_mariana.depression_failure_yourself,
                           depression_trouble_concentrating=apcaps_fu4_mariana.depression_trouble_concentrating,
                           depression_moving_slowly_fidgety=apcaps_fu4_mariana.depression_moving_slowly_fidgety,
                           depression_better_dead_hurtingse=apcaps_fu4_mariana.depression_better_dead_hurtingse,
                           healthliteracy_anxiety_depression=apcaps_fu4_mariana.anxiety_depression,
                           fbs_FU4=apcaps_fu4_mariana.fbs)
View(new_FU4)

new_FU4$depression_better_dead_hurtingse <- as.numeric(new_FU4$depression_better_dead_hurtingse) 
new_FU4$depression_little_interest <- as.numeric(new_FU4$depression_little_interest)
new_FU4$depression_failure_yourself <- as.numeric(new_FU4$depression_failure_yourself)
new_FU4$depression_feeling_depressed <- as.numeric(new_FU4$depression_feeling_depressed)
new_FU4$depression_feeling_tired <- as.numeric(new_FU4$depression_feeling_tired)
new_FU4$depression_moving_slowly_fidgety <- as.numeric(new_FU4$depression_moving_slowly_fidgety)
new_FU4$depression_poor_apetite_overeati <- as.numeric(new_FU4$depression_poor_apetite_overeati)
new_FU4$depression_trouble_concentrating <- as.numeric(new_FU4$depression_trouble_concentrating)
new_FU4$depression_trouble_sleeping <- as.numeric(new_FU4$depression_trouble_sleeping)

##### OUTCOME VARIABLES: DEPRESSION ####

new_FU4$depression_little_interest <- ifelse(new_FU4$depression_little_interest == "4",3,
                                             ifelse(new_FU4$depression_little_interest == "3",2,
                                                    ifelse(new_FU4$depression_little_interest == "2",1,
                                                           ifelse(new_FU4$depression_little_interest == "1",0, NA))))

new_FU4$depression_failure_yourself <- ifelse(new_FU4$depression_failure_yourself == "4",3,
                                              ifelse(new_FU4$depression_failure_yourself == "3",2,
                                                     ifelse(new_FU4$depression_failure_yourself == "2",1,
                                                            ifelse(new_FU4$depression_failure_yourself == "1",0, NA))))

new_FU4$depression_feeling_depressed <- ifelse(new_FU4$depression_feeling_depressed == "4",3,
                                               ifelse(new_FU4$depression_feeling_depressed == "3",2,
                                                      ifelse(new_FU4$depression_feeling_depressed == "2",1,
                                                             ifelse(new_FU4$depression_feeling_depressed == "1",0, NA))))

new_FU4$depression_feeling_tired <- ifelse(new_FU4$depression_feeling_tired == "4",3,
                                           ifelse(new_FU4$depression_feeling_tired == "3",2,
                                                  ifelse(new_FU4$depression_feeling_tired == "2",1,
                                                         ifelse(new_FU4$depression_feeling_tired == "1",0, NA))))

new_FU4$depression_moving_slowly_fidgety <- ifelse(new_FU4$depression_moving_slowly_fidgety == "4",3,
                                                   ifelse(new_FU4$depression_moving_slowly_fidgety == "3",2,
                                                          ifelse(new_FU4$depression_moving_slowly_fidgety == "2",1,
                                                                 ifelse(new_FU4$depression_moving_slowly_fidgety == "1",0, NA))))

new_FU4$depression_poor_apetite_overeati <- ifelse(new_FU4$depression_poor_apetite_overeati == "4",3,
                                                   ifelse(new_FU4$depression_poor_apetite_overeati == "3",2,
                                                          ifelse(new_FU4$depression_poor_apetite_overeati == "2",1,
                                                                 ifelse(new_FU4$depression_poor_apetite_overeati == "1",0, NA))))

new_FU4$depression_trouble_concentrating <- ifelse(new_FU4$depression_trouble_concentrating == "4",3,
                                                   ifelse(new_FU4$depression_trouble_concentrating == "3",2,
                                                          ifelse(new_FU4$depression_trouble_concentrating == "2",1,
                                                                 ifelse(new_FU4$depression_trouble_concentrating == "1",0, NA))))

new_FU4$depression_trouble_sleeping <- ifelse(new_FU4$depression_trouble_sleeping == "4",3,
                                              ifelse(new_FU4$depression_trouble_sleeping == "3",2,
                                                     ifelse(new_FU4$depression_trouble_sleeping == "2",1,
                                                            ifelse(new_FU4$depression_trouble_sleeping == "1",0, NA))))

new_FU4$depression_Score <- new_FU4$depression_little_interest + new_FU4$depression_better_dead_hurtingse + 
  new_FU4$depression_failure_yourself +
  new_FU4$depression_feeling_depressed + new_FU4$depression_feeling_tired + 
  new_FU4$depression_moving_slowly_fidgety + new_FU4$depression_poor_apetite_overeati +
  new_FU4$depression_trouble_concentrating + new_FU4$depression_trouble_sleeping

new_FU4$depression_cat <- cut(new_FU4$depression_Score, c(0,4,9,14,19,Inf),
                              include.lowest = TRUE, right = FALSE,
                              labels = c("None","Mild","Moderate","Moderately severe","Severe"))
table(new_FU4$depression_cat, useNA = "ifany")


new_FU4$mayor_depression <- ifelse(new_FU4$depression_Score >= 10, "Yes", "No")
table(new_FU4$mayor_depression, useNA = "ifany")

new_FU4$depression_any <- ifelse(new_FU4$depression_Score >= 5, "Yes","No")
table(new_FU4$depression_any, useNA = "ifany")

new_FU4$depression_any9 <- ifelse(new_FU4$depression_better_dead_hurtingse != 0 | 
                                    new_FU4$depression_Score >= 5, "Yes", "No")
table(new_FU4$depression_any9, useNA = "ifany")

fu4_complete <- subset(new_FU4, select = c("participantID",
                                           "depression_Score", "depression_cat","mayor_depression",
                                           "depression_any",
                                           "depression_little_interest",
                                           "depression_better_dead_hurtingse",
                                           "depression_failure_yourself",
                                           "depression_feeling_depressed",
                                           "depression_feeling_tired",
                                           "depression_moving_slowly_fidgety",
                                           "depression_poor_apetite_overeati",
                                           "depression_trouble_concentrating",
                                           "depression_trouble_sleeping"))

write.csv(fu4_complete, file = "fu4_complete.csv", row.names = FALSE)

