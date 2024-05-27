library(tidyverse)
library(skimr)
library(janitor)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sjPlot)


# Change working directory
setwd("/Users/marianagonzalez/Desktop/Code")

# Read in dataset
library(haven)
FU3_mariana <- read_dta("3FU_mariana copy.dta")
View(FU3_mariana)
## EXPOSURE + COVARIATES ####
new_FU3 <-data.frame(FU3_mariana$ha1id, FU3_mariana$ha1q5_5sex, FU3_mariana$ha1dv_dob, FU3_mariana$nsli2,
                     FU3_mariana$ha1q5_7marital_status, FU3_mariana$ha1q5_11aocc_resp,
                     FU3_mariana$ha1q5_13aedu_resp, FU3_mariana$ha1q8_1a1smoke, FU3_mariana$slic2,
                     FU3_mariana$ha1q10_1sleep_work, FU3_mariana$pal, FU3_mariana$pal_cat,
                     FU3_mariana$ha1q8_4a_alc_loc_spirits,FU3_mariana$ha1q30_2abp_high,
                     FU3_mariana$ha1q30_2bbp_age, FU3_mariana$ha1q30_2adiabetes, FU3_mariana$ha1q30_3bdiab_age,
                     FU3_mariana$ha1q30_9adepression, FU3_mariana$ha1q26_2asbp1,
                     FU3_mariana$ha1q26_2bsbp2, FU3_mariana$ha1q26_2csbp3, FU3_mariana$ha1q26_3adbp1,
                     FU3_mariana$ha1q26_3bdbp2, FU3_mariana$ha1q26_3cdbp3, FU3_mariana$ha1q8_4d_alc_wine, FU3_mariana$ha1q8_4c_alc_beer,
                     FU3_mariana$ha1br_fastingglucosemgdl, FU3_mariana$nsli2, FU3_mariana$ha1q8_4b_alc_bra_spirits,
                     FU3_mariana$ha1q19_1abanana, FU3_mariana$ha1q19_1b1banana, FU3_mariana$ha1q19_1b2banana,
                     FU3_mariana$ha1q19_2aapple, FU3_mariana$ha1q19_2b1apple, FU3_mariana$ha1q19_2b2apple,
                     FU3_mariana$ha1q19_3aorange, FU3_mariana$ha1q19_3b1orange, FU3_mariana$ha1q19_3b2orange,
                     FU3_mariana$ha1q19_4amango, FU3_mariana$ha1q19_4b1mango, FU3_mariana$ha1q19_4b2mango,
                     FU3_mariana$ha1q19_5aguava, FU3_mariana$ha1q19_5b1guava, FU3_mariana$ha1q19_5b2guava, 
                     FU3_mariana$ha1q19_6agrapes, FU3_mariana$ha1q19_6b1grapes, FU3_mariana$ha1q19_6b2grapes,
                     FU3_mariana$ha1q19_7apineapple, FU3_mariana$ha1q19_7b1pineapple, FU3_mariana$ha1q19_7b2pineapple,
                     FU3_mariana$ha1q19_8apapaya, FU3_mariana$ha1q19_8b1papaya, FU3_mariana$ha1q19_8b2papaya,
                     FU3_mariana$ha1q19_9apomeg, FU3_mariana$ha1q19_9b1pomeg, FU3_mariana$ha1q19_9b2pomeg,
                     FU3_mariana$ha1q19_10asapota, FU3_mariana$ha1q19_10b1sapota, FU3_mariana$ha1q19_10b2sapota,
                     FU3_mariana$ha1q19_11awmelon, FU3_mariana$ha1q19_11b1wmelon, FU3_mariana$ha1q19_11b2wmelon,
                     FU3_mariana$ha1q19_12amusk_melon, FU3_mariana$ha1q19_12b1musk_melon, FU3_mariana$ha1q19_12b2musk_melon,
                     FU3_mariana$ha1q19_13acus_apple, FU3_mariana$ha1q19_13b1cus_apple, FU3_mariana$ha1q19_13b2cus_apple,
                     FU3_mariana$ha1q19_14azizyphus, FU3_mariana$ha1q19_14b1zizyphus, FU3_mariana$ha1q19_14b2zizyphus,
                     FU3_mariana$ha1q19_15asugarcane, FU3_mariana$ha1q19_15b1sugarcane, FU3_mariana$ha1q19_15b2sugarcane,
                     FU3_mariana$ha1q19_16apalmyra, FU3_mariana$ha1q19_16b1palmyra, FU3_mariana$ha1q19_16b2palmyra,
                     FU3_mariana$ha1q20_1apalak, FU3_mariana$ha1q20_1b1palak, FU3_mariana$ha1q20_1b2palak,
                     FU3_mariana$ha1q20_2apotato, FU3_mariana$ha1q20_2b1potato, FU3_mariana$ha1q20_2b2potato,
                     FU3_mariana$ha1q20_3abeetroot, FU3_mariana$ha1q20_3b1beetroot, FU3_mariana$ha1q20_3b2beetroot,
                     FU3_mariana$ha1q20_4acabbage, FU3_mariana$ha1q20_4b1cabbage, FU3_mariana$ha1q20_4b2cabbage,
                     FU3_mariana$ha1q20_5abeans, FU3_mariana$ha1q20_5b1beans, FU3_mariana$ha1q20_5b2beans,
                     FU3_mariana$ha1q20_6aladies, FU3_mariana$ha1q20_6b1ladies, FU3_mariana$ha1q20_6b2ladies,
                     FU3_mariana$ha1q20_7acaulifl, FU3_mariana$ha1q20_7b1caulifl, FU3_mariana$ha1q20_7b2caulifl,
                     FU3_mariana$ha1q20_8abgourd, FU3_mariana$ha1q20_8b1bgourd, FU3_mariana$ha1q20_8b2bgourd,
                     FU3_mariana$ha1q20_9abrinjal, FU3_mariana$ha1q20_9b1brinjal, FU3_mariana$ha1q20_9b2brinjal,
                     FU3_mariana$ha1q20_10akovai, FU3_mariana$ha1q20_10b1kovai, FU3_mariana$ha1q20_10b2kovai,
                     FU3_mariana$ha1q20_11acapsicum, FU3_mariana$ha1q20_11b1capsicum, FU3_mariana$ha1q20_11b2capsicum,
                     FU3_mariana$ha1q20_12adrumstick, FU3_mariana$ha1q20_12b1drumstick, FU3_mariana$ha1q20_12b2drumstick,
                     FU3_mariana$ha1q20_13aplantain, FU3_mariana$ha1q20_13b1plantain, FU3_mariana$ha1q20_13b2plantain,
                     FU3_mariana$ha1q20_14acolacasia, FU3_mariana$ha1q20_14b1colacasia, FU3_mariana$ha1q20_14b2colacasia, FU3_mariana$ha1dv_age)
View(new_FU3)

##RENAME VARIABLE####

new_FU3 <-new_FU3%>%rename(participantID = FU3_mariana.ha1id, sex = FU3_mariana.ha1q5_5sex, age = FU3_mariana.ha1dv_age, age_dob = FU3_mariana.ha1dv_dob,
                           maritalStatus = FU3_mariana.ha1q5_7marital_status, occupation = FU3_mariana.ha1q5_11aocc_resp,
                           education = FU3_mariana.ha1q5_13aedu_resp, smokeTobacco = FU3_mariana.ha1q8_1a1smoke, tertilesofNSLI = FU3_mariana.slic2,
                           hrsSleepPerDay = FU3_mariana.ha1q10_1sleep_work, phyactlevel = FU3_mariana.pal, nsli2 = FU3_mariana.nsli2,
                           phyactcat = FU3_mariana.pal_cat, localSpirits = FU3_mariana.ha1q8_4a_alc_loc_spirits,
                           dxHTA = FU3_mariana.ha1q30_2abp_high, ageDxHTA = FU3_mariana.ha1q30_2bbp_age,
                           dxDM = FU3_mariana.ha1q30_2adiabetes, ageDxDM = FU3_mariana.ha1q30_3bdiab_age,
                           dxDepression = FU3_mariana.ha1q30_9adepression, systoloicBP1 = FU3_mariana.ha1q26_2asbp1,
                           systoloicBP2 = FU3_mariana.ha1q26_2bsbp2, systoloicBP3 = FU3_mariana.ha1q26_2csbp3,
                           diastolicBP1 = FU3_mariana.ha1q26_3adbp1, diastolicBP2 = FU3_mariana.ha1q26_3bdbp2, 
                           diastolicBP3 = FU3_mariana.ha1q26_3cdbp3, fbs = FU3_mariana.ha1br_fastingglucosemgdl, 
                           nsli = FU3_mariana.nsli2, localsSpirits = FU3_mariana.ha1q8_4a_alc_loc_spirits, brandedSpirits = FU3_mariana.ha1q8_4b_alc_bra_spirits,
                           wine = FU3_mariana.ha1q8_4d_alc_wine, beer = FU3_mariana.ha1q8_4c_alc_beer,
                           banana_averg_consump = FU3_mariana.ha1q19_1abanana, banana_freq = FU3_mariana.ha1q19_1b1banana, banana_consump_freq = FU3_mariana.ha1q19_1b2banana,
                           apple_averg_consump = FU3_mariana.ha1q19_2aapple, apple_freq = FU3_mariana.ha1q19_2b1apple, apple_consump_freq = FU3_mariana.ha1q19_2b2apple,
                           orange_averg_consump = FU3_mariana.ha1q19_3aorange, orange_freq = FU3_mariana.ha1q19_3b1orange, orange_consump_freq = FU3_mariana.ha1q19_3b2orange,
                           mango_averg_consump = FU3_mariana.ha1q19_4amango, mango_freq = FU3_mariana.ha1q19_4b1mango, mango_consump_freq = FU3_mariana.ha1q19_4b2mango,
                           guava_averg_consump = FU3_mariana.ha1q19_5aguava, guava_freq = FU3_mariana.ha1q19_5b1guava, guava_consump_freq = FU3_mariana.ha1q19_5b2guava,
                           grapes_averg_comsump = FU3_mariana.ha1q19_6agrapes, grapes_freq = FU3_mariana.ha1q19_6b1grapes, grapes_consump_freq = FU3_mariana.ha1q19_6b2grapes,
                           pineapple_averg_consump = FU3_mariana.ha1q19_7apineapple, pineapple_freq = FU3_mariana.ha1q19_7b1pineapple, pineapple_consump_freq = FU3_mariana.ha1q19_7b2pineapple,
                           papaya_averag_consump = FU3_mariana.ha1q19_8apapaya, papaya_freq = FU3_mariana.ha1q19_8b1papaya, papaya_consump_freq = FU3_mariana.ha1q19_8b2papaya,
                           pomegranade_averg_consump = FU3_mariana.ha1q19_9apomeg, pomegranade_freq = FU3_mariana.ha1q19_9b1pomeg, pomegranada_consump_freq = FU3_mariana.ha1q19_9b2pomeg,
                           sapota_averg_consump = FU3_mariana.ha1q19_10asapota, sapota_freq = FU3_mariana.ha1q19_10b1sapota, sapota_consump_freq = FU3_mariana.ha1q19_10b2sapota,
                           watermelon_averg_consump = FU3_mariana.ha1q19_11awmelon, watermelon_freq = FU3_mariana.ha1q19_11b1wmelon, watermelon_consump_freq = FU3_mariana.ha1q19_11b2wmelon,
                           muskmelon_averg_consump = FU3_mariana.ha1q19_12amusk_melon, muskmelon_freq = FU3_mariana.ha1q19_12b1musk_melon, muskmelon_consump_freq = FU3_mariana.ha1q19_12b2musk_melon,
                           cusapple_averg_consump = FU3_mariana.ha1q19_13acus_apple, cusapple_freq = FU3_mariana.ha1q19_13b1cus_apple, cusapple_consump_freq = FU3_mariana.ha1q19_13b2cus_apple,
                           zizyphus_averg_consump = FU3_mariana.ha1q19_14azizyphus, zizyphus_freq = FU3_mariana.ha1q19_14b1zizyphus, zizyphus_consump_freq = FU3_mariana.ha1q19_14b2zizyphus,
                           sugarcane_averg_cosump = FU3_mariana.ha1q19_15asugarcane, sugarcane_freq = FU3_mariana.ha1q19_15b1sugarcane, sugarcane_consump_freq = FU3_mariana.ha1q19_15b2sugarcane,
                           palmyra_averg_consump = FU3_mariana.ha1q19_16apalmyra, palmyra_freq = FU3_mariana.ha1q19_16b1palmyra, palmyra_consump_freq = FU3_mariana.ha1q19_16b2palmyra,
                           palak_averg_consump = FU3_mariana.ha1q20_1apalak, palak_freq = FU3_mariana.ha1q20_1b1palak, palak_consump_freq = FU3_mariana.ha1q20_1b2palak,
                           potato_averg_consump = FU3_mariana.ha1q20_2apotato, potato_freq = FU3_mariana.ha1q20_2b1potato, potato_consump_freq = FU3_mariana.ha1q20_2b2potato,
                           beetroot_averg_consump = FU3_mariana.ha1q20_3abeetroot, beetroot_freq = FU3_mariana.ha1q20_3b1beetroot, beetroot_consump_freq = FU3_mariana.ha1q20_3b2beetroot,
                           cabbage_averg_consump = FU3_mariana.ha1q20_4acabbage, cabbage_freq = FU3_mariana.ha1q20_4b1cabbage, cabbage_consump_freq = FU3_mariana.ha1q20_4b2cabbage,
                           beans_averg_consump = FU3_mariana.ha1q20_5abeans, beans_freq = FU3_mariana.ha1q20_5b1beans, beans_consump_freq = FU3_mariana.ha1q20_5b2beans,
                           ladies_averg_consump = FU3_mariana.ha1q20_6aladies, ladies_freq = FU3_mariana.ha1q20_6b1ladies, ladies_consump_freq = FU3_mariana.ha1q20_6b2ladies,
                           caulif_averg_comsump = FU3_mariana.ha1q20_7acaulifl, caulif_freq = FU3_mariana.ha1q20_7b1caulifl, caulif_consump_freq = FU3_mariana.ha1q20_7b2caulifl,
                           bgourd_averg_consump = FU3_mariana.ha1q20_8abgourd, bgourd_freq = FU3_mariana.ha1q20_8b1bgourd, bgourd_consump_freq = FU3_mariana.ha1q20_8b2bgourd,
                           brinjal_averg_consump = FU3_mariana.ha1q20_9abrinjal, brinjal_freq = FU3_mariana.ha1q20_9b1brinjal, brinjal_consump_freq = FU3_mariana.ha1q20_9b2brinjal,
                           kovai_averg_consump = FU3_mariana.ha1q20_10akovai, kovai_freq = FU3_mariana.ha1q20_10b1kovai, kovai_consump_freq = FU3_mariana.ha1q20_10b2kovai,
                           capsicum_averg_consump = FU3_mariana.ha1q20_11acapsicum, capsicum_ferq = FU3_mariana.ha1q20_11b1capsicum, capsicum_consump_freq = FU3_mariana.ha1q20_11b2capsicum,
                           drumstick_averg_consump = FU3_mariana.ha1q20_12adrumstick, drumstick_freq = FU3_mariana.ha1q20_12b1drumstick, drumstick_consump_freq = FU3_mariana.ha1q20_12b2drumstick,
                           plantain_averg_consump = FU3_mariana.ha1q20_13aplantain, plantain_freq = FU3_mariana.ha1q20_13b1plantain, plantain_consump_freq = FU3_mariana.ha1q20_13b2plantain,
                           colacasia_averg_consump = FU3_mariana.ha1q20_14acolacasia, colacasia_freq = FU3_mariana.ha1q20_14b1colacasia, colacasia_consump_freq = FU3_mariana.ha1q20_14b2colacasia)
view(new_FU3)
##AGE####
new_FU3$age_new <- as.Date(new_FU3$age_dob, format = "%Y-%m-%d") 
new_FU3$age_new <- as.integer(difftime(Sys.Date(), new_FU3$age_dob, units = "days") / 365.25)

new_FU3$new_nsli <- as.numeric(FU3_mariana$nsli2)
new_FU3$nsli <- cut(new_FU3$new_nsli, breaks = c(-Inf, 26, 47, Inf), labels = c("Low", "Medium", "High"))


new_FU3$ageGroups <- cut(new_FU3$age_new, c(18, 45, 65,Inf),
                         include.lowest = TRUE, right = FALSE,
                         labels = c("18-44", "45-64", "65+"))
table(new_FU3$ageGroups, useNA="ifany")


##SEX####
new_FU3$sex <- factor(new_FU3$sex, levels = c(1,2),
                      labels = c("Male","Female"))
table(new_FU3$sex, useNA="ifany")

####OCCUPATION####

new_FU3$occupation_category <- NA
new_FU3$occupation_category[is.na(new_FU3$occupation)] <- NA 
new_FU3$occupation_category[new_FU3$occupation %in% c(1)] <- "housework"
new_FU3$occupation_category[new_FU3$occupation %in% c(2, 3)] <- "Unemployed"
new_FU3$occupation_category[new_FU3$occupation %in% c(4)] <- "Student/training"
new_FU3$occupation_category[new_FU3$occupation %in% c(5, 6, 7, 8, 9, 10)] <- "Employed"
table(new_FU3$occupation_category, useNA = "ifany")

table(new_FU3$occupation_category, new_FU3$sex, useNA = "ifany")

##EDUCATION####
new_FU3$education <- as.factor(new_FU3$education) 
new_FU3$edu_level <- ifelse(new_FU3$education %in% c(1, 2), "None",  
                            ifelse(new_FU3$education == 3, "Primary",  
                                   ifelse(new_FU3$education == 4, "Secondary",  
                                          ifelse(new_FU3$education %in% c(5, 6,7), "Higher", NA))))
table(new_FU3$edu_level, useNA = "ifany")
table(new_FU3$sex, new_FU3$edu_level, useNA = "ifany")

##MARITAL STATUS####
new_FU3$maritalStatus <- as.factor(new_FU3$maritalStatus) 
new_FU3$maritalStatus_Cat <-  ifelse(new_FU3$maritalStatus == 1, "Never married",  
                                     ifelse(new_FU3$maritalStatus == 2, "Currently married", 
                                            ifelse(new_FU3$maritalStatus %in% c(3, 4), "Widowed/Divorced/Seperated", NA))) 
table(new_FU3$maritalStatus_Cat, useNA = "ifany")

##TOBACCO EVER####
new_FU3$smokeTobacco <- factor(new_FU3$smokeTobacco, levels = c(1,2,3),
                               labels = c("Never","Former (>6m)","Current"))
table(new_FU3$smokeTobacco, useNA="ifany")

new_FU3$smokeYN <- rep(NA ,nrow(new_FU3))
new_FU3$smokeYN[new_FU3$smokeTobacco %in% c("Former (>6m)","Current")] <- "Yes"
new_FU3$smokeYN[new_FU3$smokeTobacco %in% c("Never")] <- "No"
table(new_FU3$smokeYN, useNA="ifany")

table(new_FU3$smokeYN, new_FU3$sex, useNA = "ifany")

###AVERAGE HRS OF SLEEP A DAY####
new_FU3$sleep_cat <- cut(new_FU3$hrsSleepPerDay, 
                         breaks=c(-Inf, 6, 8, Inf), 
                         labels=c("<6", "6-8", ">8"), 
                         include.lowest=TRUE)
table(new_FU3$sleep_cat, useNA = "ifany")
## <6    6-8     >8   <NA>
## 497   4642   1777  7472

##PHYSICAL ACTIVITY MET CATEGORIES ####
new_FU3$phyactcat <- factor(new_FU3$phyactcat, levels = c(0,1,2,3),
                            labels = c("Extremely inactive ",
                                       "Lightly active",
                                       "Moderately active",
                                       "Vigorously active"))
table(new_FU3$phyactcat, useNA = "ifany")

## Extremely inactive  : 962 
## Lightly active : 3664
## Moderately active : 1625 
##Vigorously active : 328
##<NA> : 7809 

##NSLI - socioeconomic status ####
new_FU3$tertiles_nsli <- factor(new_FU3$tertilesofNSLI, levels = c(1,2,3),
                                labels = c("Low",
                                           "Medium",
                                           "High"))
table(new_FU3$tertiles_nsli, useNA = "ifany")
##   Low  Medium   High   <NA> 
##   314   1743    4887   7444

####ALCOHOL ####
new_FU3$alcohol_consump <- ifelse(is.na(new_FU3$localsSpirits) |
                                    is.na(new_FU3$brandedSpirits) | 
                                    is.na(new_FU3$beer) | 
                                    is.na(new_FU3$wine), NA, 
                                  ifelse((new_FU3$localsSpirits %in% c(1:4) |
                                            new_FU3$brandedSpirits %in% c(1:4) |
                                            new_FU3$beer %in% c(1:4) |
                                            new_FU3$wine %in% c(1:4)), "Yes", "No"))
table(new_FU3$alcohol_consump, useNA = "ifany")
##  No  Yes <NA> 
##2814 4110 7464

##FRUITS & VEGETABLES####
new_FU3$banana_daily <- new_FU3$banana_averg_consump * new_FU3$banana_consump_freq / 
  (case_when(new_FU3$banana_freq == 1 ~ 1,
             new_FU3$banana_freq == 2 ~ 7, 
             new_FU3$banana_freq == 3 ~ 30,
             new_FU3$banana_freq == 4 ~ 365))
new_FU3$apple_daily <- new_FU3$banana_averg_consump * new_FU3$banana_consump_freq /
  (case_when(new_FU3$apple_freq == 1 ~ 1,
             new_FU3$apple_freq == 2 ~ 7,
             new_FU3$apple_freq == 3 ~ 30,
             new_FU3$apple_freq == 4 ~ 365))
new_FU3$orange_daily <- new_FU3$orange_averg_consump * new_FU3$orange_consump_freq /
  (case_when(new_FU3$orange_freq == 1 ~ 1,
             new_FU3$orange_freq == 2 ~ 7,
             new_FU3$orange_freq == 3 ~ 30,
             new_FU3$orange_freq == 4 ~ 365))
new_FU3$mango_daily <-new_FU3$mango_averg_consump * new_FU3$mango_consump_freq /
  (case_when(new_FU3$mango_freq == 1 ~ 1,
             new_FU3$mango_freq == 2 ~ 7,
             new_FU3$mango_freq == 3 ~ 30,
             new_FU3$mango_freq == 4 ~ 365))
new_FU3$guava_daily <- new_FU3$guava_averg_consump * new_FU3$guava_consump_freq /
  (case_when(new_FU3$guava_freq == 1 ~ 1,
             new_FU3$guava_freq == 2 ~ 7,
             new_FU3$guava_freq == 3 ~ 30,
             new_FU3$guava_freq == 4 ~ 365))
new_FU3$grapes_daily <- new_FU3$grapes_averg_comsump * new_FU3$grapes_consump_freq /
  (case_when(new_FU3$grapes_freq == 1 ~ 1,
             new_FU3$grapes_freq == 2 ~ 7,
             new_FU3$grapes_freq == 3 ~ 30,
             new_FU3$grapes_freq == 4 ~ 365))
new_FU3$pineapple_daily <- new_FU3$pineapple_averg_consump * new_FU3$pineapple_consump_freq /
  (case_when(new_FU3$pineapple_freq == 1 ~ 1,
             new_FU3$pineapple_freq == 2 ~ 7,
             new_FU3$pineapple_freq == 3 ~ 30,
             new_FU3$pineapple_freq == 4 ~ 365))
new_FU3$papaya_daily <- new_FU3$papaya_averag_consump * new_FU3$papaya_consump_freq /
  (case_when(new_FU3$papaya_freq == 1 ~ 1,
             new_FU3$papaya_freq == 2 ~ 7,
             new_FU3$papaya_freq == 3 ~ 30,
             new_FU3$papaya_freq == 4 ~ 365))
new_FU3$pomegranade_daily <- new_FU3$pomegranade_averg_consump * new_FU3$pomegranada_consump_freq /
  (case_when(new_FU3$pomegranade_freq == 1 ~ 1,
             new_FU3$pomegranade_freq == 2 ~ 7,
             new_FU3$pomegranade_freq == 3 ~ 30,
             new_FU3$pomegranade_freq == 4 ~ 365))
new_FU3$sapota_daily <- new_FU3$sapota_averg_consump * new_FU3$sapota_consump_freq /
  (case_when(new_FU3$sapota_freq == 1 ~ 1,
             new_FU3$sapota_freq == 2 ~ 7,
             new_FU3$sapota_freq == 3 ~ 30,
             new_FU3$sapota_freq == 4 ~ 365))
new_FU3$watermelon_daily <- new_FU3$watermelon_averg_consump * new_FU3$watermelon_consump_freq /
  (case_when(new_FU3$watermelon_freq == 1 ~ 1,
             new_FU3$watermelon_freq == 2 ~ 7,
             new_FU3$watermelon_freq == 3 ~ 30,
             new_FU3$watermelon_freq == 4 ~ 365))
new_FU3$muskmelon_daily <- new_FU3$muskmelon_averg_consump * new_FU3$muskmelon_consump_freq /
  (case_when(new_FU3$muskmelon_freq == 1 ~ 1,
             new_FU3$muskmelon_freq == 2 ~ 7,
             new_FU3$muskmelon_freq == 3 ~ 30,
             new_FU3$muskmelon_freq == 4 ~ 365))
new_FU3$cusapple_daily <- new_FU3$cusapple_averg_consump * new_FU3$cusapple_consump_freq /
  (case_when(new_FU3$cusapple_freq == 1 ~ 1,
             new_FU3$cusapple_freq == 2 ~ 7,
             new_FU3$cusapple_freq == 3 ~ 30,
             new_FU3$cusapple_freq == 4 ~ 365))
new_FU3$zizyphus_daily <- new_FU3$zizyphus_averg_consump * new_FU3$zizyphus_consump_freq /
  (case_when(new_FU3$zizyphus_freq == 1 ~ 1,
             new_FU3$zizyphus_freq == 2 ~ 7,
             new_FU3$zizyphus_freq == 3 ~ 30,
             new_FU3$zizyphus_freq == 4 ~ 365))
new_FU3$sugarcane_daily <- new_FU3$sugarcane_averg_cosump * new_FU3$sugarcane_consump_freq /
  (case_when(new_FU3$sugarcane_freq == 1 ~ 1,
             new_FU3$sugarcane_freq == 2 ~ 7,
             new_FU3$sugarcane_freq == 3 ~ 30,
             new_FU3$sugarcane_freq == 4 ~ 365))
new_FU3$palmyra_daily <- new_FU3$palmyra_averg_consump * new_FU3$palmyra_consump_freq /
  (case_when(new_FU3$palmyra_freq == 1 ~ 1,
             new_FU3$palmyra_freq == 2 ~ 7,
             new_FU3$palmyra_freq == 3 ~ 30,
             new_FU3$palmyra_freq == 4 ~ 365))
##VEGETABLES##
new_FU3$palak_daily <- new_FU3$palak_averg_consump * new_FU3$palak_consump_freq / 
  (case_when(new_FU3$palak_freq == 1 ~ 1,
             new_FU3$palak_freq == 2 ~ 7, 
             new_FU3$palak_freq == 3 ~ 30,
             new_FU3$palak_freq== 4 ~ 365))
new_FU3$potato_daily <- new_FU3$potato_averg_consump * new_FU3$potato_consump_freq / 
  (case_when(new_FU3$potato_freq == 1 ~ 1,
             new_FU3$potato_freq == 2 ~ 7, 
             new_FU3$potato_freq == 3 ~ 30,
             new_FU3$potato_freq == 4 ~ 365))
new_FU3$beetroot_daily <- new_FU3$beetroot_averg_consump * new_FU3$beetroot_consump_freq / 
  (case_when(new_FU3$beetroot_freq == 1 ~ 1,
             new_FU3$beetroot_freq == 2 ~ 7, 
             new_FU3$beetroot_freq == 3 ~ 30,
             new_FU3$beetroot_freq == 4 ~ 365))
new_FU3$cabbage_daily <- new_FU3$cabbage_averg_consump * new_FU3$cabbage_consump_freq / 
  (case_when(new_FU3$cabbage_freq == 1 ~ 1,
             new_FU3$cabbage_freq == 2 ~ 7, 
             new_FU3$cabbage_freq == 3 ~ 30,
             new_FU3$cabbage_freq == 4 ~ 365))
new_FU3$beans_daily <- new_FU3$beans_averg_consump * new_FU3$beans_consump_freq / 
  (case_when(new_FU3$beans_freq == 1 ~ 1,
             new_FU3$beans_freq == 2 ~ 7, 
             new_FU3$beans_freq == 3 ~ 30,
             new_FU3$beans_freq == 4 ~ 365))
new_FU3$ladies_daily <- new_FU3$ladies_averg_consump * new_FU3$ladies_consump_freq / 
  (case_when(new_FU3$ladies_freq == 1 ~ 1,
             new_FU3$ladies_freq == 2 ~ 7, 
             new_FU3$ladies_freq == 3 ~ 30,
             new_FU3$ladies_freq == 4 ~ 365))
new_FU3$caulif_daily <- new_FU3$caulif_averg_comsump * new_FU3$caulif_consump_freq / 
  (case_when(new_FU3$caulif_freq == 1 ~ 1,
             new_FU3$caulif_freq == 2 ~ 7, 
             new_FU3$caulif_freq == 3 ~ 30,
             new_FU3$caulif_freq == 4 ~ 365))
new_FU3$bgourd_daily <- new_FU3$bgourd_averg_consump * new_FU3$bgourd_consump_freq / 
  (case_when(new_FU3$bgourd_freq == 1 ~ 1,
             new_FU3$bgourd_freq == 2 ~ 7, 
             new_FU3$bgourd_freq == 3 ~ 30,
             new_FU3$bgourd_freq == 4 ~ 365))
new_FU3$brinajl_daily <- new_FU3$brinjal_averg_consump * new_FU3$brinjal_consump_freq / 
  (case_when(new_FU3$brinjal_freq == 1 ~ 1,
             new_FU3$brinjal_freq == 2 ~ 7, 
             new_FU3$brinjal_freq == 3 ~ 30,
             new_FU3$brinjal_freq == 4 ~ 365))
new_FU3$kovai_daily <- new_FU3$kovai_averg_consump * new_FU3$kovai_consump_freq / 
  (case_when(new_FU3$kovai_freq == 1 ~ 1,
             new_FU3$kovai_freq == 2 ~ 7, 
             new_FU3$kovai_freq == 3 ~ 30,
             new_FU3$kovai_freq == 4 ~ 365))
new_FU3$capsicum_daily <- new_FU3$capsicum_averg_consump * new_FU3$capsicum_consump_freq / 
  (case_when(new_FU3$capsicum_ferq == 1 ~ 1,
             new_FU3$capsicum_ferq == 2 ~ 7, 
             new_FU3$capsicum_ferq == 3 ~ 30,
             new_FU3$capsicum_ferq == 4 ~ 365))
new_FU3$drumstick_daily <- new_FU3$drumstick_averg_consump * new_FU3$drumstick_consump_freq / 
  (case_when(new_FU3$drumstick_freq == 1 ~ 1,
             new_FU3$drumstick_freq == 2 ~ 7, 
             new_FU3$drumstick_freq == 3 ~ 30,
             new_FU3$drumstick_freq == 4 ~ 365))
new_FU3$plantain_daily <- new_FU3$plantain_averg_consump * new_FU3$plantain_consump_freq / 
  (case_when(new_FU3$plantain_freq == 1 ~ 1,
             new_FU3$plantain_freq == 2 ~ 7, 
             new_FU3$plantain_freq == 3 ~ 30,
             new_FU3$plantain_freq == 4 ~ 365))
new_FU3$colacasia_daily <- new_FU3$colacasia_averg_consump * new_FU3$colacasia_consump_freq / 
  (case_when(new_FU3$colacasia_freq == 1 ~ 1,
             new_FU3$colacasia_freq == 2 ~ 7, 
             new_FU3$colacasia_freq == 3 ~ 30,
             new_FU3$colacasia_freq == 4 ~ 365))

##total fruits and vegetables intake per day ####
new_FU3$total_daily_intake <- rowSums(new_FU3[, grep("daily", names(new_FU3))], na.rm = TRUE)

##categorize if above or below 5 portions a day
new_FU3$total_daily_intake <- ifelse(new_FU3$total_daily_intake == 0, NA, new_FU3$total_daily_intake)
new_FU3$intake_category <- ifelse(new_FU3$total_daily_intake >= 5, ">5", "<5")

table(new_FU3$intake_category, useNA = "ifany")
##  <5   >5   <NA> 
## 3338 3573  7477

##EVER BEEN DIAGNOSED WITH HYPERTENSION####
new_FU3$dxHTA <- factor(new_FU3$dxHTA, levels = c(1,2),
                        labels = c("Yes","No"))
table(new_FU3$dxHTA, useNA="ifany")
## Yes   No    <NA> 
##349   6394    7645

###EVER BEEN DIAGNOSED WITH DIABETES####
new_FU3$dxDM <- factor(new_FU3$dxDM, levels = c(1,2),
                       labels = c("Yes","No"))
table(new_FU3$dxDM, useNA="ifany")
##  Yes   No   <NA> 
##  119  6618  7651 


##########  DIABETES SCREENING AND DIAGNOSIS ##########
##diabetes based on fasting blood glucose 
new_FU3$fbs_category <- ifelse(new_FU3$fbs >= 126, "Above 126", "Below 126")
table(new_FU3$fbs_category, useNA = "ifany")

new_FU3$dm_positive <- ifelse(is.na(new_FU3$fbs), NA, ifelse(new_FU3$fbs >= 126, "Yes", "No"))
table(new_FU3$dm_positive, useNA = "ifany")

table(new_FU3$dxDM, new_FU3$dm_positive, useNA = "ifany")

### HYPERTENSION SCREENING AND DIAGNOSIS ####
# average systolic blood pressure
new_FU3$avg_systolicBP <- ifelse(is.na(new_FU3$systoloicBP2) | is.na(new_FU3$systoloicBP3),
                                 NA,
                                 rowMeans(new_FU3[, c("systoloicBP2", "systoloicBP3")], na.rm = TRUE))

# average diastolic blood pressure
new_FU3$avg_diastolicBP <- ifelse(is.na(new_FU3$diastolicBP2) | is.na(new_FU3$diastolicBP3),
                                  NA,
                                  rowMeans(new_FU3[, c("diastolicBP2", "diastolicBP3")], na.rm = TRUE))

new_FU3$systoloic_cat <- ifelse(new_FU3$avg_systolicBP >= 140, "Above 140", "Below 140")
new_FU3$diastolic_cat <- ifelse(new_FU3$avg_diastolicBP >= 90, "Above 90", "Below 90")

new_FU3$hta_positive <- ifelse(new_FU3$systoloic_cat == "Above 140" | new_FU3$diastolic_cat == "Above 90", "Yes", "No")
table(new_FU3$hta_positive, useNA = "ifany")

table(new_FU3$dxHTA, new_FU3$hta_positive, useNA = "ifany")

# number of years since diabetes diagnosis
new_FU3$years_since_diagnosis_dm <- ifelse(!is.na(new_FU3$ageDxDM), new_FU3$age - new_FU3$ageDxDM, NA)
new_FU3$years_since_diagnosis_hta <- ifelse(!is.na(new_FU3$ageDxHTA), new_FU3$age - new_FU3$ageDxHTA, NA)
##complete case of third follow up####
new_FU3 <- new_FU3 %>% mutate(fu3 = 3)
newone2 <- new_FU3[!(is.na(new_FU3$dxHTA) & is.na(new_FU3$dxDM)), ]
#research question 1 and 2
fu3_complete <- subset(newone2, select = c("participantID", "sex", "ageGroups", "maritalStatus_Cat",
                                           "occupation_category", "phyactcat", "smokeYN", "fu3","nsli",
                                           "dxHTA", "ageDxHTA", "dxDM", "ageDxDM", "dxDepression",
                                           "systoloicBP2", "systoloicBP3", "edu_level","age",
                                           "diastolicBP2", "diastolicBP3", "tertilesofNSLI",
                                           "fbs", "smokeYN", "sleep_cat", "dm_or_hta1",
                                           "fbs_category", "hta_positive", "dm_positive",
                                           "avg_systolicBP", "avg_diastolicBP", "alcohol_consump",
                                           "systoloic_cat", "diastolic_cat", "total_daily_intake", "intake_category",
                                           "years_since_diagnosis_hta", "years_since_diagnosis_dm","age","age_new"
))




#########1.	To determine and compare the incidence of depression 
## 	Diagnosed with hypertension and/or diabetes who self-reported####
#diabetes####
fu3_complete <- fu3_complete %>%
  mutate(dm_diagnosis = case_when(
    is.na(dxDM) ~ NA_character_,
    dxDM == "Yes" ~ "Yes",
    TRUE ~ "No"))
table(fu3_complete$dm_diagnosis)

#hypertension ####

fu3_complete <- fu3_complete %>%
  mutate(hta_diagnosis = case_when(
    is.na(dxHTA) ~ NA_character_,
    dxHTA == "Yes" ~ "Yes",
    TRUE ~ "No"))
table(fu3_complete$hta_diagnosis)

# Variable for self report diabetes or hypertension ####
fu3_complete$dm_or_hta1 <- ifelse(
  (fu3_complete$hta_diagnosis == "Yes" | fu3_complete$dm_diagnosis == "Yes") & (!is.na(fu3_complete$hta_diagnosis) | !is.na(fu3_complete$dm_diagnosis)),
  "Yes",
  ifelse(fu3_complete$hta_diagnosis == "No" & is.na(fu3_complete$dm_diagnosis), NA,
         ifelse(fu3_complete$dm_diagnosis == "No" & is.na(fu3_complete$hta_diagnosis), NA, "No")))
table(fu3_complete$dm_or_hta1)

# Variable for no hypertension diagnosis but high blood pressure - hta positive screening####
fu3_complete <- fu3_complete %>%
  mutate(hta_screening1 = ifelse(
    hta_diagnosis == "No" & hta_positive == "Yes" &  dm_diagnosis == "No", "Yes",
    ifelse(is.na(hta_diagnosis) | is.na(hta_positive)| is.na(dm_diagnosis), NA, "No")))
table(fu3_complete$hta_screening1)

# Variable for no diabetes diagnosis but high fasting glucose - dm positive screening####
fu3_complete <- fu3_complete %>%
  mutate(dm_screening2 = ifelse(
    dm_diagnosis == "No" & dm_positive == "Yes", "Yes",
    ifelse(is.na(dm_diagnosis) | is.na(dm_positive), NA, "No")))
table(fu3_complete$dm_screening2)

# Variable for no diabetes or no hypertension diagnosis but high blood ####
# pressure or high fasting blood glucose 
fu3_complete$screening1 <- ifelse(
  (fu3_complete$dm_screening2 == "Yes" | fu3_complete$hta_screening1 == "Yes") & (!is.na(fu3_complete$hta_screening1) | !is.na(fu3_complete$dm_screening2)),
  "Yes",
  ifelse(fu3_complete$dm_screening2 == "No" & is.na(fu3_complete$hta_screening1), NA,
         ifelse(fu3_complete$hta_screening1 == "No" & is.na(fu3_complete$dm_screening2), NA, "No")))
table(fu3_complete$screening1)

##NONE - no dm or hta or screening positive for hta or dm ####
fu3_complete <- fu3_complete %>%
  mutate(none_all = case_when(
    is.na(dm_screening3) | is.na(hta_screening2) | is.na(dm_diagnosis)| is.na(hta_diagnosis)~ NA_character_,
    dm_screening3 == "No" & hta_screening2 == "No" & dm_diagnosis == "No" & hta_diagnosis == "No" ~ "Yes",
    TRUE ~ "No"))
table(fu3_complete$none_all)

####################  GROUPING  ####################
#DIAGNOSED GROUP####
fu3_complete <- fu3_complete %>%
  mutate(diagnosed = case_when(
    is.na(dm_diagnosis) | is.na(hta_diagnosis) | is.na(dm_or_hta1) ~ NA_character_,
    dm_diagnosis == "Yes" | hta_diagnosis == "Yes" | dm_or_hta1 == "Yes" ~ "Yes",
    TRUE ~ "No"))
table(fu3_complete$diagnosed)

#UNDIAGNOSED GROUP ####
fu3_complete <- fu3_complete %>%
  mutate(undiagnosed = case_when(
    is.na(hta_screening1) | is.na(dm_screening2) | is.na(screening2) ~ NA_character_,
    hta_screening1 == "Yes" | dm_screening2 == "Yes" | screening2 == "Yes" ~ "Yes",
    TRUE ~ "No"))
table(fu3_complete$undiagnosed)

##diagnosis status of population ####
fu3_complete$diagnosis_status1 <- ifelse(
  is.na(fu3_complete$diagnosed) | is.na(fu3_complete$undiagnosed) | is.na(fu3_complete$none_all),
  NA_character_,
  ifelse(fu3_complete$diagnosed == "Yes", "Diagnosed",
         ifelse(fu3_complete$undiagnosed == "Yes", "Undiagnosed",
                ifelse(fu3_complete$none_all == "Yes", "Neither", NA_character_))))
table(fu3_complete$diagnosis_status1)



### complete case third follow up ####
#research question 1
fu3_complete1 <- subset(fu3_complete, select = c("participantID", "sex", "ageGroups", "maritalStatus_Cat",
                                                 "occupation_category", "phyactcat", "smokeYN", "fu3",
                                                 "dxHTA", "ageDxHTA", "dxDM", "ageDxDM", "dxDepression",
                                                 "systoloicBP2", "systoloicBP3", "edu_level","age",
                                                 "diastolicBP2", "diastolicBP3", "tertilesofNSLI",
                                                 "fbs", "smokeYN", "nsli", "sleep_cat", 
                                                 "fbs_category", "hta_positive", "dm_positive",
                                                 "avg_systolicBP", "avg_diastolicBP", "alcohol_consump",
                                                 "systoloic_cat", "diastolic_cat", "total_daily_intake", "intake_category",
                                                 "dm_diagnosis", "diagnosis_status", "dm_or_hta1","diagnosed", "undiagnosed"
                                                 "hta_diagnosis" ,"hta_screening2", "dm_screening3", "dm_screening2", "dm_screening1",
                                                 "screening1","age_new","hta_screening1", "hta_screening3", "screening2",
                                                 "none_all", "dm_screening2"))
write.csv(fu3_complete1, file = "fu3_complete1.csv", row.names = FALSE)






