###############################################################################################################
#
# Pre-processing script for Richards-Belle et al
#
# This script pre-processes the ELSA data files by recoding variables merging datafiles from
# different waves into a single R Data File (.rda file)
#
# Analysis for study completed on R 4.1.2
#
###############################################################################################################

# This script is to create the master dataset of data from each ELSA study wave. 
# Wave-specific variables are named e.g. w1depression (for depression from wave 1).

# This script covers:
# - Creation of a master dataset of data from each ELSA study wave (Wave-specific variables are named e.g. w1depression (for depression from wave 1)).
# - Creation of complete case survival analysis datasets
# - Creation of multiple imputation datasets for survival analysis

# Clear memory
rm(list = ls())

# Load packages
library(dplyr)      # used as main data manipulation package
library(foreign)    # allows import of SPSS files
library(stringr)    # used to manipulate string variables
library(missForest) # used for multiple imputation
library(doParallel)
library(doRNG)

# Location of data directory
data_dir <- "/home/main/data/StrokePsychosisELSAData/"

# Location of directory for writing transformed / pre-processed data files to
output_dir <- "/home/main/data/StrokePsychosisELSATransformedData/"

# Data file names
elsaindex_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/index_file_wave_0-wave_5_v2.sav", sep = "")

elsawave1_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_1_core_data_v3.sav", sep="")
elsawave2_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_2_core_data_v4.sav", sep="")
elsawave3_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_3_elsa_data_v4.sav", sep="")
elsawave4_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_4_elsa_data_v3.sav", sep="")
elsawave5_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_5_elsa_data_v4.sav", sep="")
elsawave6_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_6_elsa_data_v2.sav", sep="")
elsawave7_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_7_elsa_data.sav", sep="")
elsawave8_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_8_elsa_data_eul_v2.sav", sep="")
elsawave9_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_9_elsa_data_eul_v1.sav", sep="")

elsawave1_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_1_financial_derived_variables.sav", sep="")
elsawave2_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_2_financial_derived_variables.sav", sep="")
elsawave3_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_3_financial_derived_variables.sav", sep="")
elsawave4_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_4_financial_derived_variables.sav", sep="")
elsawave5_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_5_financial_derived_variables.sav", sep="")
elsawave6_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_6_financial_derived_variables.sav", sep="")
elsawave7_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_7_financial_derived_variables.sav", sep="")
elsawave8_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_8_elsa_financial_dvs_eul_v1.sav", sep="")
elsawave9_financial_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_9_financial_derived_variables.sav", sep="")

elsawave0_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_0_common_variables_v2.sav", sep="")
wave01998_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_0_1998_data.sav", sep="")
wave01999_file <- paste(data_dir, "/UKDA-5050-spss/spss/spss25/wave_0_1999_data.sav", sep="")

# Import data

# ELSA index file
# ELSA Index File (Waves 0–5) including key status variables for each wave such as fieldwork issue status and outcome codes, mortality status. 
# It has not been possible to update the Index File since Wave 5 as no further updates to mortality data have been received from NHS Digital.
# The index file is used as the master file for who participated in waves 1-5 when it comes to creating the master dataset later on.
elsaindex <- read.spss(elsaindex_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsaindex %>%
#  count()
# n=37,938 (user guide says file contains a total of 37,949 but is from 2013 - maybe some people have withdrawn?)

# check for duplicates in ID number
#elsaindex %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
index <- select(elsaindex,
                idauniq, 
                indexfinstatw1 = finstatw1, wave1indoutcome = outindw1,
                indexfinstatw2 = finstatw2, wave2indoutcome = outindw2, wave2nurseoutcome = outnrsw2,
                indexfinstatw3 = finstatw3, wave3indoutcome = outindw3,
                indexfinstatw4 = finstatw4, wave4indoutcome = outindw4, wave4nurseoutcome = outnrsw4,
                indexfinstatw5 = finstatw5, wave5indoutcome = outindw5,
                mortalitywave = mortwave,
                sex, dobyear)

#table(index$mortalitywave, useNA = "always")

# ASSIGNING LABELS/CODES
# Sex
index$sex <- factor(index$sex, levels = c(1, 2), labels = c("Male", "Female"))

# DoB year
index$dobyear[index$dobyear == -8] <- NA # "unknown"
index$dobyear[index$dobyear == -7] <- 1914 # "respondent >= 99 as at 01/03/2013" # the DOB year was censored for participants >99 so this has been recoded manually to the highest age

# Wave participation status (these are ultimately all not essential to the analysis)
#index$wave0indoutcome <- factor(index$wave0indoutcome, levels = c(-2, -1, 51, 52, 53, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 340, 431, 432, 440, 450, 510, 521, 540), 
#                                labels = c("Not in household or ineligible for interview", "Was in household but does not have an outcome code", "Full interview", "Partial interview", 
#                                           "Full interview in translation", "No contact", "Personal refusal", "Proxy refusal", "Broken appointment", "Ill at home", "Ill in hospital",
#                                           "Away from home", "Senile/incapacitated", "Inadequate English", "Other reasons - no interview", "No contact", "Refused before interview (personal)", 
#                                           "Refused before interview (proxy)", "Refusal during interview", "Broken appointment, no recontact", "Ill at home during survey period", 
#                                           "Away during survey period", "Language difficulties"))

#index$wave0nurseoutcome <- factor(index$wave0nurseoutcome, levels = c(-5, -1, 69, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90),
#                                  labels = c("No nurse assigned", "Not eligible/no consent", "Lost data after interview", "Refused nurse visit - not to be interviewed", 
#                                             "Schedule completed", "Schedule not completed, no contact made", "Refusal by person", "Proxy refusal", "Broken appointment", "Ill at home", 
#                                             "Ill at hospital", "Away", "Other", "Refusal to office"))

index$indexfinstatw1 <- factor(index$indexfinstatw1, levels = c(1, 4, 5), labels = c("C1CM", "C1YP", "C1NP1"))

index$indexfinstatw2 <- factor(index$indexfinstatw2, levels = c(-993, -992, -991, 1, 2, 3, 4, 5, 6), 
                               labels = c("New partner not yet in household", "Member/partner of later cohort", "Non-eligible (not a sample member or partner)", "C1CM", "C1CP", "C1SM", 
                                          "C1YP", "C1NP1", "C1NP2"))

index$indexfinstatw3 <- factor(index$indexfinstatw3, levels = c(-993, -992, -991, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
                               labels = c("New partner not yet in household", "Member/partner of later cohort", "Non-eligible (not a sample member or partner)", "C1CM", "C1CP", "C1SM", 
                                          "C1YP", "C1NP1", "C1NP2", "C3CM", "C3CP", "C3SM", "C3YP", "C3OP", "C3NP3", "C1NP3"))

index$indexfinstatw4 <- factor(index$indexfinstatw4, levels = c(-993, -991, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), 
                               labels = c("New Partner not yet in household", "Non-eligible - not a sample member or partner", "C1CM", "C1CP", "C1SM", "C1YP", "C1NP1", "C1NP2", "C3CM", 
                                          "C3CP", "C3SM", "C3YP", "C3OP", "C3NP3", "C1NP3", "C4CM", "C4CP", "C4SM", "C4YP", "C4OP", "C4NP4", "C3NP4", "C1NP4"))

index$indexfinstatw5 <- factor(index$indexfinstatw5, levels = c(-991, -10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24), 
                               labels = c("Non-eligible (not a sample member or partner)", "Temporary code - case being checked", "C1CM", "C1CP", "C1SM", "C1YP", "C1NP1", "C1NP2", 
                                          "C3CM", "C3CP", "C3SM", "C3YP", "C3OP", "C3NP3", "C1NP3", "C4CM", "C4CP", "C4SM", "C4YP", "C4OP", "C4NP4", "C3NP4", "C1NP4", "C1NP5", "C3NP5", "C4NP5"))

index$wave1indoutcome <- factor(index$wave1indoutcome, levels = c(-993, -992, -991, -1, -2, -10, 11, 13, 21, 23, 31, 43, 44, 45, 51, 52, 53, 54, 55, 56, 79, 99, 310, 330, 340, 410, 420, 
                                                                  431, 432, 440, 450, 510, 520, 530, 540, 550, 560, 561, 610, 620, 630, 680, 781, 782, 783, 791, 792, 793, 999),
                                labels = c("New partner not yet in household", "Member/partner of later cohort", "Non-eligible (not a sample member or partner)", "Not issued", 
                                           "Not issued in this wave", "Temporary code - case being checked", "Full interview in person", "Full proxy interview", 
                                           "Partial interview in person", "Partial proxy interview", "No contact", "Refusal before interview", "Refusal during interview", 
                                           "Broken appointment, no recontact", "Ill at home during survey period", "Away/ill in hospital during survey period", 
                                           "Physically/mentally unable/incompetent", "Language difficulties", "Lost productive", "Other unproductive", "Died (ineligible)", 
                                           "Died (updated, outcome unprod)", "No contact with anyone at address", "Contact made but not with responsible resident", 
                                           "Contact made but not with eligible resident", "Refusal to office", "Refusal of info about occupants of address", 
                                           "Refusal at intro/before HH module by eligible respondent", "Refusal at intro/before HH module by other", "Refusal after HH module", 
                                           "Broken appointment, no recontact", "Ill at home during survey period","Away in hospital all survey period", 
                                           "Physically/mentally unable/incompetent", "Language difficulties", "Lost productive", "Other unproductive", 
                                           "Productive but respondent requested deletion", "Address not attempted", "Address inaccessible", "Unable to locate address", 
                                           "Moved – unable to trace", "Address out of sample – moved to institution", "Duplicate address", "Address out of sample – moved out of England",
                                           "Ineligible – all SM died", "Ineligible – SM ineligible", "Ineligible – other reason", "Sector not covered"))

index$wave2indoutcome <- factor(index$wave2indoutcome, levels = c(-993, -992, -991, -2, -1, 11, 13, 21, 23, 31, 43, 44, 45, 46, 51, 52, 53, 54, 56, 60, 68, 71, 78, 79, 90, 99),
                                labels = c("New partner not yet in household", "Member/partner of later cohort", "Non-eligible (not a sample member or partner)", 
                                           "Not issued in this wave", "Not eligible for this wave", "Full interview in person", "Full proxy interview", "Partial interview in person",
                                           "Partial interview by proxy", "No contact", "Refusal before interview", "Refusal during interview", "Broken appointment", "Office refusal",
                                           "Ill at home during survey period", "Away/in hospital during survey period", "Physically or mentally incompetent", "Language difficulties",
                                           "Other unproductive", "Untraced", "Moved – unable to attempt contact at new address", "In institution", "Moved – out of Britain",
                                           "Outcome unknown", "Died", "Died (updated, outcome unprod)"))

index$wave3indoutcome <- factor(index$wave3indoutcome, levels = c(-993, -992, -991, -10, -2, -1, 11, 13, 21, 24, 25, 31, 43, 44, 45, 46, 51, 52, 53, 54, 55, 56, 60, 71, 77, 78, 79, 95, 99), 
                                labels = c("New partner not yet in household", "Member/partner of later cohort", "Non-eligible (not a sample member or partner)", 
                                           "Temporary code - case being checked", "Not issued in this wave", "Not eligible for this wave", "Full interview in person", "Full interview by proxy", 
                                           "Partial interview in person", "Institutional interview in person", "Institutional interview by proxy", "No contact", "Refusal before interview", 
                                           "Refusal during interview (computed)", "Broken appointment - no re-contact", "Office refusal", "Ill at home during survey period", 
                                           "Away/ill in hospital during survey period", "Physically or mentally unable/incompetent", "Language difficulties", "Respondent requested data deletion", 
                                           "Other unproductive", "Untraced", "In a institution, unproductive", "Ineligible - partner who is now not living with Core Member", "Out of Britain", 
                                           "Ineligible (issued in error)", "Died", "Died (updated, outcome unproductive)"))

index$wave4indoutcome <- factor(index$wave4indoutcome, levels = c(-993, -991, -4, -2, -1, 11, 13, 21, 23, 24, 25, 31, 43, 44, 45, 46, 51, 52, 53, 54, 55, 57, 59, 60, 71, 77, 78, 95), 
                                labels = c("New partner not yet in household", "Not eligible - not a sample member or partner", 
                                           "Refused W4 interview at preadvance letter, but requested interview at W5", "Not issued this wave", "Not eligible for this wave", 
                                           "Full interview in person", "Full interview by proxy", "Partial interview in person", "Partial interview by proxy", 
                                           "Institutional interview in person", "Institutional interview by proxy", "No contact", "Refusal before interview", 
                                           "Refusal during interview (computed)", "Broken appointment - no re-contact", "Office refusal", "Ill at home during survey period", 
                                           "Away/ill in hospital during survey period", "Physically or mentally unable/incompetent", "Language difficulties", "Requested data deletion", 
                                           "In a institution, unproductive", "Other unproductive", "Untraced", "In an institution", 
                                           "Ineligible - partner who is now not living with Core Member (refreshment sample only)", "Out of Britain", "Died"))

index$wave5indoutcome <- factor(index$wave5indoutcome, levels = c(-993, -991, -98, -2, -1, 11, 13, 21, 23, 24, 25, 31, 43, 44, 45, 46, 51, 52, 53, 54, 57, 59, 60, 78, 79, 95), 
                                labels = c("New partner not yet in household", "Not eligible - not a sample member or partner", "Not interviewed as ELSA /Parnter flag was missing", 
                                           "Not issued this wave", "Not eligible for this wave", "Full interview in person", "Full interview by proxy", "Partial interview in person", 
                                           "Partial interview by proxy", "Institutional interview in person", "Institutional interview by proxy", "No contact", "Refusal before interview", 
                                           "Refusal during interview (computed)", "Broken appointment - no re-contact", "Office refusal", "Ill at home during survey period", 
                                           "Away/ill in hospital during survey period", "Physically or mentally unable/incompetent", "Language difficulties", "In a institution, unproductive", 
                                           "Other unproductive", "Untraced", "Out of Britain", "Ineligible (issued in error)", "Died"))

index$mortalitywave <- factor(index$mortalitywave, levels = c(-5, 0, 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53, 61, 63),
                              labels = c("Joined household after HSE and ineligible for interview - will not have mortality data", "Alive/not known to have died", 
                                         "Pre-W1 issue - W1 sampling", "Pre-W1 - W1 fieldwork", "Pre-W1 issue - external update", 
                                         "Post-W1 issue & pre-W2 issue - W2 sampling", "Pre-W2 - W2 fieldwork", "Post-W1 issue & pre-W2 issue - external update",
                                         "Post-W2 issue & pre-W3 issue - W3 sampling", "Pre-W3 - W3 fieldwork", "Post-W2 issue & pre-W3 issue - external update",
                                         "Post-W3 issue & Pre-W4 issue - W4 sampling", "Pre-W4 - W4 fieldwork", "Post-W3 issue and pre-W4 issue - external update", 
                                         "Post- W4 issue & Pre-W5 issue - W5 sampling", "Pre-W5 - W5 fieldwork", "Post-W4 issue and pre-W5 issue - external update",
                                         "Post-W5 issue & Pre -W6 issue -W6 sampling", "Post-W5 issue and pre-W6 issue - external update"))

# ELSA wave 1 data
# Wave 1 data collection took place March 2002 – March 2003

# IMPORT DATA
elsawave1 <- read.spss(elsawave1_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave1 %>%
#  count()
# n=12,099

# check for duplicates in ID number
#elsawave1 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES (note that each wave has a separate data dictionary with the variable codes and definitions; variables are organised slightly differently in some waves)
wave1 <- select(elsawave1,
                idauniq, w1finstat = finstat, w1indout = indoc, 
                hedim01, hedim02, hedim03, hedim04, hedim05, hedim06, hedim07,
                w1strokeage = heage, w0strokeage = aagestro, w1strokeremainproblems = hepbs,
                hepsy1, hepsy2, hepsy3, hepsy4, hepsy5, hepsy6, hepsy7, hepsy8, hepsy9,
                w1psychiatricage = heagh, w1psychiatrciproblemlast2yr = heyrc,
                w1region = gor, w1ethnicgroup = aethnicr,
                w1smokeever = hesmk, w1smokenow = heska, w1weekdaysmoke = heskb, w1weekendsmoke = heskc,
                w1alcohol = heala, w1vigorousphyact = heacta, w1modphysact = heactb, w1mildphysact = heactc)

# CODING/LABELLING

# White/non-white ethnicity
wave1$w1ethnicgroup <- factor(wave1$w1ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
# table(wave1$w1ethnicgroup)

# Region
str_squish(wave1$w1region)
wave1$w1region [wave1$w1region == "A "] <- "north east"
wave1$w1region [wave1$w1region == "B "] <- "north west"
wave1$w1region [wave1$w1region == "D "] <- "yorkshire and the humber"
wave1$w1region [wave1$w1region == "E "] <- "east midlands"
wave1$w1region [wave1$w1region == "F "] <- "west midlands"
wave1$w1region [wave1$w1region == "G "] <- "east of england"
wave1$w1region [wave1$w1region == "H "] <- "london"
wave1$w1region [wave1$w1region == "J "] <- "south east"
wave1$w1region [wave1$w1region == "K "] <- "south west"
wave1[wave1$idauniq==108802, "w1region"] <- NA # changed to NA as appeared to be a blank space in this field
wave1[wave1$idauniq==103723, "w1region"] <- NA # changed to NA as appeared to be a blank space in this field
#table(wave1$w1region, useNA = "always")

# Sex
wave1$sex <- factor(wave1$sex, levels = c(1,2), labels = c("male", "female"))

# Stroke age
wave1$w1strokeage[wave1$w1strokeage == -8] <- NA # "don't know"
wave1$w1strokeage[wave1$w1strokeage == -1] <- NA # not applicable
#table(wave1$strokeage)

# Stroke age (fed forward from HSE survey, ELSA wave 0)
wave1$w0strokeage[wave1$w0strokeage == -1] <- NA
#table(wave1$w0strokeage)

# Currently taking medicines/tablets/pills for high blood pressure
wave1$w1hypertensionmeds[wave1$w1hypertensionmeds < 0] <- NA #"NA or refused"
wave1$w1hypertensionmeds[wave1$w1hypertensionmeds < 2] <- 0 #"no"
wave1$w1hypertensionmeds[wave1$w1hypertensionmeds == 2] <- 1 #"yes"
#table(wave1$w1hypertensionmeds)

# Remaining problems because of your strokes
wave1$w1strokeremainproblems[wave1$w1strokeremainproblems == 2] <- 0 #"no"
wave1$w1strokeremainproblems[wave1$w1strokeremainproblems < 0] <- NA # NA
#table(wave1$w1strokeremainproblems)

# During the last two years, have you had the emotional/psych/nervous problems?
wave1$w1psychiatrciproblemlast2yr[wave1$w1psychiatrciproblemlast2yr == 2] <- 0 # no
wave1$w1psychiatrciproblemlast2yr[wave1$w1psychiatrciproblemlast2yr < 0] <- NA # NA/refused/don't know
#table(wave1$w1psychiatrciproblemlast2yr)

# At what age were you told you had an emotional/psychiatric/nervious problem? May not relate to the psychosis diagnosis.
wave1$w1psychiatricage[wave1$w1psychiatricage == -8] <- NA # "don't know"
wave1$w1psychiatricage[wave1$w1psychiatricage == -1] <- NA # not applicable
#table(wave1$w1psychiatricage)

# Stroke (doctor diagnosed)
# Create column called w1strokeany which defines a case where 8 is recorded in any of the hedi variables
wave1 <- wave1 %>% 
  mutate(w1strokeany = case_when(hedim01 == 8 ~ '1', 
                                 hedim02 == 8 ~ '1',
                                 hedim03 == 8 ~ '1',
                                 hedim04 == 8 ~ '1',
                                 hedim05 == 8 ~ '1',
                                 hedim06 == 8 ~ '1',
                                 hedim07 == 8 ~ '1',
                                 TRUE ~ '0'))
#table(wave1$w1strokeany)
#n=516

# Psychosis
# Create column called w1psychosisany which defines a case where 1 (hallucinations), 5 (schz), 6 (psychosis) is recorded in any of the hepsy variables
wave1 <- wave1 %>%
  mutate(w1psychosisany = case_when(hepsy1 == 1 ~ '1',
                                    hepsy1 == 5 ~ '1',
                                    hepsy1 == 6 ~ '1',
                                    hepsy2 == 1 ~ '1',
                                    hepsy2 == 5 ~ '1',
                                    hepsy2 == 6 ~ '1',
                                    hepsy3 == 1 ~ '1',
                                    hepsy3 == 5 ~ '1',
                                    hepsy3 == 6 ~ '1',
                                    hepsy4 == 1 ~ '1',
                                    hepsy4 == 5 ~ '1',
                                    hepsy4 == 6 ~ '1',
                                    hepsy5 == 1 ~ '1',
                                    hepsy5 == 5 ~ '1',
                                    hepsy5 == 6 ~ '1',
                                    hepsy6 == 1 ~ '1',
                                    hepsy6 == 5 ~ '1',
                                    hepsy6 == 6 ~ '1',
                                    hepsy7 == 1 ~ '1',
                                    hepsy7 == 5 ~ '1',
                                    hepsy7 == 6 ~ '1',
                                    hepsy8 == 1 ~ '1',
                                    hepsy8 == 5 ~ '1',
                                    hepsy8 == 6 ~ '1',
                                    hepsy9 == 1 ~ '1',
                                    hepsy9 == 5 ~ '1',
                                    hepsy9 == 6 ~ '1',
                                    TRUE ~ '0'))
#table(wave1$w1psychosisany)
#n=41

# Create column called w1depression which defines a case where 3 ("depression") is recorded in any of the hepsy variables
wave1 <- wave1 %>%
  mutate(w1depression = case_when(hepsy1 == 3 ~ '1',
                                  hepsy2 == 3 ~ '1',
                                  hepsy3 == 3 ~ '1',
                                  hepsy4 == 3 ~ '1',
                                  hepsy5 == 3 ~ '1',
                                  hepsy6 == 3 ~ '1',
                                  hepsy7 == 3 ~ '1',
                                  hepsy8 == 3 ~ '1',
                                  hepsy9 == 3 ~ '1',
                                  TRUE ~ '0'))
#table(wave1$w1depression) # n=669

# Create column called w1anxiety which defines a case where 2 ("anxiety") is recorded in any of the hepsy variables
wave1 <- wave1 %>%
  mutate(w1anxiety = case_when(hepsy1 == 2 ~ '1',
                               hepsy2 == 2 ~ '1',
                               hepsy3 == 2 ~ '1',
                               hepsy4 == 2 ~ '1',
                               hepsy5 == 2 ~ '1',
                               hepsy6 == 2 ~ '1',
                               hepsy7 == 2 ~ '1',
                               hepsy8 == 2 ~ '1',
                               hepsy9 == 2 ~ '1',
                               TRUE ~ '0'))
#table(wave1$w1anxiety) # n=522

# w1 smoking ever status: have you ever smoke cigarettes?
table(wave1$w1smokeever)
wave1$w1smokeever[wave1$w1smokeever == 2] <- 0 # no
wave1$w1smokeever[wave1$w1smokeever < 0] <- NA # refused/dont know/not applicable

# w1 smoking status: do you smoke cigarettes at all nowadays?
wave1$w1smokenow[wave1$w1smokenow == 2] <- "No" # no
wave1$w1smokenow[wave1$w1smokenow == 1] <- "Yes"
wave1$w1smokenow[wave1$w1smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
#table(wave1$w1smokeever, wave1$w1smokenow)

# w1 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave1$w1alcohol[wave1$w1alcohol == 1] <- "Twice a day or more"
wave1$w1alcohol[wave1$w1alcohol == 2] <- "Daily or almost daily"
wave1$w1alcohol[wave1$w1alcohol == 3] <- "Once or twice a week"
wave1$w1alcohol[wave1$w1alcohol == 4] <- "Once or twice a month"
wave1$w1alcohol[wave1$w1alcohol == 5] <- "Special occasions only"
wave1$w1alcohol[wave1$w1alcohol == 6] <- "Not at all"
wave1$w1alcohol[wave1$w1alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave1$w1alcohol)

# w1 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave1$w1vigorousphyact[wave1$w1vigorousphyact == 1] <- "more than once a week"
wave1$w1vigorousphyact[wave1$w1vigorousphyact == 2] <- "once a week"
wave1$w1vigorousphyact[wave1$w1vigorousphyact == 3] <- "one to three times a month"
wave1$w1vigorousphyact[wave1$w1vigorousphyact == 4] <- "hardly ever, or never"
wave1$w1vigorousphyact[wave1$w1vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave1$w1vigorousphyact)

# ELSA wave 2 data
# Wave 2 data collection took place June 2004 – July 2005

# IMPORT DATA
elsawave2 <- read.spss(elsawave2_file, to.data.frame = TRUE, use.value.labels = FALSE)

#glimpse(elsawave2)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave2 %>%
#  count()
# n=9,432

# check for duplicates in ID number
#elsawave2 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave2 <- select(elsawave2,
                idauniq, w2finstat = finstat, w2indout = w2indout, 
                hedim01, hedim02, hedim03, hedim04, hedim05, hedim06, hedim07,
                HePsy1, HePsy2, HePsy3, HePsy4, HePsy5, HePsy6,
                w2strokeage = HeAge, w2strokelast2yrm = HeAgeR, w2strokelast2yry = HeAgeRY, w2nstrokessincew1 = Henmst,
                w2strokeremainproblems = HePbs, w2disputew1stroke = HeDiaN8,
                w2psychiatricage = HeAgh, w2psychiatriclast2yrm = HeAghR, w2psychiatriclast2yry = HeAghRY, w2psychiatrciproblemlast2yr = HeYrc,
                w2region = gor, w2ethnicgroup = fqethnr,
                w2smokeever = HeSmk, w2smokenow = HESka, w2confirmsw1smokingstatus = HeSkd, w2disputew1smoking = HeSke,
                w2vigorousphyact = HeActa, w2alcohol = scako)

#White/non-white ethnicity
wave2$w2ethnicgroup <- factor(wave2$w2ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
#table(wave2$w2ethnicgroup)

#Region
str_squish(wave2$w2region)
wave2$w2region [wave2$w2region == "A "] <- "north east"
wave2$w2region [wave2$w2region == "B "] <- "north west"
wave2$w2region [wave2$w2region == "D "] <- "yorkshire and the humber"
wave2$w2region [wave2$w2region == "E "] <- "east midlands"
wave2$w2region [wave2$w2region == "F "] <- "west midlands"
wave2$w2region [wave2$w2region == "G "] <- "east of england"
wave2$w2region [wave2$w2region == "H "] <- "london"
wave2$w2region [wave2$w2region == "J "] <- "south east"
wave2$w2region [wave2$w2region == "K "] <- "south west"
wave2$w2region [wave2$w2region == -2] <- NA
wave2[wave2$idauniq==104274, "w2region"] <- NA  # changed to NA as appeared to be a blank space in this field
wave2[wave2$idauniq==116864, "w2region"] <- NA  # changed to NA as appeared to be a blank space in this field
wave2[wave2$idauniq==121006, "w2region"] <- NA  # changed to NA as appeared to be a blank space in this field
#table(wave2$w2region, useNA = "always")

# Stroke (doctor diagnosed)
#Create column called w2strokeany which defines a case where 8 is recorded in any of the hedi variables
wave2 <- wave2 %>% 
  mutate(w2strokeany = case_when(hedim01 == 8 ~ '1', 
                                 hedim02 == 8 ~ '1',
                                 hedim03 == 8 ~ '1',
                                 hedim04 == 8 ~ '1',
                                 hedim05 == 8 ~ '1',
                                 hedim06 == 8 ~ '1',
                                 hedim07 == 8 ~ '1',
                                 TRUE ~ '0'))
#table(wave2$w2strokeany)
#n=170

#Psychosis
#Create column called w2psychosisany which defines a case where 1 (hallucinations), 5 (schz), 6 (psychosis) is recorded in any of the hepsy variables
wave2 <- wave2 %>%
  mutate(w2psychosisany = case_when(HePsy1 == 1 ~ '1',
                                    HePsy1 == 5 ~ '1',
                                    HePsy1 == 6 ~ '1',
                                    HePsy2 == 1 ~ '1',
                                    HePsy2 == 5 ~ '1',
                                    HePsy2 == 6 ~ '1',
                                    HePsy3 == 1 ~ '1',
                                    HePsy3 == 5 ~ '1',
                                    HePsy3 == 6 ~ '1',
                                    HePsy4 == 1 ~ '1',
                                    HePsy4 == 5 ~ '1',
                                    HePsy4 == 6 ~ '1',
                                    HePsy5 == 1 ~ '1',
                                    HePsy5 == 5 ~ '1',
                                    HePsy5 == 6 ~ '1',
                                    HePsy6 == 1 ~ '1',
                                    HePsy6 == 5 ~ '1',
                                    HePsy6 == 6 ~ '1',
                                    TRUE ~ '0'))
#table(wave2$w2psychosisany)
#n=8

#w2 stroke in last two years, year and month
wave2$w2strokelast2yry[wave2$w2strokelast2yry < 0] <- NA # not applicable
wave2$w2strokelast2yrm[wave2$w2strokelast2yrm < 0] <- NA
#table(wave2$w2strokelast2yry)
#table(wave2$w2strokelast2yrm)

#w2 number of strokes since last visit
wave2$w2nstrokessincew1[wave2$w2nstrokessincew1 < 0] <- NA # not applicable
#table(wave2$w2nstrokessincew1)

# Remaining problems because of your strokes
wave2$w2strokeremainproblems[wave2$w2strokeremainproblems == 2] <- 0 #"no"
wave2$w2strokeremainproblems[wave2$w2strokeremainproblems < 0] <- NA # NA
#table(wave2$w2strokeremainproblems)

# Disputed w1 stroke reasons
wave2$w2disputew1stroke[wave2$w2disputew1stroke < 0] <- NA # refused, don't know, not applicable
wave2$w2disputew1stroke[wave2$w2disputew1stroke == 1] <- "never had"
wave2$w2disputew1stroke[wave2$w2disputew1stroke == 2] <- "no longer has"
wave2$w2disputew1stroke[wave2$w2disputew1stroke == 3] <- "did not have previously but has now"
#table(wave2$w2disputew1stroke)

#Create column called w2depression which defines a case where 3 ("depression") is recorded in any of the hepsy variables
wave2 <- wave2 %>%
  mutate(w2depression = case_when(HePsy1 == 3 ~ '1',
                                  HePsy2 == 3 ~ '1',
                                  HePsy3 == 3 ~ '1',
                                  HePsy4 == 3 ~ '1',
                                  HePsy5 == 3 ~ '1',
                                  HePsy6 == 3 ~ '1',
                                  TRUE ~ '0'))
#table(wave2$w2depression) # n=165

#Create column called w2anxiety which defines a case where 2 ("anxiety") is recorded in any of the hepsy variables
wave2 <- wave2 %>%
  mutate(w2anxiety = case_when(HePsy1 == 2 ~ '1',
                               HePsy2 == 2 ~ '1',
                               HePsy3 == 2 ~ '1',
                               HePsy4 == 2 ~ '1',
                               HePsy5 == 2 ~ '1',
                               HePsy6 == 2 ~ '1',
                               TRUE ~ '0'))
#table(wave2$w2anxiety) # n=143

#w2 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave2$w2vigorousphyact[wave2$w2vigorousphyact == 1] <- "more than once a week"
wave2$w2vigorousphyact[wave2$w2vigorousphyact == 2] <- "once a week"
wave2$w2vigorousphyact[wave2$w2vigorousphyact == 3] <- "one to three times a month"
wave2$w2vigorousphyact[wave2$w2vigorousphyact == 4] <- "hardly ever, or never"
wave2$w2vigorousphyact[wave2$w2vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave2$w2vigorousphyact)

#w2 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave2$w2alcohol[wave2$w2alcohol == 1] <- "Almost every day"
wave2$w2alcohol[wave2$w2alcohol == 2] <- "Five or six days a week"
wave2$w2alcohol[wave2$w2alcohol == 3] <- "Three or four days a week"
wave2$w2alcohol[wave2$w2alcohol == 4] <- "Once or twice a week"
wave2$w2alcohol[wave2$w2alcohol == 5] <- "Once or twice a month"
wave2$w2alcohol[wave2$w2alcohol == 6] <- "Once every couple of months"
wave2$w2alcohol[wave2$w2alcohol == 7] <- "Once or twice a year"
wave2$w2alcohol[wave2$w2alcohol == 8] <- "Not at all"
wave2$w2alcohol[wave2$w2alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave2$w2alcohol)

#w2 smoking ever status: have you ever smoke cigarettes? low numbers for this variable as not many people joined in w2 (and stauts already known for w1 participants)
wave2$w2smokeever[wave2$w2smokeever == 2] <- 0 # no
wave2$w2smokeever[wave2$w2smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave2$w2smokeever)

#w2 smoking status: do you smoke cigarettes at all nowadays?
wave2$w2smokenow[wave2$w2smokenow == 1] <- "Yes" # no
wave2$w2smokenow[wave2$w2smokenow == 2] <- "No" # no
wave2$w2smokenow[wave2$w2smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
#table(wave2$w2smokenow)

#w2 disputing w1 smoking: numbers and reasons
wave2$w2disputew1smoking[wave2$w2disputew1smoking == 1] <- "never smoked"
wave2$w2disputew1smoking[wave2$w2disputew1smoking == 2] <- "no longer smoked by w1"
wave2$w2disputew1smoking[wave2$w2disputew1smoking == 3] <- "stopped smoking between w1 and w2"
#table(wave2$w2confirmsw1smokingstatus, wave2$w2disputew1smoking)

# ELSA wave 3 data
# Wave 3 data collection took place May 2006 – August 2007

# IMPORT DATA
elsawave3 <- read.spss(elsawave3_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave3 %>%
#  count()
# n=9,771

# check for duplicates in ID number
#elsawave3 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave3 <- select(elsawave3,
                idauniq, w3finstat = finstat, w3indoutcome = w3indout, w3samplingstatus = sampsta,
                w3stroke = dhedimst, w3strokeage = heage, w3strokelast2yrm = heage, w3strokelast2yry = heagery, w3nstrokessincew2 = henmst,
                w3strokeremainproblems = hepbs, w3disputew2stroke = hedanst,
                halluc = hepsyha, schz = hepsysc, psychosis = hepsyps, w3depression = hepsyde, w3anxiety = hepsyan,
                w3region = GOR, w3ethnicgroup = fqethnr,
                w3smokeever = hesmk, w3smokenow = heska, w3confirmsw2smokingstatus = heskd, w3disputew2smoking = heske,
                w3vigorousphyact = heacta, w3alcohol = scako)

#RECODING AND LABELLING

#White/non-white ethnicity
wave3$w3ethnicgroup <- factor(wave3$w3ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
#table(wave3$w3ethnicgroup)

#Region
str_squish(wave3$w3region)
wave3$w3region [wave3$w3region == "A "] <- "north east"
wave3$w3region [wave3$w3region == "B "] <- "north west"
wave3$w3region [wave3$w3region == "D "] <- "yorkshire and the humber"
wave3$w3region [wave3$w3region == "E "] <- "east midlands"
wave3$w3region [wave3$w3region == "F "] <- "west midlands"
wave3$w3region [wave3$w3region == "G "] <- "east of england"
wave3$w3region [wave3$w3region == "H "] <- "london"
wave3$w3region [wave3$w3region == "J "] <- "south east"
wave3$w3region [wave3$w3region == "K "] <- "south west"
wave3$w3region [wave3$w3region == -2] <- NA
wave3[wave3$idauniq==104995, "w3region"] <- NA # recoded as field contained just a blank space
#table(wave3$w3region, useNA = "always")

#Stroke age and year and number, recoding NA values
wave3$w3strokeage[wave3$w3strokeage <= 0] <- NA
wave3$w3strokelast2yry[wave3$w3strokelast2yry <= 0] <- NA
wave3$w3nstrokessincew2[wave3$w3nstrokessincew2 < 0] <- NA # not applicable

# Stroke (newly reported in w3) # note that these variables are coded differently in the data files compared to previous waves 
wave3$w3stroke[wave3$w3stroke==-1] <- 0 # not applicable changed to no
#table(wave3$w3stroke)
#n=111

# Hallucinations
wave3$halluc[wave3$halluc==-1] <- 0 # not applicable changed to no
#table(wave3$halluc)
#n=14

# Schizophrenia
wave3$schz[wave3$schz==-1] <- 0 # not applicable changed to no
table(wave3$schz)
#n=14

# Psychosis
wave3$psychosis[wave3$psychosis==-1] <-0 # not applicable changed to no
#table(wave3$psychosis)
#n=10

#Create column called w3psychosisany which defines a case where 1 is recorded in w3 halluc, schz or psychosis variables
wave3 <- wave3 %>% 
  mutate(w3psychosisany = case_when(halluc == 1 ~ '1', 
                                    schz == 1 ~ '1',
                                    psychosis == 1 ~ '1',
                                    TRUE ~ '0'))
#table(wave3$w3psychosisany)
#n=32

# Remaining problems because of your strokes
wave3$w3strokeremainproblems[wave3$w3strokeremainproblems == 2] <- 0 #"no"
wave3$w3strokeremainproblems[wave3$w3strokeremainproblems < 0] <- NA # NA
#table(wave3$w3strokeremainproblems)

# Disputed w2 stroke reasons
wave3$w3disputew2stroke[wave3$w3disputew2stroke < 0] <- NA # refused, don't know, not applicable
wave3$w3disputew2stroke[wave3$w3disputew2stroke == 1] <- "never had"
wave3$w3disputew2stroke[wave3$w3disputew2stroke == 2] <- "no longer has"
wave3$w3disputew2stroke[wave3$w3disputew2stroke == 3] <- "did not have previously but has now"
wave3$w3disputew2stroke[wave3$w3disputew2stroke == 4] <- "misdiagnosed"
#table(wave3$w3disputew2stroke)

#Anxiety
wave3$w3anxiety[wave3$w3anxiety==-1] <-0 # not applicable changed to no
#table(wave3$w3anxiety) # n=419

#Depression
wave3$w3depression[wave3$w3depression==-1] <- 0 # not applicable changed to no
#table(wave3$w3depression) # n=560

#w3 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave3$w3vigorousphyact[wave3$w3vigorousphyact == 1] <- "more than once a week"
wave3$w3vigorousphyact[wave3$w3vigorousphyact == 2] <- "once a week"
wave3$w3vigorousphyact[wave3$w3vigorousphyact == 3] <- "one to three times a month"
wave3$w3vigorousphyact[wave3$w3vigorousphyact == 4] <- "hardly ever, or never"
wave3$w3vigorousphyact[wave3$w3vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave3$w3vigorousphyact)

#w3 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave3$w3alcohol[wave3$w3alcohol == 1] <- "Almost every day"
wave3$w3alcohol[wave3$w3alcohol == 2] <- "Five or six days a week"
wave3$w3alcohol[wave3$w3alcohol == 3] <- "Three or four days a week"
wave3$w3alcohol[wave3$w3alcohol == 4] <- "Once or twice a week"
wave3$w3alcohol[wave3$w3alcohol == 5] <- "Once or twice a month"
wave3$w3alcohol[wave3$w3alcohol == 6] <- "Once every couple of months"
wave3$w3alcohol[wave3$w3alcohol == 7] <- "Once or twice a year"
wave3$w3alcohol[wave3$w3alcohol == 8] <- "Not at all"
wave3$w3alcohol[wave3$w3alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave3$w3alcohol)

#w3 smoking ever status: have you ever smoke cigarettes?
wave3$w3smokeever[wave3$w3smokeever == 2] <- 0 # no
wave3$w3smokeever[wave3$w3smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave3$w3smokeever)

#w3 smoking status: do you smoke cigarettes at all nowadays?
wave3$w3smokenow[wave3$w3smokenow == 1] <- "Yes" # no
wave3$w3smokenow[wave3$w3smokenow == 2] <- "No" # no
wave3$w3smokenow[wave3$w3smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
wave3$w3smokenow[wave3$w3smokenow == -8] <- "No" # don't know
wave3$w3smokenow[wave3$w3smokenow == -9] <- "No" # refused, coded as no given no evidence of smoking now
#table(wave3$w3smokeever, wave3$w3smokenow)

#w3 disputing w2 smoking: numbers and reasons
wave3$w3disputew2smoking[wave3$w3disputew2smoking == 1] <- "never smoked"
wave3$w3disputew2smoking[wave3$w3disputew2smoking == 2] <- "no longer smoked by w2"
wave3$w3disputew2smoking[wave3$w3disputew2smoking == 3] <- "stopped smoking between w2 and w3"
#table(wave3$w3confirmsw2smokingstatus, wave3$w3disputew2smoking)

# ELSA wave 4 data
#Wave 4 data collection took place May 2008 – July 2009

# IMPORT DATA
elsawave4 <- read.spss(elsawave4_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave4 %>%
#  count()
# n=11,050

# check for duplicates in ID number
#elsawave4 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave4 <- select(elsawave4,
                idauniq, w4finstat = finstat4, w4indoutcome = outindw4, w4samplingstatus = samptyp,
                w4stroke = hedimst, w4strokeage = heage, w4strokelast2yrm = heager, w4strokelast2yry = heagery, w4nstrokessincew3 = henmst,
                w4strokeremainproblems = hepbs, w4disputew3stroke = hedanst,
                halluc = hepsyha, schz = hepsysc, psychosis = hepsyps, w4depression = hepsyde, w4anxiety = hepsyan,
                w4region = GOR, w4ethnicgroup = fqethnr,
                w4smokeever = hesmk, w4smokenow = heska, w4confirmsw3smokingstatus = heskd, w4disputew3smoking = heske,
                w4vigorousphyact = heacta, w4alcohol = scako)

#RECODING AND LABELLING

str_squish(wave4$w4region)
wave4$w4region [wave4$w4region == "A "] <- "north east"
wave4$w4region [wave4$w4region == "B "] <- "north west"
wave4$w4region [wave4$w4region == "D "] <- "yorkshire and the humber"
wave4$w4region [wave4$w4region == "E "] <- "east midlands"
wave4$w4region [wave4$w4region == "F "] <- "west midlands"
wave4$w4region [wave4$w4region == "G "] <- "east of england"
wave4$w4region [wave4$w4region == "H "] <- "london"
wave4$w4region [wave4$w4region == "J "] <- "south east"
wave4$w4region [wave4$w4region == "K "] <- "south west"
wave4$w4region [wave4$w4region == -2] <- NA
table(wave4$w4region, useNA = "always")

#White/non-white ethnicity
wave4$w4ethnicgroup <- factor(wave4$w4ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
table(wave4$w4ethnicgroup)

#w4 stroke age
wave4$w4strokeage[wave4$w4strokeage <= 0] <- NA # not applicable
#table(wave4$w4strokeage)

#w4 stroke year
wave4$w4strokelast2yry[wave4$w4strokelast2yry <= 0] <- NA # not applicable
#table(wave4$w4strokelast2yry)

#w4 number of strokes
wave4$w4nstrokessincew3[wave4$w4nstrokessincew3 < 0] <- NA # not applicable
#table(wave4$w4nstrokessincew3)

# Stroke (newly reported)
wave4$w4stroke[wave4$w4stroke==-1] <- 0 # Not applicable
wave4$w4stroke[wave4$w4stroke==-9] <- 0 # "refused"
wave4$w4stroke[wave4$w4stroke==-8] <- 0 # "don't know"
#table(wave4$w4stroke) #n=183

# Hallucinations
wave4$halluc[wave4$halluc==-1] <- 0 # Not applicable
wave4$halluc[wave4$halluc==-8] <- 0 # "don't know"
wave4$halluc[wave4$halluc==-2] <- 0 # interview tech error
#table(wave4$halluc) #n=19

# Schizophrenia
wave4$schz[wave4$schz==-1] <- 0 # Not applicable
wave4$schz[wave4$schz==-8] <- 0 # "don't know"
wave4$schz[wave4$schz==-2] <- 0 # interview tech error
#table(wave4$schz) #n=14

# Psychosis
wave4$psychosis[wave4$psychosis==-1] <- 0 # Not applicable
wave4$psychosis[wave4$psychosis==-8] <- 0 # "don't know"
wave4$psychosis[wave4$psychosis==-2] <- 0 # interview tech error
#table(wave4$psychosis) #n=10

#Create column called w4psychosisany which defines a case where 1 is recorded in w4 halluc, schz or psychosis variables
wave4 <- wave4 %>% 
  mutate(w4psychosisany = case_when(halluc == 1 ~ '1', 
                                    schz == 1 ~ '1',
                                    psychosis == 1 ~ '1',
                                    TRUE ~ '0'))
#table(wave4$w4psychosisany) #n=32

# Remaining problems because of your strokes
wave4$w4strokeremainproblems[wave4$w4strokeremainproblems == 2] <- 0 #"no"
wave4$w4strokeremainproblems[wave4$w4strokeremainproblems < 0] <- NA # NA
#table(wave4$w4strokeremainproblems)

# Disputed w3 stroke reasons
wave4$w4disputew3stroke[wave4$w4disputew3stroke < 0] <- NA # refused, don't know, not applicable
wave4$w4disputew3stroke[wave4$w4disputew3stroke == 1] <- "never had"
wave4$w4disputew3stroke[wave4$w4disputew3stroke == 2] <- "no longer has"
wave4$w4disputew3stroke[wave4$w4disputew3stroke == 3] <- "did not have previously but has now"
wave4$w4disputew3stroke[wave4$w4disputew3stroke == 4] <- "misdiagnosed"
#table(wave4$w4disputew3stroke)

#Anxiety
wave4$w4anxiety[wave4$w4anxiety < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave4$w4anxiety) # n=535

#Depression
wave4$w4depression[wave4$w4depression < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave4$w4depression) # n=681

#w4 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave4$w4vigorousphyact[wave4$w4vigorousphyact == 1] <- "more than once a week"
wave4$w4vigorousphyact[wave4$w4vigorousphyact == 2] <- "once a week"
wave4$w4vigorousphyact[wave4$w4vigorousphyact == 3] <- "one to three times a month"
wave4$w4vigorousphyact[wave4$w4vigorousphyact == 4] <- "hardly ever, or never"
wave4$w4vigorousphyact[wave4$w4vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave4$w4vigorousphyact)

#w4 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave4$w4alcohol[wave4$w4alcohol == 1] <- "Almost every day"
wave4$w4alcohol[wave4$w4alcohol == 2] <- "Five or six days a week"
wave4$w4alcohol[wave4$w4alcohol == 3] <- "Three or four days a week"
wave4$w4alcohol[wave4$w4alcohol == 4] <- "Once or twice a week"
wave4$w4alcohol[wave4$w4alcohol == 5] <- "Once or twice a month"
wave4$w4alcohol[wave4$w4alcohol == 6] <- "Once every couple of months"
wave4$w4alcohol[wave4$w4alcohol == 7] <- "Once or twice a year"
wave4$w4alcohol[wave4$w4alcohol == 8] <- "Not at all"
wave4$w4alcohol[wave4$w4alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave4$w4alcohol)

#w4 smoking ever status: have you ever smoke cigarettes?
wave4$w4smokeever[wave4$w4smokeever == 2] <- 0 # no
wave4$w4smokeever[wave4$w4smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave4$w4smokeever)

#w4 smoking status: do you smoke cigarettes at all nowadays?
wave4$w4smokenow[wave4$w4smokenow == 1] <- "Yes" # no
wave4$w4smokenow[wave4$w4smokenow == 2] <- "No" # no
wave4$w4smokenow[wave4$w4smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
wave4$w4smokenow[wave4$w4smokenow == -2] <- "No" # schedule not applicable, most likely due to reporting never smoking
wave4$w4smokenow[wave4$w4smokenow == -8] <- "No" # don't know
wave4$w4smokenow[wave4$w4smokenow == -9] <- "No" # refused, coded as no given no evidence of smoking now
#table(wave4$w4smokeever, wave4$w4smokenow)

#w4 disputing w3 smoking: numbers and reasons
wave4$w4disputew3smoking[wave4$w4disputew3smoking == 1] <- "never smoked"
wave4$w4disputew3smoking[wave4$w4disputew3smoking == 2] <- "no longer smoked by w3"
wave4$w4disputew3smoking[wave4$w4disputew3smoking == 3] <- "stopped smoking between w3 and w4"
#table(wave4$w4confirmsw3smokingstatus, wave4$w4disputew3smoking)

# ELSA wave 5 data
#Wave 5 data collection took place June 2010 – July 2011

# IMPORT DATA
elsawave5 <- read.spss(elsawave5_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
# elsawave5 %>%
#  count()
# n=10,274

# check for duplicates in ID number
# elsawave5 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave5 <- select(elsawave5,
                idauniq, w5finstat = finstatw5, w5indoutcome = w5indout, w5samplingstatus = samptyp,
                w5stroke = hedimst, w5strokeage = heage, w5strokelast2yrm = heager, w5strokelast2yry = heagery, w5nstrokessincew4 = henmst,
                w5strokeremainproblems = hepbs, w5disputew4stroke = hedanst,
                halluc = hepsyha, schz = hepsysc, psychosis = hepsyps, w5depression = hepsyde, w5anxiety = hepsyan,               
                w5region = GOR, w5ethnicgroup = fqethnr,                
                w5smokeever = hesmk, w5smokenow = heska, w5confirmsw4smokingstatus = heskd, w5disputew4smoking = heske,
                w5vigorousphyact = heacta, w5alcohol = scako)

#RECODING AND LABELLING

#White/non-white ethnicity
wave5$w5ethnicgroup <- factor(wave5$w5ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
#table(wave5$w5ethnicgroup)

#Region
str_squish(wave5$w5region)
wave5$w5region [wave5$w5region == "A "] <- "north east"
wave5$w5region [wave5$w5region == "B "] <- "north west"
wave5$w5region [wave5$w5region == "D "] <- "yorkshire and the humber"
wave5$w5region [wave5$w5region == "E "] <- "east midlands"
wave5$w5region [wave5$w5region == "F "] <- "west midlands"
wave5$w5region [wave5$w5region == "G "] <- "east of england"
wave5$w5region [wave5$w5region == "H "] <- "london"
wave5$w5region [wave5$w5region == "J "] <- "south east"
wave5$w5region [wave5$w5region == "K "] <- "south west"
wave5$w5region [wave5$w5region == -2] <- NA
wave5[wave5$idauniq==100005, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==102077, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==103445, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==104385, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==104956, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==106165, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==110960, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==112303, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==112877, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==118377, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==118465, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==118700, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==119160, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==119164, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==120644, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==150907, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==150921, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==151196, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==162012, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==162038, "w5region"] <- NA # changed to NA as contained blank space
wave5[wave5$idauniq==703440, "w5region"] <- NA # changed to NA as contained blank space
#table(wave5$w5region, useNA = "always")

#w5 stroke age
wave5$w5strokeage[wave5$w5strokeage <= 0] <- NA # not applicable
#table(wave5$w5strokeage)

#w5 stroke year
wave5$w5strokelast2yry[wave5$w5strokelast2yry <= 0] <- NA # not applicable
#table(wave5$w5strokelast2yry)

#w5 number of strokes
wave5$w5nstrokessincew4[wave5$w5nstrokessincew4 < 0] <- NA # not applicable
#table(wave5$w5nstrokessincew4)

# Stroke (newly reported)
wave5$w5stroke[wave5$w5stroke==-1] <- 0 # Not applicable
wave5$w5stroke[wave5$w5stroke==-9] <- 0 # "refused"
wave5$w5stroke[wave5$w5stroke==-8] <- 0 # "don't know"
#table(wave5$w5stroke) #n=149

# Hallucinations
wave5$halluc[wave5$halluc==-1] <- 0 # Not applicable
wave5$halluc[wave5$halluc==-9] <- 0 # "refused"
wave5$halluc[wave5$halluc==-8] <- 0 # "don't know"
wave5$halluc[wave5$halluc==-2] <- 0 # interview tech error
# table(wave5$halluc) #n=14

# Schizophrenia
wave5$schz[wave5$schz==-1] <- 0 # Not applicable
wave5$schz[wave5$schz==-9] <- 0 # "refused"
wave5$schz[wave5$schz==-8] <- 0 # "don't know"
wave5$schz[wave5$schz==-2] <- 0 # interview tech error
#table(wave5$schz) #n=11

# Psychosis
wave5$psychosis[wave5$psychosis==-1] <- 0 # Not applicable
wave5$psychosis[wave5$psychosis==-9] <- 0 # "refused"
wave5$psychosis[wave5$psychosis==-8] <- 0 # "don't know"
wave5$psychosis[wave5$psychosis==-2] <- 0 # interview tech error
#table(wave5$psychosis) #n=12

#Create column called w5psychosisany which defines a case where 1 is recorded in w5 halluc, schz or psychosis variables
wave5 <- wave5 %>% 
  mutate(w5psychosisany = case_when(halluc == 1 ~ '1', 
                                    schz == 1 ~ '1',
                                    psychosis == 1 ~ '1',
                                    TRUE ~ '0'))
#table(wave5$w5psychosisany) #n=28

# Remaining problems because of your strokes
wave5$w5strokeremainproblems[wave5$w5strokeremainproblems == 2] <- 0 #"no"
wave5$w5strokeremainproblems[wave5$w5strokeremainproblems < 0] <- NA # NA
#table(wave5$w5strokeremainproblems)

# Disputed w4 stroke reasons
wave5$w5disputew4stroke[wave5$w5disputew4stroke < 0] <- NA # refused, don't know, not applicable
wave5$w5disputew4stroke[wave5$w5disputew4stroke == 1] <- "never had"
wave5$w5disputew4stroke[wave5$w5disputew4stroke == 2] <- "no longer has"
wave5$w5disputew4stroke[wave5$w5disputew4stroke == 3] <- "did not have previously but has now"
wave5$w5disputew4stroke[wave5$w5disputew4stroke == 4] <- "misdiagnosed"
#table(wave5$w5disputew4stroke)

#Anxiety
wave5$w5anxiety[wave5$w5anxiety < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave5$w5anxiety) # n=546

#Depression
wave5$w5depression[wave5$w5depression < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave5$w5depression) # n=685

#w5 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave5$w5vigorousphyact[wave5$w5vigorousphyact == 1] <- "more than once a week"
wave5$w5vigorousphyact[wave5$w5vigorousphyact == 2] <- "once a week"
wave5$w5vigorousphyact[wave5$w5vigorousphyact == 3] <- "one to three times a month"
wave5$w5vigorousphyact[wave5$w5vigorousphyact == 4] <- "hardly ever, or never"
wave5$w5vigorousphyact[wave5$w5vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave5$w5vigorousphyact)

#w5 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave5$w5alcohol[wave5$w5alcohol == 1] <- "Almost every day"
wave5$w5alcohol[wave5$w5alcohol == 2] <- "Five or six days a week"
wave5$w5alcohol[wave5$w5alcohol == 3] <- "Three or four days a week"
wave5$w5alcohol[wave5$w5alcohol == 4] <- "Once or twice a week"
wave5$w5alcohol[wave5$w5alcohol == 5] <- "Once or twice a month"
wave5$w5alcohol[wave5$w5alcohol == 6] <- "Once every couple of months"
wave5$w5alcohol[wave5$w5alcohol == 7] <- "Once or twice a year"
wave5$w5alcohol[wave5$w5alcohol == 8] <- "Not at all"
wave5$w5alcohol[wave5$w5alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave5$w5alcohol)

#w5 smoking ever status: have you ever smoke cigarettes?
wave5$w5smokeever[wave5$w5smokeever == 2] <- 0 # no
wave5$w5smokeever[wave5$w5smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave5$w5smokeever)

#w5 smoking status: do you smoke cigarettes at all nowadays?
wave5$w5smokenow[wave5$w5smokenow == 1] <- "Yes" # no
wave5$w5smokenow[wave5$w5smokenow == 2] <- "No" # no
wave5$w5smokenow[wave5$w5smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
wave5$w5smokenow[wave5$w5smokenow == -2] <- "No" # schedule not applicable, most likely due to reporting never smoking
wave5$w5smokenow[wave5$w5smokenow == -8] <- "No" # don't know
wave5$w5smokenow[wave5$w5smokenow == -9] <- "No" # refused, coded as no given no evidence of smoking now
#table(wave5$w5smokeever, wave5$w5smokenow)

#w5 disputing w4 smoking: numbers and reasons
wave5$w5disputew4smoking[wave5$w5disputew4smoking == 1] <- "never smoked"
wave5$w5disputew4smoking[wave5$w5disputew4smoking == 2] <- "no longer smoked by w4"
wave5$w5disputew4smoking[wave5$w5disputew4smoking == 3] <- "stopped smoking between w4 and w5"
#table(wave5$w5confirmsw4smokingstatus, wave5$w5disputew4smoking)

# ELSA wave 6 data 
# wave 6 is not updated with mortality data
# Wave 6 data collection took place May 2012 – June 2013

# IMPORT DATA
elsawave6 <- read.spss(elsawave6_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave6 %>%
#  count()
# n=10,274

# check for duplicates in ID number
#elsawave6 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave6 <- select(elsawave6,
                idauniq, w6dobyear = Indobyr, w6sex = indsex, w6finstat = finstatw6, w6indoutcome = w6indout, w6samplingstatus = samptyp,
                w6stroke = hedimst, w6strokeage = HeAge, w6strokelast2yrm = HeAgeR, w6strokelast2yry = HeAgeRY, w6nstrokessincew5 = HeNmSt,
                w6disputew5stroke = hedanst,
                halluc = hepsyha, schz = hepsysc, psychosis = hepsyps, w6depression = hepsyde, w6anxiety = hepsyan,               
                w6region = GOR, w6ethnicgroup = Fqethnr,                
                w6smokeever = HeSmk, w6smokenow = HESka, w6confirmsw5smokingstatus = HeSkd, w6disputew5smoking = HeSke,
                w6vigorousphyact = HeActa, w6alcohol = scako)

#RECODING AND LABELLING

#White/non-white ethnicity
wave6$w6ethnicgroup <- factor(wave6$w6ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
#table(wave6$w6ethnicgroup)

#Region
wave6$w6region <- str_squish(wave6$w6region)
wave6$w6region [wave6$w6region == "E12000001"] <- "north east"
wave6$w6region [wave6$w6region == "E12000002"] <- "north west"
wave6$w6region [wave6$w6region == "E12000003"] <- "yorkshire and the humber"
wave6$w6region [wave6$w6region == "E12000004"] <- "east midlands"
wave6$w6region [wave6$w6region == "E12000005"] <- "west midlands"
wave6$w6region [wave6$w6region == "E12000006"] <- "east of england"
wave6$w6region [wave6$w6region == "E12000007"] <- "london"
wave6$w6region [wave6$w6region == "E12000008"] <- "south east"
wave6$w6region [wave6$w6region == "E12000009"] <- "south west"
wave6$w6region [wave6$w6region == "S99999999"] <- "scotland"
wave6$w6region [wave6$w6region == "W99999999"] <- "wales"
wave6[wave6$idauniq==104385, "w6region"] <- NA # these have been manually set to NA as the field was populated with a blank space for these participants
wave6[wave6$idauniq==104900, "w6region"] <- NA
wave6[wave6$idauniq==105692, "w6region"] <- NA
wave6[wave6$idauniq==111085, "w6region"] <- NA
wave6[wave6$idauniq==111109, "w6region"] <- NA
wave6[wave6$idauniq==111223, "w6region"] <- NA
wave6[wave6$idauniq==112303, "w6region"] <- NA
wave6[wave6$idauniq==162368, "w6region"] <- NA
wave6[wave6$idauniq==164926, "w6region"] <- NA
#table(wave6$w6region, useNA = "always")

#Create column called w6psychosisany which defines a case where 1 is recorded in w6 halluc, schz or psychosis variables
wave6 <- wave6 %>% 
  mutate(w6psychosisany = case_when(halluc == 1 ~ '1', 
                                    schz == 1 ~ '1',
                                    psychosis == 1 ~ '1',
                                    TRUE ~ '0'))
#table(wave6$w6psychosisany) #n=50

# Stroke (newly reported): recoding
wave6$w6stroke[wave6$w6stroke==-9] <- 0 # "refused"
wave6$w6stroke[wave6$w6stroke==-8] <- 0 # "don't know"
#table(wave6$w6stroke) # n=158

# Disputed w5 stroke reasons
wave6$w6disputew5stroke[wave6$w6disputew5stroke < 0] <- NA # refused, don't know, not applicable
wave6$w6disputew5stroke[wave6$w6disputew5stroke == 1] <- "never had"
wave6$w6disputew5stroke[wave6$w6disputew5stroke == 2] <- "no longer has"
wave6$w6disputew5stroke[wave6$w6disputew5stroke == 3] <- "did not have previously but has now"
wave6$w6disputew5stroke[wave6$w6disputew5stroke == 4] <- "misdiagnosed"
#table(wave6$w6disputew5stroke)

#w6 stroke age
wave6$w6strokeage[wave6$w6strokeage == -1] <- NA # not applicable
#table(wave6$w6strokeage)

#w6 stroke age year
wave6$w6strokelast2yry[wave6$w6strokelast2yry < 0] <- NA # not applicable
#table(wave6$w6strokelast2yry) 

#w6 anxiety
wave6$w6anxiety[wave6$w6anxiety < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave6$w6anxiety) # n=587

#w6 depression
wave6$w6depression[wave6$w6depression < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave6$w6depression) # n=727

#w6 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave6$w6vigorousphyact[wave6$w6vigorousphyact == 1] <- "more than once a week"
wave6$w6vigorousphyact[wave6$w6vigorousphyact == 2] <- "once a week"
wave6$w6vigorousphyact[wave6$w6vigorousphyact == 3] <- "one to three times a month"
wave6$w6vigorousphyact[wave6$w6vigorousphyact == 4] <- "hardly ever, or never"
wave6$w6vigorousphyact[wave6$w6vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave6$w6vigorousphyact)

#w6 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave6$w6alcohol[wave6$w6alcohol == 1] <- "Almost every day"
wave6$w6alcohol[wave6$w6alcohol == 2] <- "Five or six days a week"
wave6$w6alcohol[wave6$w6alcohol == 3] <- "Three or four days a week"
wave6$w6alcohol[wave6$w6alcohol == 4] <- "Once or twice a week"
wave6$w6alcohol[wave6$w6alcohol == 5] <- "Once or twice a month"
wave6$w6alcohol[wave6$w6alcohol == 6] <- "Once every couple of months"
wave6$w6alcohol[wave6$w6alcohol == 7] <- "Once or twice a year"
wave6$w6alcohol[wave6$w6alcohol == 8] <- "Not at all"
wave6$w6alcohol[wave6$w6alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave6$w6alcohol)

#w6 smoking ever status: have you ever smoke cigarettes?
wave6$w6smokeever[wave6$w6smokeever == 2] <- 0 # no
wave6$w6smokeever[wave6$w6smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave6$w6smokeever)

#w6 smoking status: do you smoke cigarettes at all nowadays?
wave6$w6smokenow[wave6$w6smokenow == 1] <- "Yes" # no
wave6$w6smokenow[wave6$w6smokenow == 2] <- "No" # no
wave6$w6smokenow[wave6$w6smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
wave6$w6smokenow[wave6$w6smokenow == -2] <- "No" # schedule not applicable, most likely due to reporting never smoking
wave6$w6smokenow[wave6$w6smokenow == -8] <- "No" # don't know
wave6$w6smokenow[wave6$w6smokenow == -9] <- "No" # refused, coded as no given no evidence of smoking now
#table(wave6$w6smokeever, wave6$w6smokenow)

#w6 disputing w5 smoking: numbers and reasons
wave6$w6disputew5smoking[wave6$w6disputew5smoking == 1] <- "no longer smoked by w5"
wave6$w6disputew5smoking[wave6$w6disputew5smoking == 2] <- "stopped smoking between w5 and w6"
#table(wave6$w6confirmsw5smokingstatus, wave6$w6disputew5smoking)

#w6 number of strokes since w5
wave6$w6nstrokessincew5[wave6$w6nstrokessincew5 < 0] <- NA # not applicable
#table(wave6$w6nstrokessincew5)

# ELSA wave 7 data 
# wave 7 is not updated with mortality data
# Wave 7 data collection took place June 2014 – May 2015

# IMPORT DATA
elsawave7 <- read.spss(elsawave7_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave7 %>%
#  count()
# n=9,666

# check for duplicates in ID number
#elsawave7 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave7 <- select(elsawave7,
                idauniq, w7dobyear = Indobyr, w7sex = indsex, w7finstat = finstatw7, w7indoutcome = w7indout, w7samplingstatus = samptyp,
                w7stroke = hedimst, w7strokeage = HeAge, w7strokelast2yrm = HeAgeR, w7strokelast2yry = HeAgeRY, w7nstrokessincew6 = HeNmSt,
                w7disputew6stroke = hedanst,
                halluc = hepsyha, schz = hepsysc, psychosis = hepsyps, w7depression = hepsyde, w7anxiety = hepsyan,               
                w7region = gor, w7ethnicgroup = Fqethnr,                
                w7smokeever = HeSmk, w7smokenow = HESka, w7confirmsw6smokingstatus = HeSkd, w7disputew6smoking = HeSke,
                w7vigorousphyact = HeActa, w7alcohol = scako)

#RECODING AND LABELLING

#White/non-white ethnicity
wave7$w7ethnicgroup <- factor(wave7$w7ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
#table(wave7$w7ethnicgroup)

#Region
wave7$w7region <- str_squish(wave7$w7region)
wave7$w7region [wave7$w7region == "E12000001"] <- "north east"
wave7$w7region [wave7$w7region == "E12000002"] <- "north west"
wave7$w7region [wave7$w7region == "E12000003"] <- "yorkshire and the humber"
wave7$w7region [wave7$w7region == "E12000004"] <- "east midlands"
wave7$w7region [wave7$w7region == "E12000005"] <- "west midlands"
wave7$w7region [wave7$w7region == "E12000006"] <- "east of england"
wave7$w7region [wave7$w7region == "E12000007"] <- "london"
wave7$w7region [wave7$w7region == "E12000008"] <- "south east"
wave7$w7region [wave7$w7region == "E12000009"] <- "south west"
wave7$w7region [wave7$w7region == "S99999999"] <- "scotland"
wave7$w7region [wave7$w7region == "W99999999"] <- "wales"
#table(wave7$w7region, useNA = "always")

#Create column called w7psychosisany which defines a case where 1 is recorded in w7 halluc, schz or psychosis variables
wave7 <- wave7 %>% 
  mutate(w7psychosisany = case_when(halluc == 1 ~ '1', 
                                    schz == 1 ~ '1',
                                    psychosis == 1 ~ '1',
                                    TRUE ~ '0'))
#table(wave7$w7psychosisany) #n=42

# Stroke (newly reported): recoding
wave7$w7stroke[wave7$w7stroke==-9] <- 0 # "refused"
wave7$w7stroke[wave7$w7stroke==-8] <- 0 # "don't know"
#table(wave7$w7stroke) # n=138

# Disputed w6 stroke reasons
wave7$w7disputew6stroke[wave7$w7disputew6stroke < 0] <- NA # refused, don't know, not applicable
wave7$w7disputew6stroke[wave7$w7disputew6stroke == 1] <- "never had"
wave7$w7disputew6stroke[wave7$w7disputew6stroke == 2] <- "no longer has"
wave7$w7disputew6stroke[wave7$w7disputew6stroke == 3] <- "did not have previously but has now"
wave7$w7disputew6stroke[wave7$w7disputew6stroke == 4] <- "misdiagnosed"
#table(wave7$w7disputew6stroke)

#w7 stroke age
wave7$w7strokeage[wave7$w7strokeage == -1] <- NA # not applicable
#table(wave7$w7strokeage)

#w7 stroke age year
wave7$w7strokelast2yry[wave7$w7strokelast2yry < 0] <- NA # not applicable
#table(wave7$w7strokelast2yry) 

#w7 anxiety
wave7$w7anxiety[wave7$w7anxiety < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave7$w7anxiety) # n=521

#w7 depression
wave7$w7depression[wave7$w7depression < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave7$w7depression) # n=692

#w7 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave7$w7vigorousphyact[wave7$w7vigorousphyact == 1] <- "more than once a week"
wave7$w7vigorousphyact[wave7$w7vigorousphyact == 2] <- "once a week"
wave7$w7vigorousphyact[wave7$w7vigorousphyact == 3] <- "one to three times a month"
wave7$w7vigorousphyact[wave7$w7vigorousphyact == 4] <- "hardly ever, or never"
wave7$w7vigorousphyact[wave7$w7vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave7$w7vigorousphyact)

#w7 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave7$w7alcohol[wave7$w7alcohol == 1] <- "Almost every day"
wave7$w7alcohol[wave7$w7alcohol == 2] <- "Five or six days a week"
wave7$w7alcohol[wave7$w7alcohol == 3] <- "Three or four days a week"
wave7$w7alcohol[wave7$w7alcohol == 4] <- "Once or twice a week"
wave7$w7alcohol[wave7$w7alcohol == 5] <- "Once or twice a month"
wave7$w7alcohol[wave7$w7alcohol == 6] <- "Once every couple of months"
wave7$w7alcohol[wave7$w7alcohol == 7] <- "Once or twice a year"
wave7$w7alcohol[wave7$w7alcohol == 8] <- "Not at all"
wave7$w7alcohol[wave7$w7alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave7$w7alcohol)

#w7 smoking ever status: have you ever smoke cigarettes?
wave7$w7smokeever[wave7$w7smokeever == 2] <- 0 # no
wave7$w7smokeever[wave7$w7smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave7$w7smokeever)

#w7 smoking status: do you smoke cigarettes at all nowadays?
wave7$w7smokenow[wave7$w7smokenow == 1] <- "Yes" # no
wave7$w7smokenow[wave7$w7smokenow == 2] <- "No" # no
wave7$w7smokenow[wave7$w7smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
wave7$w7smokenow[wave7$w7smokenow == -2] <- "No" # schedule not applicable, most likely due to reporting never smoking
wave7$w7smokenow[wave7$w7smokenow == -8] <- "No" # don't know
wave7$w7smokenow[wave7$w7smokenow == -9] <- "No" # refused, coded as no given no evidence of smoking now
#table(wave7$w7smokeever, wave7$w7smokenow)

#w7 disputing w6 smoking: numbers and reasons
wave7$w7disputew6smoking[wave7$w7disputew6smoking == 1] <- "no longer smoked by w6"
wave7$w7disputew6smoking[wave7$w7disputew6smoking == 2] <- "stopped smoking between w6 and w7"
wave7$w7disputew6smoking[wave7$w7disputew6smoking == 3] <- "stopped smoking between w6 and w7"
#table(wave7$w7confirmsw6smokingstatus, wave7$w7disputew6smoking)

#w7 number of strokes since w6
wave7$w7nstrokessincew6[wave7$w7nstrokessincew6 < 0] <- NA # not applicable
#table(wave7$w7nstrokessincew6)

# ELSA wave 8 data 
# wave 8 is not updated with mortality data
#Wave 8 data collection took place May 2016 – June 2017

# IMPORT DATA
elsawave8 <- read.spss(elsawave8_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave8 %>%
#  count()
# n=8,445

# check for duplicates in ID number
#elsawave8 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave8 <- select(elsawave8,
                idauniq, w8dobyear = indobyr, w8sex = indsex, w8finstat = finstat, w8indoutcome = w8indout, w8samplingstatus = samptyp,
                w8stroke = hedimst, w8strokeage = heage, w8nstrokessincew7 = henmst,
                w8disputew7stroke = hedanst,
                halluc = hepsyha, schz = hepsysc, psychosis = hepsyps, w8depression = hepsyde, w8anxiety = hepsyan,               
                w8region = gor, w8ethnicgroup = fqethnmr,                
                w8smokeever = hesmk, w8smokenow = heska, w8confirmsw7smokingstatus = heskd, w8disputew7smoking = heske,
                w8vigorousphyact = heacta, w8alcohol = scako)

#Variables not included in this file: w8strokelast2yrm = HeageR,  w8strokelast2yry = HeAgeRY

#RECODING AND LABELLING

#White/non-white ethnicity
wave8$w8ethnicgroup <- factor(wave8$w8ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
# table(wave8$w8ethnicgroup)

#Region
wave8$w8region <- str_squish(wave8$w8region)
wave8$w8region [wave8$w8region == "E12000001"] <- "north east"
wave8$w8region [wave8$w8region == "E12000002"] <- "north west"
wave8$w8region [wave8$w8region == "E12000003"] <- "yorkshire and the humber"
wave8$w8region [wave8$w8region == "E12000004"] <- "east midlands"
wave8$w8region [wave8$w8region == "E12000005"] <- "west midlands"
wave8$w8region [wave8$w8region == "E12000006"] <- "east of england"
wave8$w8region [wave8$w8region == "E12000007"] <- "london"
wave8$w8region [wave8$w8region == "E12000008"] <- "south east"
wave8$w8region [wave8$w8region == "E12000009"] <- "south west"
wave8$w8region [wave8$w8region == "S99999999"] <- "scotland"
wave8$w8region [wave8$w8region == "W99999999"] <- "wales"
# table(wave8$w8region, useNA = "always")

#Create column called w8psychosisany which defines a case where 1 is recorded in w8 halluc, schz or psychosis variables
wave8 <- wave8 %>% 
  mutate(w8psychosisany = case_when(halluc == 1 ~ '1', 
                                    schz == 1 ~ '1',
                                    psychosis == 1 ~ '1',
                                    TRUE ~ '0'))
#table(wave8$w8psychosisany) #n=8

# Stroke (newly reported): recoding
wave8$w8stroke[wave8$w8stroke < 0] <- 0 # "refused/dont know/NA
#table(wave8$w8stroke) # n=151

# Disputed w7 stroke reasons
wave8$w8disputew7stroke[wave8$w8disputew7stroke < 0] <- NA # refused, don't know, not applicable
wave8$w8disputew7stroke[wave8$w8disputew7stroke == 1] <- "never had"
wave8$w8disputew7stroke[wave8$w8disputew7stroke == 2] <- "no longer has"
wave8$w8disputew7stroke[wave8$w8disputew7stroke == 3] <- "did not have previously but has now"
wave8$w8disputew7stroke[wave8$w8disputew7stroke == 4] <- "misdiagnosed"
#table(wave8$w8disputew7stroke)

#w8 stroke age
wave8$w8strokeage[wave8$w8strokeage < 0] <- NA # not applicable
#table(wave8$w8strokeage)

#w8 stroke age year - not needed as this variable was not found in this file
#wave8$w8strokelast2yry[wave8$w8strokelast2yry < 0] <- NA # not applicable
#table(wave8$w8strokelast2yry) 

#w8 anxiety
wave8$w8anxiety[wave8$w8anxiety < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave8$w8anxiety) # n=64

#w8 depression
wave8$w8depression[wave8$w8depression < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave8$w8depression) # n=69

#w8 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave8$w8vigorousphyact[wave8$w8vigorousphyact == 1] <- "more than once a week"
wave8$w8vigorousphyact[wave8$w8vigorousphyact == 2] <- "once a week"
wave8$w8vigorousphyact[wave8$w8vigorousphyact == 3] <- "one to three times a month"
wave8$w8vigorousphyact[wave8$w8vigorousphyact == 4] <- "hardly ever, or never"
wave8$w8vigorousphyact[wave8$w8vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave8$w8vigorousphyact)

#w8 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave8$w8alcohol[wave8$w8alcohol == 1] <- "Almost every day"
wave8$w8alcohol[wave8$w8alcohol == 2] <- "Five or six days a week"
wave8$w8alcohol[wave8$w8alcohol == 3] <- "Three or four days a week"
wave8$w8alcohol[wave8$w8alcohol == 4] <- "Once or twice a week"
wave8$w8alcohol[wave8$w8alcohol == 5] <- "Once or twice a month"
wave8$w8alcohol[wave8$w8alcohol == 6] <- "Once every couple of months"
wave8$w8alcohol[wave8$w8alcohol == 7] <- "Once or twice a year"
wave8$w8alcohol[wave8$w8alcohol == 8] <- "Not at all"
wave8$w8alcohol[wave8$w8alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave8$w8alcohol)

#w8 smoking ever status: have you ever smoke cigarettes?
wave8$w8smokeever[wave8$w8smokeever == 2] <- 0 # no
wave8$w8smokeever[wave8$w8smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave8$w8smokeever)

#w8 smoking status: do you smoke cigarettes at all nowadays?
wave8$w8smokenow[wave8$w8smokenow == 1] <- "Yes" # no
wave8$w8smokenow[wave8$w8smokenow == 2] <- "No" # no
wave8$w8smokenow[wave8$w8smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
wave8$w8smokenow[wave8$w8smokenow == -2] <- "No" # schedule not applicable, most likely due to reporting never smoking
wave8$w8smokenow[wave8$w8smokenow == -8] <- "No" # don't know
wave8$w8smokenow[wave8$w8smokenow == -9] <- "No" # refused, coded as no given no evidence of smoking now
#table(wave8$w8smokeever, wave8$w8smokenow)

#w8 disputing w7 smoking: numbers and reasons
wave8$w8disputew7smoking[wave8$w8disputew7smoking == 1] <- "no longer smoked by w7"
wave8$w8disputew7smoking[wave8$w8disputew7smoking == 2] <- "stopped smoking between w7 and w8"
wave8$w8disputew7smoking[wave8$w8disputew7smoking == 3] <- "stopped smoking between w7 and w8"
#table(wave8$w8confirmsw7smokingstatus, wave8$w8disputew7smoking)

#w8 number of strokes since w7
wave8$w8nstrokessincew7[wave8$w8nstrokessincew7 < 0] <- NA # not applicable
#table(wave8$w8nstrokessincew7)

# ELSA wave 9 data

# Import data
elsawave9 <- read.spss(elsawave9_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave9 %>%
#  count()
# n=8,736

# check for duplicates in ID number
#elsawave9 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave9 <- select(elsawave9,
                idauniq, w9dobyear = indobyr, w9sex = indsex, w9finstat = finstat, w9indoutcome = w9indout, w9samplingstatus = samptyp,
                w9stroke = hedimst, w9strokeage = heage, w9nstrokessincew8 = henmst,
                w9disputew8stroke = hedanst,
                halluc = hepsyha, schz = hepsysc, psychosis = hepsyps, w9depression = hepsyde, w9anxiety = hepsyan,               
                w9region = GOR, w9ethnicgroup = fqethnmr,                
                w9smokeever = hesmk, w9smokenow = heska, w9confirmsw8smokingstatus = heskd, w9disputew8smoking = heske,
                w9vigorousphyact = heacta, w9alcohol = scalcm)

#Variables not included: w9strokelast2yrm = HeageR,  w9strokelast2yry = HeAgeRY

#RECODING AND LABELLING

#White/non-white ethnicity
wave9$w9ethnicgroup <- factor(wave9$w9ethnicgroup, levels = c(1, 2), labels = c("white", "non-white"))
table(wave9$w9ethnicgroup)

#Region
wave9$w9region <- str_squish(wave9$w9region)
wave9$w9region [wave9$w9region == "E12000001"] <- "north east"
wave9$w9region [wave9$w9region == "E12000002"] <- "north west"
wave9$w9region [wave9$w9region == "E12000003"] <- "yorkshire and the humber"
wave9$w9region [wave9$w9region == "E12000004"] <- "east midlands"
wave9$w9region [wave9$w9region == "E12000005"] <- "west midlands"
wave9$w9region [wave9$w9region == "E12000006"] <- "east of england"
wave9$w9region [wave9$w9region == "E12000007"] <- "london"
wave9$w9region [wave9$w9region == "E12000008"] <- "south east"
wave9$w9region [wave9$w9region == "E12000009"] <- "south west"
wave9$w9region [wave9$w9region == "S99999999"] <- "scotland"
wave9$w9region [wave9$w9region == "W99999999"] <- "wales"
wave9[wave9$idauniq==105348, "w9region"] <- NA # these were changed to NA as contained a blank space only
wave9[wave9$idauniq==116848, "w9region"] <- NA
wave9[wave9$idauniq==909548, "w9region"] <- NA
table(wave9$w9region, useNA = "always")


#Create column called w9psychosisany which defines a case where 1 is recorded in w9 halluc, schz or psychosis variables
wave9 <- wave9 %>% 
  mutate(w9psychosisany = case_when(halluc == 1 ~ '1', 
                                    schz == 1 ~ '1',
                                    psychosis == 1 ~ '1',
                                    TRUE ~ '0'))
#table(wave9$w9psychosisany) #n=12

# Stroke (newly reported): recoding
wave9$w9stroke[wave9$w9stroke < 0] <- 0 # "refused/dont know/NA
#table(wave9$w9stroke) # n=127

# Disputed w8 stroke reasons
wave9$w9disputew8stroke[wave9$w9disputew8stroke < 0] <- NA # refused, don't know, not applicable
wave9$w9disputew8stroke[wave9$w9disputew8stroke == 1] <- "never had"
wave9$w9disputew8stroke[wave9$w9disputew8stroke == 2] <- "no longer has"
wave9$w9disputew8stroke[wave9$w9disputew8stroke == 3] <- "did not have previously but has now"
wave9$w9disputew8stroke[wave9$w9disputew8stroke == 4] <- "misdiagnosed"
#table(wave9$w9disputew8stroke)

#w9 stroke age
wave9$w9strokeage[wave9$w9strokeage < 0] <- NA # not applicable
#table(wave9$w9strokeage)

#w9 anxiety
wave9$w9anxiety[wave9$w9anxiety < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave9$w9anxiety) # n=172

#w9 depression
wave9$w9depression[wave9$w9depression < 0] <- 0 #not applicable/don't know/questionnaire error/refused changed to no
#table(wave9$w9depression) # n=189

#w9 vigorous physical activity: Do you take part in sports or activities that are vigorous ….? frequency in daily life
wave9$w9vigorousphyact[wave9$w9vigorousphyact == 1] <- "more than once a week"
wave9$w9vigorousphyact[wave9$w9vigorousphyact == 2] <- "once a week"
wave9$w9vigorousphyact[wave9$w9vigorousphyact == 3] <- "one to three times a month"
wave9$w9vigorousphyact[wave9$w9vigorousphyact == 4] <- "hardly ever, or never"
wave9$w9vigorousphyact[wave9$w9vigorousphyact < 0] <- NA # refused/dont know/not applicable
#table(wave9$w9vigorousphyact)

#w9 alcohol: In the past 12 months have you taken an alcoholic drink …? frequency
wave9$w9alcohol[wave9$w9alcohol == 1] <- "Almost every day"
wave9$w9alcohol[wave9$w9alcohol == 2] <- "Five or six days a week"
wave9$w9alcohol[wave9$w9alcohol == 3] <- "Three or four days a week"
wave9$w9alcohol[wave9$w9alcohol == 4] <- "Once or twice a week"
wave9$w9alcohol[wave9$w9alcohol == 5] <- "Once or twice a month"
wave9$w9alcohol[wave9$w9alcohol == 6] <- "Once every couple of months"
wave9$w9alcohol[wave9$w9alcohol == 7] <- "Once or twice a year"
wave9$w9alcohol[wave9$w9alcohol == 8] <- "Not at all"
wave9$w9alcohol[wave9$w9alcohol < 0] <- NA # refused/dont know/not applicable
#table(wave9$w9alcohol)

#w9 smoking ever status: have you ever smoke cigarettes?
wave9$w9smokeever[wave9$w9smokeever == 2] <- 0 # no
wave9$w9smokeever[wave9$w9smokeever < 0] <- NA # refused/dont know/not applicable
#table(wave9$w9smokeever)

#w9 smoking status: do you smoke cigarettes at all nowadays?
wave9$w9smokenow[wave9$w9smokenow == 1] <- "Yes" # no
wave9$w9smokenow[wave9$w9smokenow == 2] <- "No" # no
wave9$w9smokenow[wave9$w9smokenow == -1] <- "No" # question not applicable but can be coded as 'no' based on responses to smokeever
wave9$w9smokenow[wave9$w9smokenow == -2] <- "No" # schedule not applicable, most likely due to reporting never smoking
wave9$w9smokenow[wave9$w9smokenow == -8] <- "No" # don't know
wave9$w9smokenow[wave9$w9smokenow == -9] <- "No" # refused, coded as no given no evidence of smoking now
#table(wave9$w9smokeever, wave9$w9smokenow)

#w9 disputing w8 smoking: numbers and reasons
wave9$w9disputew8smoking[wave9$w9disputew8smoking == 1] <- "no longer smoked by w8"
wave9$w9disputew8smoking[wave9$w9disputew8smoking == 2] <- "stopped smoking before w8"
wave9$w9disputew8smoking[wave9$w9disputew8smoking == 3] <- "stopped smoking between w8 and w9"
#table(wave9$w9confirmsw8smokingstatus, wave9$w9disputew8smoking)

#w9 number of strokes since w8
wave9$w9nstrokessincew8[wave9$w9nstrokessincew8 < 0] <- NA # not applicable
#table(wave9$w9nstrokessincew8)

# Quintile of net financial wealth was located in separate wave-specific files ("financial derived variables"); The next area of code pulls out these variables from each wave:
# netfw_bu_s = Net financial wealth (benefit unit level). Gross financial wealth with financial debt subtracted.
# nfwq5_bu_s = Quintiles of net financial wealth (netfw_bu).

#Wave 1
# IMPORT DATA
elsawave1_financial <- read.spss(elsawave1_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave1_financial <- select(elsawave1_financial,
                              idauniq, w1netfw_sum = netfw_bu_s, w1netfw_quintile = nfwq5_bu_s)

#Wave 2
# IMPORT DATA
elsawave2_financial <- read.spss(elsawave2_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave2_financial <- select(elsawave2_financial,
                              idauniq, w2netfw_sum = netfw_bu_s, w2netfw_quintile = nfwq5_bu_s)

#WWave 3
# IMPORT DATA
elsawave3_financial <- read.spss(elsawave3_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave3_financial <- select(elsawave3_financial,
                              idauniq, w3netfw_sum = netfw_bu_s, w3netfw_quintile = nfwq5_bu_s)

#Wave 4
# IMPORT DATA
elsawave4_financial <- read.spss(elsawave4_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave4_financial <- select(elsawave4_financial,
                              idauniq, w4netfw_sum = netfw_bu_s, w4netfw_quintile = nfwq5_bu_s)
#Wave 5
# IMPORT DATA
elsawave5_financial <- read.spss(elsawave5_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave5_financial <- select(elsawave5_financial,
                              idauniq, w5netfw_sum = netfw_bu_s, w5netfw_quintile = nfwq5_bu_s)

#Wave 6
# IMPORT DATA
elsawave6_financial <- read.spss(elsawave6_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave6_financial <- select(elsawave6_financial,
                              idauniq, w6netfw_sum = netfw_bu_s, w6netfw_quintile = nfwq5_bu_s)

#Wave 7
# IMPORT DATA
elsawave7_financial <- read.spss(elsawave7_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave7_financial <- select(elsawave7_financial,
                              idauniq, w7netfw_sum = netfw_bu_s, w7netfw_quintile = nfwq5_bu_s)

#Wave 8
# IMPORT DATA
elsawave8_financial <- read.spss(elsawave8_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave8_financial <- select(elsawave8_financial,
                              idauniq, w8netfw_sum = netfw_bu_s, w8netfw_quintile = nfwq5_bu_s)

#Wave 9
# IMPORT DATA
elsawave9_financial <- read.spss(elsawave9_financial_file, to.data.frame = TRUE, use.value.labels = FALSE)

# SELECT SUBSET OF VARIABLES
elsawave9_financial <- select(elsawave9_financial,
                              idauniq, w9netfw_sum = netfw_bu_s, w9netfw_quintile = nfwq5_bu_s)

# merge financial datasets into one data frame
combineddata_financial <- merge(elsawave1_financial, elsawave2_financial, by="idauniq", all = TRUE)
combineddata_financial <- merge(combineddata_financial, elsawave3_financial, by="idauniq", all = TRUE)
combineddata_financial <- merge(combineddata_financial, elsawave4_financial, by="idauniq", all = TRUE)
combineddata_financial <- merge(combineddata_financial, elsawave5_financial, by="idauniq", all = TRUE)
combineddata_financial <- merge(combineddata_financial, elsawave6_financial, by="idauniq", all = TRUE)
combineddata_financial <- merge(combineddata_financial, elsawave7_financial, by="idauniq", all = TRUE)
combineddata_financial <- merge(combineddata_financial, elsawave8_financial, by="idauniq", all = TRUE)
combineddata_financial <- merge(combineddata_financial, elsawave9_financial, by="idauniq", all = TRUE)

# Convert financial sums to numeric values
combineddata_financial$w1netfw_sum <- as.numeric(combineddata_financial$w1netfw_sum)
combineddata_financial$w2netfw_sum <- as.numeric(combineddata_financial$w2netfw_sum)
combineddata_financial$w3netfw_sum <- as.numeric(combineddata_financial$w3netfw_sum)
combineddata_financial$w4netfw_sum <- as.numeric(combineddata_financial$w4netfw_sum)
combineddata_financial$w5netfw_sum <- as.numeric(combineddata_financial$w5netfw_sum)
combineddata_financial$w6netfw_sum <- as.numeric(combineddata_financial$w6netfw_sum)
combineddata_financial$w7netfw_sum <- as.numeric(combineddata_financial$w7netfw_sum)
combineddata_financial$w8netfw_sum <- as.numeric(combineddata_financial$w8netfw_sum)
combineddata_financial$w9netfw_sum <- as.numeric(combineddata_financial$w9netfw_sum)


# ELSA wave 0 data
# Some fields are taken from the ELSA wave 0 files (the Health Survey for England year that participants were sourced from)
#The Common Variables dataset contains all HSE respondents for ELSA (N = 26,787).
#Wave 0 Common Variables dataset contains variables present at all survey years covering Cohorts 1, 3 and 4. 
#Due to differences between HSE/ELSA datasets, the Common Variables file is the closest that exists to a single Wave 0 dataset.
#For completeness, stroke data was checked in all wave 0 files (mental health was not collected)

# Import data
elsawave0 <- read.spss(elsawave0_file, to.data.frame = TRUE, use.value.labels = FALSE)

# HIGH LEVEL VALIDATION CHECKS
# count the number of observations in dataset
#elsawave0 %>%
#  count()
# n=26,787 (matches user guide)

# check for duplicates in ID number
#elsawave0 %>%
#  count(idauniq) %>%
#  filter(n>1)
# n=0

# SELECT SUBSET OF VARIABLES
wave0 <- select(elsawave0,
                idauniq, illsm1, illsm2, illsm3, illsm4, illsm5, illsm6)

# Stroke
#Create column called w0stroke which defines a case where 8 is recorded in any of the illsm1 variables (15=Stroke/cerebral haemorrhage/cerebral thrombosis)
wave0 <- wave0 %>% 
  mutate(w0strokeany = case_when(illsm1 == 15 ~ '1', 
                                 illsm2 == 15 ~ '1',
                                 illsm3 == 15 ~ '1',
                                 illsm4 == 15 ~ '1',
                                 illsm5 == 15 ~ '1',
                                 illsm6 == 15 ~ '1',
                                 TRUE ~ '0'))
#table(wave0$w0strokeany) n=377

# Import data
wave01998 <- read.spss(wave01998_file, to.data.frame = TRUE, use.value.labels = FALSE)

wave01998 <- select(wave01998,
                    idauniq, docstro, agestro, recstro)

#rename/labelling
#Doctor diagnosed stroke
wave01998$docstro[wave01998$docstro < 0] <- 0
wave01998$docstro[wave01998$docstro == 2] <- 0
#table(wave01998$docstro) # n=321

wave01998$w098stroke <- wave01998$docstro
wave01998$w098strokeage <- wave01998$agestro
wave01998$w098strokeage[wave01998$w098strokeage < 0] <- NA
#table(wave01998$w098strokeage)

# Import data
wave01999 <- read.spss(wave01999_file, to.data.frame = TRUE, use.value.labels = FALSE)

wave01999 <- select(wave01999,
                    idauniq, docstro, agestro, recstro)
#rename/labelling
#Doctor diagnosed stroke
wave01999$docstro[wave01999$docstro < 0] <- 0
wave01999$docstro[wave01999$docstro == 2] <- 0
#table(wave01999$docstro) # n=8

wave01999$w099stroke <- wave01999$docstro
wave01999$w099strokeage <- wave01999$agestro
wave01999$w099strokeage[wave01999$w099strokeage < 0] <- NA
#table(wave01999$w099strokeage)

#Create merged dataset containing identifiers and variables of interest across waves 0-9.

#Create merged dataset of index and wave1 (index used as the master)
combineddata <- merge(index, wave1, by="idauniq", all.x = TRUE) # all.y keeps unmatched records in the second file, all.x would keep all unmatched records from first file.

#add wave 2
combineddata <- merge(combineddata, wave2, by="idauniq", all.x = TRUE) # all.y keeps unmatched records in the second file, all.x would keep all unmatched records from first file.

#add wave 3
combineddata <- merge(combineddata, wave3, by="idauniq", all.x = TRUE) 

#add wave 4
combineddata <- merge(combineddata, wave4, by="idauniq", all.x = TRUE)

#add wave 5
combineddata <- merge(combineddata, wave5, by="idauniq", all.x = TRUE) 

# add wave 6
combineddata <- merge(combineddata, wave6, by="idauniq", all = TRUE) # all keeps all unique id numbers, regardless of whether they are in the first file

# add wave 7
combineddata <- merge(combineddata, wave7, by="idauniq", all = TRUE)

# add wave 8
combineddata <- merge(combineddata, wave8, by="idauniq", all = TRUE)

# add wave 9
combineddata <- merge(combineddata, wave9, by="idauniq", all = TRUE)

#add wave 0 - 1998 (does not cover all ELSA participants - only used for stroke occurrence).
combineddata <- merge(combineddata, wave0, by="idauniq", all.x = TRUE) # all.y keeps unmatched records in the second file, all.x would keep all unmatched records from first file. all.x used as participants are only needed from wave 0 if they took part in one of the other ELSA waves.

#add wave 0 - 1998 (does not cover all ELSA participants - only used for stroke occurrence and stroke age)
combineddata <- merge(combineddata, wave01998, by="idauniq", all.x = TRUE)

#add wave 0 - 1999 (does not cover all ELSA participants - only used for stroke occurrence and stroke age). Wave 0 - 2001 does not contain stroke data.
combineddata <- merge(combineddata, wave01999, by="idauniq", all.x = TRUE)

#add financial combined dataset
combineddata <- merge(combineddata, combineddata_financial, by="idauniq", all.x = TRUE) # all.y keeps unmatched records in the second file, all.x would keep all unmatched records from first file.

# Select all variables of interest for the main dataset, which is called 'waves12345' (named before the later waves were added!)
waves12345 <- select(combineddata,
                     idauniq, sex, w6sex, w7sex, w8sex, w9sex, dobyear, w6dobyear, w7dobyear, w8dobyear, w9dobyear,
                     indexfinstatw1, wave1indoutcome, 
                     indexfinstatw2, wave2indoutcome, 
                     indexfinstatw3, wave3indoutcome, 
                     indexfinstatw4, wave4indoutcome, 
                     indexfinstatw5, wave5indoutcome, 
                     w6indoutcome, w7indoutcome, w8indoutcome, w9indoutcome,
                     w0strokeany, w098stroke, w099stroke, w1strokeany, w2strokeany, w3stroke, w4stroke, w5stroke, w6stroke, w7stroke, w8stroke, w9stroke,
                     w1psychosisany, w2psychosisany, w3psychosisany, w4psychosisany, w5psychosisany, w6psychosisany, w7psychosisany, w8psychosisany, w9psychosisany,
                     w1depression, w2depression, w3depression, w4depression, w5depression, w6depression, w7depression, w8depression, w9depression,
                     w1anxiety, w2anxiety, w3anxiety, w4anxiety, w5anxiety, w6anxiety, w7anxiety, w8anxiety, w9anxiety,
                     w098strokeage, w099strokeage, w1strokeage, w2strokeage, w2strokelast2yry, w3strokeage, w3strokelast2yry, w4strokeage, w4strokelast2yry, 
                     w5strokeage, w5strokelast2yry, w6strokeage, w6strokelast2yry, w7strokeage, w7strokelast2yry, w8strokeage, w9strokeage,
                     w2nstrokessincew1, w3nstrokessincew2, w4nstrokessincew3, w5nstrokessincew4, w6nstrokessincew5, w7nstrokessincew6, w8nstrokessincew7, w9nstrokessincew8,
                     w1strokeremainproblems, w2strokeremainproblems, w3strokeremainproblems, w4strokeremainproblems, w5strokeremainproblems,
                     w2disputew1stroke, w3disputew2stroke, w4disputew3stroke, w5disputew4stroke, w6disputew5stroke, w7disputew6stroke, w8disputew7stroke, w9disputew8stroke,
                     w1ethnicgroup, w2ethnicgroup, w3ethnicgroup, w4ethnicgroup, w5ethnicgroup, w6ethnicgroup, w7ethnicgroup, w8ethnicgroup, w9ethnicgroup,
                     w1region, w2region, w3region, w4region, w5region, w6region, w7region, w8region, w9region,
                     w1alcohol, w2alcohol, w3alcohol, w4alcohol, w5alcohol, w6alcohol, w7alcohol, w8alcohol, w9alcohol,
                     w1smokenow, w2smokenow, w3smokenow, w4smokenow, w5smokenow, w6smokenow, w7smokenow, w8smokenow, w9smokenow,
                     w2disputew1smoking, w3disputew2smoking, w4disputew3smoking, w5disputew4smoking, w6disputew5smoking, w7disputew6smoking, w8disputew7smoking, w9disputew8smoking,
                     w1smokeever, w2smokeever, w3smokeever, w4smokeever, w5smokeever, w6smokeever, w7smokeever, w8smokeever, w9smokeever,
                     w1vigorousphyact, w2vigorousphyact, w3vigorousphyact, w4vigorousphyact, w5vigorousphyact, w6vigorousphyact, w7vigorousphyact, w8vigorousphyact, w9vigorousphyact,
                     w1netfw_sum, w2netfw_sum, w3netfw_sum, w4netfw_sum, w5netfw_sum, w6netfw_sum, w7netfw_sum, w8netfw_sum, w9netfw_sum,
                     w1netfw_quintile, w2netfw_quintile, w3netfw_quintile, w4netfw_quintile, w5netfw_quintile, w6netfw_quintile, w7netfw_quintile, w8netfw_quintile, w9netfw_quintile,
                     mortalitywave)

# Manual data changes to account for disputed strokes at later waves. If stroke was disputed, then stroke, stroke age and number of strokes since last wave are recoded to no/NA.
waves12345$w1strokeany <- as.numeric(waves12345$w1strokeany)
waves12345$w2strokeany <- as.numeric(waves12345$w2strokeany)
waves12345[waves12345$idauniq==119177, "w1strokeany"] <- 0 # disputed w1 stroke diagnosis at w2
waves12345[waves12345$idauniq==119177, "w1strokeage"] <- NA # 
waves12345[waves12345$idauniq==119177, "w3nstrokessincew2"] <- NA # did not report a stroke at any of these subsequent waves
waves12345[waves12345$idauniq==119177, "w4nstrokessincew3"] <- NA 
waves12345[waves12345$idauniq==119177, "w5nstrokessincew4"] <- NA 
waves12345[waves12345$idauniq==119177, "w6nstrokessincew5"] <- NA 
waves12345[waves12345$idauniq==119177, "w7nstrokessincew6"] <- NA 

waves12345[waves12345$idauniq==105909, "w2strokeany"] <- 0 # disputed w2 stroke diagnosis at w3 (later had a stroke at w4)
waves12345[waves12345$idauniq==105909, "w2nstrokessincew1"] <- NA
waves12345[waves12345$idauniq==105909, "w2strokeage"] <- NA
waves12345[waves12345$idauniq==105909, "w2strokelast2yry"] <- NA

waves12345[waves12345$idauniq==107705, "w2strokeany"] <- 0 # disputed w2 stroke diagnosis at w3
waves12345[waves12345$idauniq==107705, "w2nstrokessincew1"] <- NA
waves12345[waves12345$idauniq==107705, "w2strokeage"] <- NA
waves12345[waves12345$idauniq==107705, "w2strokelast2yry"] <- NA

waves12345[waves12345$idauniq==105639, "w2strokeany"] <- 0 # disputed w2 stroke diagnosis at w3
waves12345[waves12345$idauniq==105639, "w2nstrokessincew1"] <- NA
waves12345[waves12345$idauniq==105639, "w2strokeage"] <- NA
waves12345[waves12345$idauniq==105639, "w2strokelast2yry"] <- NA

waves12345[waves12345$idauniq==110914, "w2strokeany"] <- 0 # disputed w2 stroke diagnosis at w3
waves12345[waves12345$idauniq==110914, "w2nstrokessincew1"] <- NA
waves12345[waves12345$idauniq==110914, "w2strokeage"] <- NA
waves12345[waves12345$idauniq==110914, "w2strokelast2yry"] <- NA

waves12345[waves12345$idauniq==120566, "w2strokeany"] <- 0 # disputed w2 stroke diagnosis at w3
waves12345[waves12345$idauniq==120566, "w2nstrokessincew1"] <- NA
waves12345[waves12345$idauniq==120566, "w2strokeage"] <- NA
waves12345[waves12345$idauniq==120566, "w2strokelast2yry"] <- NA

waves12345[waves12345$idauniq==108439, "w3stroke"] <- 0 # disputed w3 stroke diagnosis at w4
waves12345[waves12345$idauniq==108439, "w3nstrokessincew2"] <- NA
waves12345[waves12345$idauniq==108439, "w3strokeage"] <- NA
waves12345[waves12345$idauniq==108439, "w3strokelast2yry"] <- NA

waves12345[waves12345$idauniq==116819, "w3stroke"] <- 0 # disputed w3 stroke diagnosis at w4
waves12345[waves12345$idauniq==116819, "w3nstrokessincew2"] <- NA
waves12345[waves12345$idauniq==116819, "w3strokeage"] <- NA
waves12345[waves12345$idauniq==116819, "w3strokelast2yry"] <- NA

waves12345[waves12345$idauniq==104223, "w4stroke"] <- 0 # disputed w4 stroke diagnosis at w5
waves12345[waves12345$idauniq==104223, "w4nstrokessincew3"] <- NA
waves12345[waves12345$idauniq==104223, "w4strokeage"] <- NA
waves12345[waves12345$idauniq==104223, "w4strokelast2yry"] <- NA

waves12345[waves12345$idauniq==117119, "w4stroke"] <- 0 # disputed w4 stroke diagnosis at w5
waves12345[waves12345$idauniq==117119, "w4nstrokessincew3"] <- NA
waves12345[waves12345$idauniq==117119, "w4strokeage"] <- NA
waves12345[waves12345$idauniq==117119, "w4strokelast2yry"] <- NA

waves12345[waves12345$idauniq==119706, "w4stroke"] <- 0 # disputed w4 stroke diagnosis at w5
waves12345[waves12345$idauniq==119706, "w4nstrokessincew3"] <- NA
waves12345[waves12345$idauniq==119706, "w4strokeage"] <- NA
waves12345[waves12345$idauniq==119706, "w4strokelast2yry"] <- NA

waves12345[waves12345$idauniq==105426, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==105426, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==105426, "w5strokeage"] <- NA
waves12345[waves12345$idauniq==105426, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==108689, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==108689, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==108689, "w5strokeage"] <- NA
waves12345[waves12345$idauniq==108689, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==111160, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==111160, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==111160, "w5strokeage"] <- NA
waves12345[waves12345$idauniq==111160, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==120605, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==120605, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==120605, "w5strokeage"] <- NA
waves12345[waves12345$idauniq==120605, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==111364, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==111364, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==111364, "w5strokeage"] <- NA
waves12345[waves12345$idauniq==111364, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==116712, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==116712, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==116712, "w5strokeage"] <- NA
waves12345[waves12345$idauniq==116712, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==119241, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==119241, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==119241, "w5strokeage"] <- NA
waves12345[waves12345$idauniq==119241, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==161053, "w5stroke"] <- 0 # disputed w5 stroke diagnosis at w6
waves12345[waves12345$idauniq==161053, "w5strokeage"] <- NA 
waves12345[waves12345$idauniq==161053, "w5nstrokessincew4"] <- NA
waves12345[waves12345$idauniq==161053, "w5strokelast2yry"] <- NA

waves12345[waves12345$idauniq==107888, "w6stroke"] <- 0 # disputed w6 stroke diagnosis at w7 ("misdiagnosed")
waves12345[waves12345$idauniq==107888, "w6strokeage"] <- NA 
waves12345[waves12345$idauniq==107888, "w6nstrokessincew5"] <- NA
waves12345[waves12345$idauniq==107888, "w6strokelast2yry"] <- NA

waves12345[waves12345$idauniq==116955, "w6stroke"] <- 0 # disputed w6 stroke diagnosis at w7 ("misdiagnosed")
waves12345[waves12345$idauniq==116955, "w6strokeage"] <- NA 
waves12345[waves12345$idauniq==116955, "w6nstrokessincew5"] <- NA
waves12345[waves12345$idauniq==116955, "w6strokelast2yry"] <- NA

waves12345[waves12345$idauniq==120478, "w6stroke"] <- 0 # disputed w6 stroke diagnosis at w7 ("misdiagnosed")
waves12345[waves12345$idauniq==120478, "w6strokeage"] <- NA 
waves12345[waves12345$idauniq==120478, "w6nstrokessincew5"] <- NA
waves12345[waves12345$idauniq==120478, "w6strokelast2yry"] <- NA

waves12345[waves12345$idauniq==111663, "w6stroke"] <- 0 # disputed w6 stroke diagnosis at w7 ("never had")
waves12345[waves12345$idauniq==111663, "w6strokeage"] <- NA 
waves12345[waves12345$idauniq==111663, "w6nstrokessincew5"] <- NA
waves12345[waves12345$idauniq==111663, "w6strokelast2yry"] <- NA

waves12345[waves12345$idauniq==105528, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==105528, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==105528, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==105528, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==118074, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==118074, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==118074, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==118074, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==111905, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==111905, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==111905, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==111905, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==119110, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==119110, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==119110, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==119110, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==161536, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==161536, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==161536, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==161536, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==120797, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==120797, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==120797, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==120797, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==112824, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==112824, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==112824, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==112824, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==110967, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==110967, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==110967, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==110967, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==160559, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==160559, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==160559, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==160559, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==120595, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("never had")
waves12345[waves12345$idauniq==120595, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==120595, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==120595, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==112277, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("misdiagnosed")
waves12345[waves12345$idauniq==112277, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==112277, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==112277, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==165590, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("misdiagnosed")
waves12345[waves12345$idauniq==165590, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==165590, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==165590, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==119868, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("misdiagnosed")
waves12345[waves12345$idauniq==119868, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==119868, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==119868, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==160191, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("misdiagnosed")
waves12345[waves12345$idauniq==160191, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==160191, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==160191, "w7strokelast2yry"] <- NA

waves12345[waves12345$idauniq==111913, "w7stroke"] <- 0 # disputed w7 stroke diagnosis at w8 ("misdiagnosed")
waves12345[waves12345$idauniq==111913, "w7strokeage"] <- NA 
waves12345[waves12345$idauniq==111913, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==111913, "w7strokelast2yry"] <- NA

# 107429 disputed w7 stroke at w8 with the reason 'no longer has' - not recoded as it is not clear what this means

waves12345[waves12345$idauniq==117639, "w8stroke"] <- 0 # disputed w8 stroke diagnosis at w9 ("misdiagnosed")
waves12345[waves12345$idauniq==117639, "w8strokeage"] <- NA 
waves12345[waves12345$idauniq==117639, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==117639, "w8strokelast2yry"] <- NA

waves12345[waves12345$idauniq==120725, "w8stroke"] <- 0 # disputed w8 stroke diagnosis at w9 ("never had")
waves12345[waves12345$idauniq==120725, "w8strokeage"] <- NA 
waves12345[waves12345$idauniq==120725, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==120725, "w8strokelast2yry"] <- NA

waves12345[waves12345$idauniq==117463, "w8stroke"] <- 0 # disputed w8 stroke diagnosis at w9 ("never had")
waves12345[waves12345$idauniq==117463, "w8strokeage"] <- NA 
waves12345[waves12345$idauniq==117463, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==117463, "w8strokelast2yry"] <- NA

waves12345[waves12345$idauniq==118977, "w8stroke"] <- 0 # disputed w8 stroke diagnosis at w9 ("never had")
waves12345[waves12345$idauniq==118977, "w8strokeage"] <- NA 
waves12345[waves12345$idauniq==118977, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==118977, "w8strokelast2yry"] <- NA

waves12345[waves12345$idauniq==150335, "w8stroke"] <- 0 # disputed w8 stroke diagnosis at w9 ("never had")
waves12345[waves12345$idauniq==150335, "w8strokeage"] <- NA 
waves12345[waves12345$idauniq==150335, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==150335, "w8strokelast2yry"] <- NA

waves12345[waves12345$idauniq==160620, "w8stroke"] <- 0 # disputed w8 stroke diagnosis at w9 ("never had")
waves12345[waves12345$idauniq==160620, "w8strokeage"] <- NA 
waves12345[waves12345$idauniq==160620, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==160620, "w8strokelast2yry"] <- NA

waves12345[waves12345$idauniq==163281, "w8stroke"] <- 0 # disputed w8 stroke diagnosis at w9 ("never had")
waves12345[waves12345$idauniq==163281, "w8strokeage"] <- NA 
waves12345[waves12345$idauniq==163281, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==163281, "w8strokelast2yry"] <- NA

# 107289 disputed w8 stroke at w9 with the reason 'no longer has' - not recoded as it is not clear what this means

#111214 has no record of any strokes, but has 'number of strokes since last wave' recorded at a few waves. Recoded to NA given they did not report any strokes.
waves12345[waves12345$idauniq==111214, "w8nstrokessincew7"] <- NA
waves12345[waves12345$idauniq==111214, "w7nstrokessincew6"] <- NA
waves12345[waves12345$idauniq==111214, "w6nstrokessincew5"] <- NA

# Manual data changes to account for disputed smoking status at later waves

#w2 disputes
waves12345[waves12345$idauniq==108410, "w1smokenow"] <- "No" # never smoked
waves12345[waves12345$idauniq==108553, "w1smokenow"] <- "No" # never smoked
waves12345[waves12345$idauniq==111854, "w1smokenow"] <- "No" # never smoked
waves12345[waves12345$idauniq==118935, "w1smokenow"] <- "No" # never smoked
waves12345[waves12345$idauniq==105587, "w1smokenow"] <- "No" # no longer smoked by w1
waves12345[waves12345$idauniq==106835, "w1smokenow"] <- "No" # no longer smoked by w1
waves12345[waves12345$idauniq==107300, "w1smokenow"] <- "No" # no longer smoked by w1
waves12345[waves12345$idauniq==118510, "w1smokenow"] <- "No" # no longer smoked by w1

#w3 disputes
waves12345[waves12345$idauniq==104837, "w2smokenow"] <- "No" # no longer smoked by w2
waves12345[waves12345$idauniq==108623, "w2smokenow"] <- "No" # no longer smoked by w2
waves12345[waves12345$idauniq==111298, "w2smokenow"] <- "No" # no longer smoked by w2
waves12345[waves12345$idauniq==118830, "w2smokenow"] <- "No" # no longer smoked by w2

#w4 disputes
waves12345[waves12345$idauniq==104413, "w3smokenow"] <- "No" # no longer smoked by w3
waves12345[waves12345$idauniq==108192, "w3smokenow"] <- "No" # no longer smoked by w3
waves12345[waves12345$idauniq==111138, "w3smokenow"] <- "No" # no longer smoked by w3
waves12345[waves12345$idauniq==111672, "w3smokenow"] <- "No" # no longer smoked by w3
waves12345[waves12345$idauniq==112606, "w3smokenow"] <- "No" # no longer smoked by w3
waves12345[waves12345$idauniq==119514, "w3smokenow"] <- "No" # no longer smoked by w3
waves12345[waves12345$idauniq==151038, "w3smokenow"] <- "No" # never smoked

#w5 disputes
waves12345[waves12345$idauniq==104243, "w4smokenow"] <- "No" # no longer smoked by w4
waves12345[waves12345$idauniq==108120, "w4smokenow"] <- "No" # no longer smoked by w4
waves12345[waves12345$idauniq==112604, "w4smokenow"] <- "No" # no longer smoked by w4
waves12345[waves12345$idauniq==118270, "w4smokenow"] <- "No" # never smoked

#w6 disputes - no relevant ones
#w7 disputes
waves12345[waves12345$idauniq==118619, "w6smokenow"] <- "No" # no longer smoked by w6
waves12345[waves12345$idauniq==161269, "w6smokenow"] <- "No" # no longer smoked by w6

#w8 disputes
waves12345[waves12345$idauniq==113048, "w7smokenow"] <- "No" # no longer smoked by w7

#w9 disputes
waves12345[waves12345$idauniq==167582, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==103908, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==117852, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==120739, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==161829, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==150154, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==160374, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==161100, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==160270, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==161021, "w8smokenow"] <- "No" # no longer smoked by w8
waves12345[waves12345$idauniq==166080, "w8smokenow"] <- "No" # no longer smoked by w8

#Create variable for reported stroke ever across waves 1-9
waves12345 <- waves12345 %>% 
  mutate(strokeever = case_when(w0strokeany == 1 ~ 1,
                                w098stroke == 1 ~ 1, 
                                w099stroke == 1 ~ 1,
                                w1strokeany == 1 ~ 1, 
                                w2strokeany == 1 ~ 1,
                                w3stroke == 1 ~ 1,
                                w4stroke == 1 ~ 1,
                                w5stroke == 1 ~ 1,
                                w6stroke == 1 ~ 1,
                                w7stroke == 1 ~ 1,
                                w8stroke == 1 ~ 1,
                                w9stroke == 1 ~ 1,
                                TRUE ~ 0))
# table(waves12345$strokeever) # n=1612

#Create variable for reported psychosis ever across waves 1-9
waves12345 <- waves12345 %>% 
  mutate(psychosisever = case_when(w1psychosisany == "1" ~ 1, 
                                   w2psychosisany == "1" ~ 1,
                                   w3psychosisany == "1" ~ 1,
                                   w4psychosisany == "1" ~ 1,
                                   w5psychosisany == "1" ~ 1,
                                   w6psychosisany == "1" ~ 1,
                                   w7psychosisany == "1" ~ 1,
                                   w8psychosisany == "1" ~ 1,
                                   w9psychosisany == "1" ~ 1,
                                   TRUE ~ 0))
# table(waves12345$psychosisever) # n=162

#Create variable for reported depression ever across waves 1-9
waves12345 <- waves12345 %>% 
  mutate(depressionever = case_when(w1depression == "1" ~ 1, 
                                    w2depression == "1" ~ 1,
                                    w3depression == "1" ~ 1,
                                    w4depression == "1" ~ 1,
                                    w5depression == "1" ~ 1,
                                    w6depression == "1" ~ 1,
                                    w7depression == "1" ~ 1,
                                    w8depression == "1" ~ 1,
                                    w9depression == "1" ~ 1,
                                    TRUE ~ 0))
#table(waves12345$depressionever) # n=2009

#Create variable for reported anxiety ever across waves 1-9
waves12345 <- waves12345 %>% 
  mutate(anxietyever = case_when(w1anxiety == "1" ~ 1, 
                                 w2anxiety == "1" ~ 1,
                                 w3anxiety == "1" ~ 1,
                                 w4anxiety == "1" ~ 1,
                                 w5anxiety == "1" ~ 1,
                                 w6anxiety == "1" ~ 1,
                                 w7anxiety == "1" ~ 1,
                                 w8anxiety == "1" ~ 1,
                                 w9anxiety == "1" ~ 1,
                                 TRUE ~ 0))
#table(waves12345$anxietyever) # n=1747

#Create columns indicating if participants took part productively in each wave
#Wave 1
waves12345 <- waves12345 %>%
  mutate(w1participated = case_when(wave1indoutcome == "Full interview in person" ~ 1,
                                    wave1indoutcome == "Full proxy interview" ~ 1,
                                    wave1indoutcome == "Partial interview in person" ~ 1,
                                    wave1indoutcome == "Partial proxy interview" ~ 1,
                                    TRUE ~ 0))

#Wave 2
waves12345 <- waves12345 %>%
  mutate(w2participated = case_when(wave2indoutcome == "Full interview in person" ~ 1,
                                    wave2indoutcome == "Full proxy interview" ~ 1,
                                    wave2indoutcome == "Partial interview in person" ~ 1,
                                    wave2indoutcome == "Partial interview by proxy" ~ 1,
                                    TRUE ~ 0))

#Wave 3
waves12345 <- waves12345 %>%
  mutate(w3participated = case_when(wave3indoutcome == "Full interview in person" ~ 1,
                                    wave3indoutcome == "Full interview by proxy" ~ 1,
                                    wave3indoutcome == "Partial interview in person" ~ 1,
                                    wave3indoutcome == "Institutional interview in person" ~ 1,
                                    wave3indoutcome == "Institutional interview by proxy" ~ 1,
                                    wave3indoutcome == "Partial proxy interview" ~ 1,
                                    TRUE ~ 0))

#Wave 4
waves12345 <- waves12345 %>%
  mutate(w4participated = case_when(wave4indoutcome == "Full interview in person" ~ 1,
                                    wave4indoutcome == "Full interview by proxy" ~ 1,
                                    wave4indoutcome == "Partial interview in person" ~ 1,
                                    wave4indoutcome == "Institutional interview in person" ~ 1,
                                    wave4indoutcome == "Institutional interview by proxy" ~ 1,
                                    wave4indoutcome == "Partial interview by proxy" ~ 1,
                                    TRUE ~ 0))

#Wave 5
waves12345 <- waves12345 %>%
  mutate(w5participated = case_when(wave5indoutcome == "Full interview in person" ~ 1,
                                    wave5indoutcome == "Full interview by proxy" ~ 1,
                                    wave5indoutcome == "Partial interview in person" ~ 1,
                                    wave5indoutcome == "Institutional interview in person" ~ 1,
                                    wave5indoutcome == "Institutional interview by proxy" ~ 1,
                                    wave5indoutcome == "Partial interview by proxy" ~ 1,
                                    TRUE ~ 0))

#Wave 6
waves12345 <- waves12345 %>%
  mutate(w6participated = case_when(w6indoutcome == 11 ~ 1, # Full interview in person
                                    w6indoutcome == 13 ~ 1, # Full interview by proxy
                                    w6indoutcome == 21 ~ 1, # Partial interview in person
                                    w6indoutcome == 23 ~ 1, # Partial interview by proxy
                                    w6indoutcome == 24 ~ 1, #institutional interview in person
                                    w6indoutcome == 25 ~ 1, #institutional interview by proxy
                                    TRUE ~ 0))

#Wave 7
waves12345 <- waves12345 %>%
  mutate(w7participated = case_when(w7indoutcome == 11 ~ 1, # Full interview in person
                                    w7indoutcome == 13 ~ 1, # Full interview by proxy
                                    w7indoutcome == 21 ~ 1, # Partial interview in person
                                    w7indoutcome == 23 ~ 1, # Partial interview by proxy
                                    w7indoutcome == 24 ~ 1, #institutional interview in person
                                    w7indoutcome == 25 ~ 1, #institutional interview by proxy
                                    TRUE ~ 0))

#Wave 8
waves12345 <- waves12345 %>%
  mutate(w8participated = case_when(w8indoutcome == 11 ~ 1, # Full interview in person
                                    w8indoutcome == 13 ~ 1, # Full interview by proxy
                                    w8indoutcome == 21 ~ 1, # Partial interview in person
                                    w8indoutcome == 23 ~ 1, # Partial interview by proxy
                                    w8indoutcome == 24 ~ 1, #institutional interview in person
                                    w8indoutcome == 25 ~ 1, #institutional interview by proxy
                                    TRUE ~ 0))

#Wave 9
waves12345 <- waves12345 %>%
  mutate(w9participated = case_when(w9indoutcome == 11 ~ 1, # Full interview in person
                                    w9indoutcome == 13 ~ 1, # Full interview by proxy
                                    w9indoutcome == 21 ~ 1, # Partial interview in person
                                    w9indoutcome == 23 ~ 1, # Partial interview by proxy
                                    w9indoutcome == 24 ~ 1, #institutional interview in person
                                    w9indoutcome == 25 ~ 1, #institutional interview by proxy
                                    TRUE ~ 0))

#Create a variable to indicate if participated productively in all waves 1-8 (binary)
waves12345$allwavesparticipated <- rowSums(waves12345[,c("w1participated", "w2participated", "w3participated", "w4participated", "w5participated", "w6participated", "w7participated", "w8participated", "w9participated")], na.rm=TRUE)
waves12345$allwavesparticipated[waves12345$allwavesparticipated < 9] <- 0 # not all waves
waves12345$allwavesparticipated[waves12345$allwavesparticipated == 9] <- 1 # all waves
#table(waves12345$allwavesparticipated) # n = 3225

#Create column calculating how many waves each participant took part in
waves12345$totalwaves <- rowSums(waves12345[,c("w1participated", "w2participated", "w3participated", "w4participated", "w5participated", "w6participated", "w7participated", "w8participated", "w9participated")], na.rm = TRUE)
#str(waves12345$totalwaves)

#Create column to calculate the wave that the participant first took part in (start of f-up)
waves12345 <- waves12345 %>% 
  mutate(wavefirstparticipate = case_when(w1participated == 1 ~ 1, 
                                          w2participated == 1 ~ 2,
                                          w3participated == 1 ~ 3,
                                          w4participated == 1 ~ 4,
                                          w5participated == 1 ~ 5,
                                          w6participated == 1 ~ 6,
                                          w7participated == 1 ~ 7,
                                          w8participated == 1 ~ 8,
                                          w9participated == 1 ~ 9,
                                          TRUE ~ 0))

#Create column to calculate the wave that the participant last took part in (end of f-up)
waves12345 <- waves12345 %>% 
  mutate(wavelastparticipate = case_when(w9participated == 1 ~ 9,
                                         w8participated == 1 ~ 8,
                                         w7participated == 1 ~ 7,
                                         w6participated == 1 ~ 6,
                                         w5participated == 1 ~ 5, 
                                         w4participated == 1 ~ 4,
                                         w3participated == 1 ~ 3,
                                         w2participated == 1 ~ 2,
                                         w1participated == 1 ~ 1,
                                         TRUE ~ 0))

#Remove participants that did not take part in any wave 1-9 (as they are not needed for this analyses)
waves12345 <- waves12345 %>%
  filter(wavefirstparticipate != 0)

#Sex - taken from index file for participants in waves 1-5 and then taken from each subsequent wave file for people that joined later
waves12345$w6sex <- factor(waves12345$w6sex, levels = c(1, 2), labels = c("Male", "Female"))
waves12345$w7sex <- factor(waves12345$w7sex, levels = c(1, 2), labels = c("Male", "Female"))
waves12345$w8sex <- factor(waves12345$w8sex, levels = c(1, 2), labels = c("Male", "Female"))
waves12345$w9sex <- factor(waves12345$w9sex, levels = c(1, 2), labels = c("Male", "Female"))

waves12345 <- waves12345 %>%
  mutate(sex_merge = coalesce(w6sex, sex)) # fills in the blanks with other variable

waves12345 <- waves12345 %>%
  mutate(sex_merge2 = coalesce(w7sex, sex_merge)) # fills in the blanks with other variable

waves12345 <- waves12345 %>%
  mutate(sex_merge3 = coalesce(w8sex, sex_merge2)) # fills in the blanks with other variable

waves12345 <- waves12345 %>%
  mutate(sex_merge4 = coalesce(w9sex, sex_merge3)) # fills in the blanks with other variable

#Condense into main variable
waves12345$sex <- waves12345$sex_merge4

#drop variables only used for merging
waves12345$sex_merge <- NULL
waves12345$sex_merge2 <- NULL
waves12345$sex_merge3 <- NULL
waves12345$sex_merge4 <- NULL

#DOB years (index file only contained DOB year for those who took part in waves 1-5, variable needed that contains DOB years for those that joined at later waves)
str(waves12345$w7dobyear)
waves12345 <- waves12345 %>% 
  mutate(dobyear_merge = case_when(dobyear >1 ~ dobyear,
                                   w6dobyear >1 ~ w6dobyear,
                                   w7dobyear >1 ~ w7dobyear, 
                                   w8dobyear >1 ~ w8dobyear,
                                   w9dobyear >1 ~ w9dobyear,
                                   TRUE ~ 0))
waves12345$dobyear_merge[waves12345$dobyear_merge == 0] <- NA
waves12345$dobyear <- waves12345$dobyear_merge
waves12345$dobyear_merge <- NULL

#Create column to calculate age at wave 1 (took place March 2002-March 2003. 2002 used as main year)
waves12345 <- waves12345 %>%
  mutate(w1age = 2002 - dobyear)

#Stroke age

# Generate W2 stroke age (generated from this data frame as this contains DOB year)
#Generate 'w2strokeage_calc' based on DOB year and W2 stroke reported year (which is the year of first stroke in last 2 years)
waves12345 <- waves12345 %>%
  mutate(w2strokeage_calc = w2strokelast2yry - dobyear)

waves12345$w2strokeage_calc[waves12345$w2strokeage_calc <= 0] <- NA
waves12345$w2strokeage[waves12345$w2strokeage <= 0] <- NA
waves12345 <- waves12345 %>%
  mutate(w2strokeage = coalesce(w2strokeage, w2strokeage_calc)) # fills in the blanks in _calc with the values from w2strokeage (n=4)

# Generate W3 stroke age
waves12345 <- waves12345 %>%
  mutate(w3strokeage_calc = w3strokelast2yry - dobyear)

waves12345 <- waves12345 %>%
  mutate(w3strokeage = coalesce(w3strokeage, w3strokeage_calc)) # fills in the blanks in _calc with the values from w3strokeage

# Generate W4 stroke age
waves12345 <- waves12345 %>%
  mutate(w4strokeage_calc = w4strokelast2yry - dobyear)

waves12345 <- waves12345 %>%
  mutate(w4strokeage = coalesce(w4strokeage, w4strokeage_calc))

# Generate W5 stroke age
waves12345 <- waves12345 %>%
  mutate(w5strokeage_calc = w5strokelast2yry - dobyear)

waves12345 <- waves12345 %>%
  mutate(w5strokeage = coalesce(w5strokeage, w5strokeage_calc))

# Generate W6 stroke age
waves12345 <- waves12345 %>%
  mutate(w6strokeage_calc = w6strokelast2yry - dobyear)

waves12345 <- waves12345 %>%
  mutate(w6strokeage = coalesce(w6strokeage, w6strokeage_calc))

# Generate W7 stroke age
waves12345 <- waves12345 %>%
  mutate(w7strokeage_calc = w7strokelast2yry - dobyear)

waves12345 <- waves12345 %>%
  mutate(w7strokeage = coalesce(w7strokeage, w7strokeage_calc))

#there is no stroke year variable for w8 and w9, so separate code not needed

# Generate 'minstrokeage' as the youngest stroke age reported per participant across waves 0-9
waves12345 <- waves12345 %>% 
  rowwise() %>%
  mutate(minstrokeage = pmin(w098strokeage, w099strokeage, w1strokeage, w2strokeage, w3strokeage, w4strokeage, w5strokeage, w6strokeage, w7strokeage, w8strokeage, w9strokeage, na.rm = TRUE))

#Drop variables only used to calculate stroke age
waves12345$w2strokeage_calc <- NULL
waves12345$w3strokeage_calc <- NULL
waves12345$w4strokeage_calc <- NULL
waves12345$w5strokeage_calc <- NULL
waves12345$w6strokeage_calc <- NULL
waves12345$w7strokeage_calc <- NULL

# Generate variable for total number of strokes (since first stroke) i.e. stroke recurrence
waves12345$ntotalstrokes <- rowSums(waves12345[,c("w2nstrokessincew1", "w3nstrokessincew2", "w4nstrokessincew3", "w5nstrokessincew4", "w6nstrokessincew5", "w7nstrokessincew6", "w8nstrokessincew7", "w9nstrokessincew8")], na.rm=TRUE)

# Generate total number of strokes (since first stroke) in those that had another stroke (i.e excluding zeros)
waves12345$ntotalstrokesnozero <- waves12345$ntotalstrokes
waves12345$ntotalstrokesnozero[waves12345$ntotalstrokesnozero == 0] <- NA

#Create column to calculate the wave that stroke was first reported. (0.5 used for wave 0, as '0' is used to indicate no stroke at all)
waves12345 <- waves12345 %>% 
  mutate(wavefirstreport_stroke = case_when(w098stroke == 1 ~ 0.5, 
                                            w099stroke == 1 ~ 0.5,
                                            w0strokeany == 1 ~ 0.5,
                                            w1strokeany == 1 ~ 1, 
                                            w2strokeany == 1 ~ 2,
                                            w3stroke == 1 ~ 3,
                                            w4stroke == 1 ~ 4,
                                            w5stroke == 1 ~ 5,
                                            w6stroke == 1 ~ 6,
                                            w7stroke == 1 ~ 7,
                                            w8stroke == 1 ~ 8,
                                            w9stroke == 1 ~ 9,
                                            TRUE ~ 0))

#Create column to calculate the wave that psychosis was first reported
waves12345 <- waves12345 %>% 
  mutate(wavefirstreport_psychosis = case_when(w1psychosisany == "1" ~ 1, 
                                               w2psychosisany == "1" ~ 2,
                                               w3psychosisany == "1" ~ 3,
                                               w4psychosisany == "1" ~ 4,
                                               w5psychosisany == "1" ~ 5,
                                               w6psychosisany == "1" ~ 6,
                                               w7psychosisany == "1" ~ 7,
                                               w8psychosisany == "1" ~ 8,
                                               w9psychosisany == "1" ~ 9,
                                               TRUE ~ 0))

#Create column to calculate the wave that depression was first reported
waves12345 <- waves12345 %>% 
  mutate(wavefirstreport_depression = case_when(w1depression == "1" ~ 1, 
                                                w2depression == "1" ~ 2,
                                                w3depression == 1 ~ 3,
                                                w4depression == 1 ~ 4,
                                                w5depression == 1 ~ 5,
                                                w6depression == 1 ~ 6,
                                                w7depression == 1 ~ 7,
                                                w8depression == 1 ~ 8,
                                                w9depression == 1 ~ 9,
                                                TRUE ~ 0))

#Create column to calculate the wave that anxiety was first reported
waves12345 <- waves12345 %>% 
  mutate(wavefirstreport_anxiety = case_when(w1anxiety == "1" ~ 1, 
                                             w2anxiety == "1" ~ 2,
                                             w3anxiety == 1 ~ 3,
                                             w4anxiety == 1 ~ 4,
                                             w5anxiety == 1 ~ 5,
                                             w6anxiety == 1 ~ 6,
                                             w7anxiety == 1 ~ 7,
                                             w8anxiety == 1 ~ 8,
                                             w9anxiety == 1 ~ 9,
                                             TRUE ~ 0))

#Create a column to calculate the first wave number that a participant missed due to death. Mortalitywave levels are defined in the index file data dictionary.
waves12345 <- waves12345 %>%
  mutate(wavefirstreport_death = as.numeric(mortalitywave))
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 1] <- NA
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 2] <- NA
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 3] <- 1
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 4] <- 1
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 5] <- 1
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 6] <- 2
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 7] <- 2
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 8] <- 2
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 9] <- 3
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 10] <- 3
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 11] <- 3
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 12] <- 4
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 13] <- 4
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 14] <- 4
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 15] <- 5
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 16] <- 5
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 17] <- 5
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 18] <- 6
waves12345$wavefirstreport_death[waves12345$wavefirstreport_death == 19] <- 6
#table(waves12345$wavefirstreport_death)

#Create column to calculate if participant died before end of mortality follow-up (wave 6)
waves12345 <- waves12345 %>%
  mutate(diedbeforeendfup = recode(wavefirstreport_death, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1, .missing = 0))
#table(waves12345$diedbeforeendfup) n = 2239

#Create new variable with stroke, psychosis and death order categories
waves12345 <- waves12345 %>% 
  mutate(strokepsychosisorder = case_when(wavefirstreport_stroke == 0 & wavefirstreport_death < 7 & wavefirstreport_psychosis >= 1 ~ 'psychosis then died',
                                          wavefirstreport_psychosis == 0 & wavefirstreport_death < 7 & wavefirstreport_stroke >= 0.5 ~ 'stroke then died',
                                          wavefirstreport_psychosis >= 1 & wavefirstreport_stroke == 0 & !wavefirstreport_death %in% (1:6) ~ 'psychosis only',
                                          wavefirstreport_psychosis == 0 & wavefirstreport_stroke >= 0.5 & !wavefirstreport_death %in% (1:6) ~ 'stroke only',
                                          wavefirstreport_psychosis != 0 & wavefirstreport_stroke != 0 & wavefirstreport_stroke < wavefirstreport_psychosis ~ 'stroke then psychosis',
                                          wavefirstreport_psychosis != 0 & wavefirstreport_stroke != 0 & wavefirstreport_psychosis < wavefirstreport_stroke ~ 'psychosis then stroke',
                                          wavefirstreport_psychosis != 0 & wavefirstreport_stroke != 0 & wavefirstreport_stroke == wavefirstreport_psychosis ~ 'same time',
                                          wavefirstparticipate >= 1 ~ 'no stroke or psychosis',
                                          TRUE ~ 'no participation'))
#table(waves12345$strokepsychosisorder)

#Create column combining all reported ethnicity (where discrepant ethnicity reported across waves, then most common one is used)

#table(waves12345$w1ethnicgroup, useNA = "always")
waves12345$w1ethnicgroup <- as.numeric(waves12345$w1ethnicgroup)
waves12345$w1ethnicgroup[waves12345$w1ethnicgroup == 2] <- 50
waves12345$w1ethnicgroup[is.na(waves12345$w1ethnicgroup)] <- 0

#table(waves12345$w2ethnicgroup, useNA = "always")
waves12345$w2ethnicgroup <- as.numeric(waves12345$w2ethnicgroup)
waves12345$w2ethnicgroup[waves12345$w2ethnicgroup == 2] <- 50
waves12345$w2ethnicgroup[is.na(waves12345$w2ethnicgroup)] <- 0

#table(waves12345$w3ethnicgroup, useNA = "always")
waves12345$w3ethnicgroup <- as.numeric(waves12345$w3ethnicgroup)
waves12345$w3ethnicgroup[waves12345$w3ethnicgroup == 2] <- 50
waves12345$w3ethnicgroup[is.na(waves12345$w3ethnicgroup)] <- 0

#table(waves12345$w4ethnicgroup, useNA = "always")
waves12345$w4ethnicgroup <- as.numeric(waves12345$w4ethnicgroup)
waves12345$w4ethnicgroup[waves12345$w4ethnicgroup == 2] <- 50
waves12345$w4ethnicgroup[is.na(waves12345$w4ethnicgroup)] <- 0

#table(waves12345$w5ethnicgroup, useNA = "always")
waves12345$w5ethnicgroup <- as.numeric(waves12345$w5ethnicgroup)
waves12345$w5ethnicgroup[waves12345$w5ethnicgroup == 2] <- 50
waves12345$w5ethnicgroup[is.na(waves12345$w5ethnicgroup)] <- 0

#table(waves12345$w6ethnicgroup, useNA = "always")
waves12345$w6ethnicgroup <- as.numeric(waves12345$w6ethnicgroup)
waves12345$w6ethnicgroup[waves12345$w6ethnicgroup == 2] <- 50
waves12345$w6ethnicgroup[is.na(waves12345$w6ethnicgroup)] <- 0

#table(waves12345$w7ethnicgroup, useNA = "always")
waves12345$w7ethnicgroup <- as.numeric(waves12345$w7ethnicgroup)
waves12345$w7ethnicgroup[waves12345$w7ethnicgroup == 2] <- 50
waves12345$w7ethnicgroup[is.na(waves12345$w7ethnicgroup)] <- 0

#table(waves12345$w8ethnicgroup, useNA = "always")
waves12345$w8ethnicgroup <- as.numeric(waves12345$w8ethnicgroup)
waves12345$w8ethnicgroup[waves12345$w8ethnicgroup == 2] <- 50
waves12345$w8ethnicgroup[is.na(waves12345$w8ethnicgroup)] <- 0

#table(waves12345$w9ethnicgroup, useNA = "always")
waves12345$w9ethnicgroup <- as.numeric(waves12345$w9ethnicgroup)
waves12345$w9ethnicgroup[waves12345$w9ethnicgroup == 2] <- 50
waves12345$w9ethnicgroup[is.na(waves12345$w9ethnicgroup)] <- 0

waves12345$ethnicgroup <- waves12345$w1ethnicgroup+waves12345$w2ethnicgroup+waves12345$w3ethnicgroup+waves12345$w4ethnicgroup+waves12345$w5ethnicgroup+waves12345$w6ethnicgroup+waves12345$w7ethnicgroup+waves12345$w8ethnicgroup+waves12345$w9ethnicgroup

waves12345$ethnicgroup[waves12345$ethnicgroup == 0] <- NA
waves12345$ethnicgroup[waves12345$ethnicgroup <= 9] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 50] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 51] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 52] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 53] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 54] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 55] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 56] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 57] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 58] <- 1
waves12345$ethnicgroup[waves12345$ethnicgroup == 100] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 101] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 150] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 151] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 152] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 200] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 202] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 250] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 252] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 300] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 350] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 400] <- 2
waves12345$ethnicgroup[waves12345$ethnicgroup == 450] <- 2
#table(waves12345$ethnicgroup, useNA = "always")

waves12345$ethnicgroup[is.na(waves12345$ethnicgroup)] <- 3
waves12345$ethnicgroup <- factor(waves12345$ethnicgroup, levels = c(1, 2, 3), labels = c("White", "Non-White", "Unknown"))
#table(waves12345$ethnicgroup)

#Create column to calculate binary variable of people that had psychosis only (no stroke) n=138
waves12345 <- waves12345 %>% 
  mutate(psychosisonly = case_when(strokepsychosisorder == "psychosis only" ~ 1, 
                                   strokepsychosisorder == "psychosis then died" ~ 1,
                                   TRUE ~ 0))

#Create column to calculate binary variable of people that had stroke and psychosis n=24
waves12345 <- waves12345 %>% 
  mutate(strokepsychosisgroup = case_when(strokepsychosisorder == "stroke then psychosis" ~ 1, 
                                          strokepsychosisorder == "psychosis then stroke" ~ 1,
                                          strokepsychosisorder == "same time" ~ 1,
                                          TRUE ~ 0))

#Create column to calculate binary variable of people that had stroke only (no psychosis) n=1438
waves12345 <- waves12345 %>% 
  mutate(strokeonly = case_when(strokepsychosisorder == "stroke only" ~ 1, 
                                strokepsychosisorder == "stroke then died" ~ 1,
                                TRUE ~ 0))

#Create variable defining stroke/psychosis categories (3 groups - does not consider order of events)
waves12345 <- waves12345 %>% 
  mutate(strokepsychosiscat = case_when(strokepsychosisorder == "psychosis only" ~ "Psychosis only", 
                                        strokepsychosisorder == "psychosis then died" ~ "Psychosis only",
                                        strokepsychosisorder == "stroke only" ~ "Stroke only",
                                        strokepsychosisorder == "stroke then died" ~ "Stroke only",
                                        strokepsychosisorder == "same time" ~ "Stroke and Psychosis",
                                        strokepsychosisorder == "psychosis then stroke" ~ "Stroke and Psychosis",
                                        strokepsychosisorder == "stroke then psychosis" ~ "Stroke and Psychosis",
                                        TRUE ~ "No Stroke or Psychosis"))

#Create variable defining stroke/depression categories (3 groups - does not consider order of events)
waves12345 <- waves12345 %>%
  mutate(strokedepresscat = case_when(strokeever == 1 & depressionever == 1 ~ "Stroke and depression",
                                      strokeever == 0 & depressionever == 1 ~ "Depression only",
                                      strokeever == 1 & depressionever == 0 ~ "Stroke only",
                                      TRUE ~ "No Stroke or Depression"))
#table(waves12345$strokedepresscat)

#Create variable defining stroke/anxiety categories (3 groups - does not consider order of events)
waves12345 <- waves12345 %>%
  mutate(strokeanxcat = case_when(strokeever == 1 & anxietyever == 1 ~ "Stroke and anxiety",
                                  strokeever == 0 & anxietyever == 1 ~ "Anxiety only",
                                  strokeever == 1 & anxietyever == 0 ~ "Stroke only",
                                  TRUE ~ "No Stroke or Anxiety"))
#table(waves12345$strokeanxcat)

#Create variable with each participants baseline alcohol usage (e.g. if they started taking part in w2, then w2 alcohol is used as baseline). If "hi" is displayed then there is an error!
waves12345 <- waves12345 %>%
  mutate(alcoholbaseline = case_when(wavefirstparticipate == 1 ~ w1alcohol,
                                     wavefirstparticipate == 2 ~ w2alcohol,
                                     wavefirstparticipate == 3 ~ w3alcohol,
                                     wavefirstparticipate == 4 ~ w4alcohol,
                                     wavefirstparticipate == 5 ~ w5alcohol,
                                     wavefirstparticipate == 6 ~ w6alcohol,
                                     wavefirstparticipate == 7 ~ w7alcohol,
                                     wavefirstparticipate == 8 ~ w8alcohol,
                                     wavefirstparticipate == 9 ~ w9alcohol,
                                     TRUE ~ "hi"))

#recoded alcohol groups, as different categories were used across different waves:
waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Twice a day or more"] <- "Daily/almost daily"
waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Daily or almost daily"] <- "Daily/almost daily"
waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Almost every day"] <- "Daily/almost daily"
waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Five or six days a week"] <- "Daily/almost daily"

waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Once or twice a week"] <- "1-4 times/week"
waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Three or four days a week"] <- "1-4 times/week"

waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Once or twice a month"] <- "Monthly"

waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Once every couple of months"] <- "Rarely/special occasions only"
waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Once or twice a year"] <- "Rarely/special occasions only"
waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Special occasions only"] <- "Rarely/special occasions only"

#waves12345$alcoholbaseline[waves12345$alcoholbaseline == "Not at all"] # no change required
#table(waves12345$alcoholbaseline)

#Create variable with each participants baseline level of vigorous activity. If "hi" is displayed then there is an error!
waves12345 <- waves12345 %>%
  mutate(vigorousactbaseline = case_when(wavefirstparticipate == 1 ~ w1vigorousphyact,
                                         wavefirstparticipate == 2 ~ w2vigorousphyact,
                                         wavefirstparticipate == 3 ~ w3vigorousphyact,
                                         wavefirstparticipate == 4 ~ w4vigorousphyact,
                                         wavefirstparticipate == 5 ~ w5vigorousphyact,
                                         wavefirstparticipate == 6 ~ w6vigorousphyact,
                                         wavefirstparticipate == 7 ~ w7vigorousphyact,
                                         wavefirstparticipate == 8 ~ w8vigorousphyact,
                                         wavefirstparticipate == 9 ~ w9vigorousphyact,
                                         TRUE ~ "hi"))

#Create variable with each participants baseline smoking status. If "hi" is displayed then there is an error!
waves12345 <- waves12345 %>%
  mutate(smokingbaseline = case_when(wavefirstparticipate == 1 ~ w1smokenow,
                                     wavefirstparticipate == 2 ~ w2smokenow,
                                     wavefirstparticipate == 3 ~ w3smokenow,
                                     wavefirstparticipate == 4 ~ w4smokenow,
                                     wavefirstparticipate == 5 ~ w5smokenow,
                                     wavefirstparticipate == 6 ~ w6smokenow,
                                     wavefirstparticipate == 7 ~ w7smokenow,
                                     wavefirstparticipate == 8 ~ w8smokenow,
                                     wavefirstparticipate == 9 ~ w9smokenow,
                                     TRUE ~ "hi"))

#Create column for baseline region
waves12345 <- waves12345 %>%
  mutate(region = case_when(wavefirstparticipate == 1 ~ w1region,
                            wavefirstparticipate == 2 ~ w2region,
                            wavefirstparticipate == 3 ~ w3region,
                            wavefirstparticipate == 4 ~ w4region,
                            wavefirstparticipate == 5 ~ w5region,
                            wavefirstparticipate == 6 ~ w6region,
                            wavefirstparticipate == 7 ~ w7region,
                            wavefirstparticipate == 8 ~ w8region,
                            wavefirstparticipate == 9 ~ w9region,
                            TRUE ~ "hi"))
#table(waves12345$region)

#Create column for baseline net financial wealth sum
waves12345 <- waves12345 %>%
  mutate(netwealth_sum = case_when(wavefirstparticipate == 1 ~ w1netfw_sum,
                                   wavefirstparticipate == 2 ~ w2netfw_sum,
                                   wavefirstparticipate == 3 ~ w3netfw_sum,
                                   wavefirstparticipate == 4 ~ w4netfw_sum,
                                   wavefirstparticipate == 5 ~ w5netfw_sum,
                                   wavefirstparticipate == 6 ~ w6netfw_sum,
                                   wavefirstparticipate == 7 ~ w7netfw_sum,
                                   wavefirstparticipate == 8 ~ w8netfw_sum,
                                   wavefirstparticipate == 9 ~ w9netfw_sum,
                                   TRUE ~ as.numeric(NA)))
#table(waves12345$netwealth)

#Create column for baseline net financial wealth quintile
waves12345 <- waves12345 %>%
  mutate(netwealth_q5 = case_when(wavefirstparticipate == 1 ~ w1netfw_quintile,
                                  wavefirstparticipate == 2 ~ w2netfw_quintile,
                                  wavefirstparticipate == 3 ~ w3netfw_quintile,
                                  wavefirstparticipate == 4 ~ w4netfw_quintile,
                                  wavefirstparticipate == 5 ~ w5netfw_quintile,
                                  wavefirstparticipate == 6 ~ w6netfw_quintile,
                                  wavefirstparticipate == 7 ~ w7netfw_quintile,
                                  wavefirstparticipate == 8 ~ w8netfw_quintile,
                                  wavefirstparticipate == 9 ~ w9netfw_quintile,
                                  TRUE ~ as.numeric(NA)))
#table(waves12345$netwealth_q5, useNA = "always")

# Create variable with three age categories
waves12345 <- waves12345 %>% 
  mutate(age_cat = case_when(w1age < 60 ~ "<60",
                             w1age >= 60 & w1age <70 ~ "60-69",
                             w1age >= 70 ~ "70+"))
#table(waves12345$age_cat)

#Remove unnecessary data frames from memory
remove(elsaindex_file, wave01998_file, wave01999_file, elsawave0_file, elsawave1_file, elsawave2_file, elsawave3_file, elsawave4_file, elsawave5_file, elsawave6_file, elsawave7_file, elsawave8_file, elsawave9_file,
       elsawave1_financial_file, elsawave2_financial_file, elsawave3_financial_file, elsawave4_financial_file, elsawave5_financial_file, elsawave6_financial_file, elsawave7_financial_file, elsawave8_financial_file, elsawave9_financial_file)

#Save waves12345 to file as an rda file type
save(waves12345, file = paste(output_dir, "waves12345.rda", sep=""))

# CREATE SURVIVAL ANALYSIS DATASETS

# Stroke risk after psychosis

load(file = "waves12345.rda")

strokeinpsychosis_surv <- waves12345 %>%
  select(idauniq, wavefirstparticipate, wavelastparticipate, wavefirstreport_stroke, wavefirstreport_psychosis, strokeever, psychosisever, 
         w1age, sex, ethnicgroup, alcoholbaseline, smokingbaseline, vigorousactbaseline, netwealth_q5, region, age_cat)

# Get data into right format
strokeinpsychosis_surv$wavefirstparticipate <- as.numeric(strokeinpsychosis_surv$wavefirstparticipate)
strokeinpsychosis_surv$wavelastparticipate <- as.numeric(strokeinpsychosis_surv$wavelastparticipate)
strokeinpsychosis_surv$wavefirstreport_stroke <- as.numeric(strokeinpsychosis_surv$wavefirstreport_stroke)
strokeinpsychosis_surv$wavefirstreport_psychosis <- as.numeric(strokeinpsychosis_surv$wavefirstreport_psychosis)
strokeinpsychosis_surv$strokeever <- as.numeric(strokeinpsychosis_surv$strokeever)
strokeinpsychosis_surv$psychosisever <- as.numeric(strokeinpsychosis_surv$psychosisever)
strokeinpsychosis_surv$netwealth_q5 <- factor(strokeinpsychosis_surv$netwealth_q5, levels = c(5, 4, 3, 2, 1))

# Assign numeric labels to categorical variables (reference group == 1 i.e. the most healthy level of the variable). 

# Vigorous physical activity
strokeinpsychosis_surv$vigorousactbaseline[strokeinpsychosis_surv$vigorousactbaseline == "hardly ever, or never"] <- 4
strokeinpsychosis_surv$vigorousactbaseline[strokeinpsychosis_surv$vigorousactbaseline == "one to three times a month"] <- 3
strokeinpsychosis_surv$vigorousactbaseline[strokeinpsychosis_surv$vigorousactbaseline == "once a week"] <- 2
strokeinpsychosis_surv$vigorousactbaseline[strokeinpsychosis_surv$vigorousactbaseline == "more than once a week"] <- 1

# Alcohol use
strokeinpsychosis_surv$alcoholbaseline[strokeinpsychosis_surv$alcoholbaseline == "Daily/almost daily"] <- 5
strokeinpsychosis_surv$alcoholbaseline[strokeinpsychosis_surv$alcoholbaseline == "1-4 times/week"] <- 4
strokeinpsychosis_surv$alcoholbaseline[strokeinpsychosis_surv$alcoholbaseline == "Monthly"] <- 3
strokeinpsychosis_surv$alcoholbaseline[strokeinpsychosis_surv$alcoholbaseline == "Rarely/special occasions only"] <- 2
strokeinpsychosis_surv$alcoholbaseline[strokeinpsychosis_surv$alcoholbaseline == "Not at all"] <- 1

# First, account for strokes reported in wave 0 e.g. wave 0.5 (0 is already used to mean no stroke). 
# Where first reported stroke is 0.5, use 0.5 as first wave, otherwise use 'wavefirstparticipate'.
strokeinpsychosis_surv <- strokeinpsychosis_surv %>%
  mutate(wavefirstparticipate = case_when(wavefirstreport_stroke == 0.5 ~ 0.5,
                                          TRUE ~ wavefirstparticipate))

# Create follow-up time variables
strokeinpsychosis_surv <- strokeinpsychosis_surv %>%
  mutate(fuptime = case_when(psychosisever == 1 & strokeever == 0 ~ wavelastparticipate - wavefirstreport_psychosis, #psychosis but no stroke, f-up starts from report of psychosis to last wave participated
                             psychosisever == 0 & strokeever == 1 ~ wavefirstreport_stroke - wavefirstparticipate, #stroke but no psychosis, f-up starts from wave first participated until stroke is reported
                             psychosisever == 0 & strokeever == 0 ~ wavelastparticipate - wavefirstparticipate, #no stroke and no psychosis, use full available follow-up (from first wave to last wave participated)
                             wavefirstreport_stroke > 0 & wavefirstreport_stroke < wavefirstreport_psychosis ~ 0, #if first stroke happened prior to report of psychosis, then f-up time = 0
                             wavefirstreport_stroke == wavefirstreport_psychosis ~ 0, #if stroke and psychosis reported at the same time, then f-up time = 0
                             psychosisever == 1 & strokeever == 1 ~ wavefirstreport_stroke - wavefirstreport_psychosis, #psychosis and stroke, f-up starts from report of psychosis until stroke is reported
                             TRUE ~ 999)) # no 999s appear so everyone has been coded as per the above

# Convert follow-up time from waves to years (follow-up occurred every two years i.e. *2)
strokeinpsychosis_surv <- strokeinpsychosis_surv %>%
  mutate(fuptime = fuptime * 2)

#Create strokeever variable censored at 10 years (any events occuring after 10 years are coded as 0)
strokeinpsychosis_surv <- strokeinpsychosis_surv %>%
  mutate(strokeever_10 = case_when(strokeever == 1 & fuptime > 10 ~ 0,
                                   TRUE ~ strokeever))

#Create strokeever variable censored at 4 years
strokeinpsychosis_surv <- strokeinpsychosis_surv %>%
  mutate(strokeever_4 = case_when(strokeever == 1 & fuptime > 4 ~ 0,
                                   TRUE ~ strokeever))

#table(strokeinpsychosis_surv$strokeever)
#table(strokeinpsychosis_surv$strokeever_10)

# Psychosis risk after stroke

load(file = "waves12345.rda")

#Pull out variables needed for this analysis (psychosis in stroke population - outcome, exposure and covariates)
psychosisinstroke_surv <- waves12345 %>%
  select(idauniq, wavefirstparticipate, wavelastparticipate, wavefirstreport_stroke, wavefirstreport_psychosis, strokeever, psychosisever, 
         w1age, sex, ethnicgroup, alcoholbaseline, smokingbaseline, vigorousactbaseline, netwealth_q5, region, age_cat)

#Get data into right format
psychosisinstroke_surv$wavefirstparticipate <- as.numeric(psychosisinstroke_surv$wavefirstparticipate)
psychosisinstroke_surv$wavelastparticipate <- as.numeric(psychosisinstroke_surv$wavelastparticipate)
psychosisinstroke_surv$wavefirstreport_stroke <- as.numeric(psychosisinstroke_surv$wavefirstreport_stroke)
psychosisinstroke_surv$wavefirstreport_psychosis <- as.numeric(psychosisinstroke_surv$wavefirstreport_psychosis)
psychosisinstroke_surv$strokeever <- as.numeric(psychosisinstroke_surv$strokeever)
psychosisinstroke_surv$psychosisever <- as.numeric(psychosisinstroke_surv$psychosisever)
psychosisinstroke_surv$netwealth_q5 <- factor(psychosisinstroke_surv$netwealth_q5, levels = c(5, 4, 3, 2, 1))

#Assign numeric labels to cateogrical variables (reference group == 1 i.e. the most healthy level of the variable). 
#If the categories are used then results are not shown in an order than makes sense.

#Vigorous physical activity
psychosisinstroke_surv$vigorousactbaseline[psychosisinstroke_surv$vigorousactbaseline == "hardly ever, or never"] <- 4
psychosisinstroke_surv$vigorousactbaseline[psychosisinstroke_surv$vigorousactbaseline == "more than once a week"] <- 1
psychosisinstroke_surv$vigorousactbaseline[psychosisinstroke_surv$vigorousactbaseline == "once a week"] <- 2
psychosisinstroke_surv$vigorousactbaseline[psychosisinstroke_surv$vigorousactbaseline == "one to three times a month"] <- 3
#table(psychosisinstroke_surv$vigorousactbaseline)

#Alcohol use
psychosisinstroke_surv$alcoholbaseline[psychosisinstroke_surv$alcoholbaseline == "1-4 times/week"] <- 4
psychosisinstroke_surv$alcoholbaseline[psychosisinstroke_surv$alcoholbaseline == "Daily/almost daily"] <- 5
psychosisinstroke_surv$alcoholbaseline[psychosisinstroke_surv$alcoholbaseline == "Monthly"] <- 3
psychosisinstroke_surv$alcoholbaseline[psychosisinstroke_surv$alcoholbaseline == "Not at all"] <- 1
psychosisinstroke_surv$alcoholbaseline[psychosisinstroke_surv$alcoholbaseline == "Rarely/special occasions only"] <- 2
#table(psychosisinstroke_surv$alcoholbaseline)
#table(psychosisinstroke_surv$ethnicgroup)

# First, account for strokes reported in wave 0 e.g. wave 0.5 (0 is already used to mean no stroke). 
# Where first reported stroke is 0.5, use 0.5 as first wave, otherwise use 'wavefirstparticipate'.
psychosisinstroke_surv <- psychosisinstroke_surv %>%
  mutate(wavefirstparticipate = case_when(wavefirstreport_stroke == 0.5 ~ 0.5,
                                          TRUE ~ wavefirstparticipate))

#Create follow-up time variables
psychosisinstroke_surv <- psychosisinstroke_surv %>%
  mutate(fuptime = case_when(psychosisever == 1 & strokeever == 0 ~ wavefirstreport_psychosis - wavefirstparticipate, #psychosis but no stroke, f-up starts from wave first participated until first report of psychosis
                             psychosisever == 0 & strokeever == 1 ~ wavelastparticipate - wavefirstreport_stroke, #stroke but no psychosis, f-up starts from wave stroke first reported until wave last participated
                             psychosisever == 0 & strokeever == 0 ~ wavelastparticipate - wavefirstparticipate, #no stroke and no psychosis, use full available follow-up (from first wave to last wave participated)
                             wavefirstreport_psychosis > 0 & wavefirstreport_psychosis < wavefirstreport_stroke ~ 0, #if first psychosis report happened prior to report of stroke, then f-up time = 0
                             wavefirstreport_stroke == wavefirstreport_psychosis ~ 0, #if stroke and psychosis reported at the same time, then f-up time = 0
                             psychosisever == 1 & strokeever == 1 ~ wavefirstreport_psychosis - wavefirstreport_stroke, #psychosis and stroke, f-up starts from first report of stroke until psychosis is reported
                             TRUE ~ 999))      
# no 999s appear so everyone has been coded as per the above

#Convert follow-up time from waves to years (follow-up occurred every two years i.e. *2)
psychosisinstroke_surv <- psychosisinstroke_surv %>%
  mutate(fuptime = fuptime * 2)

#Create psychosisever variable censored at 10 years
psychosisinstroke_surv <- psychosisinstroke_surv %>%
  mutate(psychosisever_10 = case_when(psychosisever == 1 & fuptime > 10 ~ 0,
                                      TRUE ~ psychosisever))

#Create psychosisever variable censored at 4 years
psychosisinstroke_surv <- psychosisinstroke_surv %>%
  mutate(psychosisever_4 = case_when(psychosisever == 1 & fuptime > 4 ~ 0,
                                      TRUE ~ psychosisever))


#Merge censored variables into both dataframes
censoredvar_psychosis <- psychosisinstroke_surv %>%
  select(idauniq, psychosisever_10, psychosisever_4)

censoredvar_stroke <- strokeinpsychosis_surv %>%
  select(idauniq, strokeever_10, strokeever_4)

psychosisinstroke_surv <- merge(psychosisinstroke_surv, censoredvar_stroke, by="idauniq", all = TRUE)
strokeinpsychosis_surv <- merge(strokeinpsychosis_surv, censoredvar_psychosis, by="idauniq", all = TRUE)

remove(censoredvar_psychosis, censoredvar_stroke)

save(strokeinpsychosis_surv, file = paste(output_dir, "strokeinpsychosis_surv.rda", sep=""))
save(psychosisinstroke_surv, file = paste(output_dir, "psychosisinstroke_surv.rda", sep=""))

# MULTIPLE IMPUTATION

#Alvin: load files
load(file = "waves12345.rda")
load(file = "strokeinpsychosis_surv.rda")
load(file = "psychosisinstroke_surv.rda")

#Select variables for inclusion in multiple imputation (baseline covariates)
strokeinpsychosis_imp <- waves12345 %>%
  select(idauniq, netwealth_q5, alcoholbaseline, smokingbaseline, vigorousactbaseline, w1age, sex, region, ethnicgroup)

# Convert characters into factors
strokeinpsychosis_imp$netwealth_q5 <- as.factor(strokeinpsychosis_imp$netwealth_q5)
strokeinpsychosis_imp$alcoholbaseline <- as.factor(strokeinpsychosis_imp$alcoholbaseline)
strokeinpsychosis_imp$smokingbaseline <- as.factor(strokeinpsychosis_imp$smokingbaseline)
strokeinpsychosis_imp$vigorousactbaseline <- as.factor(strokeinpsychosis_imp$vigorousactbaseline)
strokeinpsychosis_imp$sex <- as.factor(strokeinpsychosis_imp$sex)
strokeinpsychosis_imp$region <- as.factor(strokeinpsychosis_imp$region)
strokeinpsychosis_imp$ethnicgroup <- as.factor(strokeinpsychosis_imp$ethnicgroup)

# Change 'unknown' ethnicities to NA
strokeinpsychosis_imp$ethnicgroup[strokeinpsychosis_imp$ethnicgroup == "Unknown"] <- NA

# Make sure format of data is a data frame (required for missForest to run)
strokeinpsychosis_imp <- as.data.frame(strokeinpsychosis_imp)

# Register parallel process with doParallel
registerDoParallel()

# Set seed for reproducibility
registerDoRNG(seed = 123)

# Record start time
start_time <- Sys.time()

# Impute
data.imp <- missForest(xmis = strokeinpsychosis_imp, parallelize = "forests")

# Record end time
end_time <- Sys.time()

# Report time duration of missForest imputation
end_time - start_time

# Check normalized root mean squared error
data.imp$OOBerror

# Create data frame with imputed data
imputeddata <- data.imp$ximp

# Pull out other variables needed for this survival analysis
strokeinpsychosis_variables <- strokeinpsychosis_surv %>%
  select(idauniq, strokeever, psychosisever, fuptime, age_cat, strokeever_10, strokeever_4, psychosisever_10, psychosisever_4)

# Merge imputed data with newly created data frame above
strokeinpsychosis_imp <- merge(strokeinpsychosis_variables, imputeddata, by="idauniq", all.x = TRUE) # all.y keeps unmatched records in the second file, all.x would keep all unmatched records from first file.

# Create new data frame for this survival analysis
psychosisinstroke_imp <- strokeinpsychosis_imp

# Remove stroke in psychosis f-up time variable
psychosisinstroke_imp$fuptime <- NULL

# Merge in psychosis in stroke f-up time variable 
psychosisinstroke_variables <- psychosisinstroke_surv %>%
  select(idauniq, fuptime)

psychosisinstroke_imp <- merge(psychosisinstroke_variables, psychosisinstroke_imp, by="idauniq", all.x = TRUE) # all.y keeps unmatched records in the second file, all.x would keep all unmatched records from first file.

psychosisinstroke_imp$netwealth_q5 <- factor(psychosisinstroke_imp$netwealth_q5, levels = c(5, 4, 3, 2, 1))
strokeinpsychosis_imp$netwealth_q5 <- factor(strokeinpsychosis_imp$netwealth_q5, levels = c(5, 4, 3, 2, 1))


#Save files
save(strokeinpsychosis_imp, file = paste(output_dir, "strokeinpsychosis_imp.rda", sep=""))
save(psychosisinstroke_imp, file = paste(output_dir, "psychosisinstroke_imp.rda", sep=""))

#Versions

# packageVersion("dplyr")

# packageVersion("foreign")

# packageVersion("stringr")

# packageVersion("missForest")

# packageVersion("doParallel")

# packageVersion("doRNG")

