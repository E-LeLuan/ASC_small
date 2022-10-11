# Prediction tidy script

library(Matrix)
library(lme4)
library(lmerTest)
library(emmeans)
library(stats)
library(fitdistrplus)
library(tidyverse)
library(buildmer)
library(performance)
library(see)
library(sjPlot)
#Set seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
library(readr)
P1_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P1_ASC_SMALL.csv")
P2_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P2_ASC_SMALL.csv")
P3_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P3_ASC_SMALL.csv")
P4_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P4_ASC_SMALL.csv")
P5_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P5_ASC_SMALL.csv")
P6_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P6_ASC_SMALL.csv")
P7_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P7_ASC_SMALL.csv")
P8_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P8_ASC_SMALL.csv")
P9_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P9_ASC_SMALL.csv")
P10_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P10_ASC_SMALL.csv")
P11_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P11_ASC_SMALL.csv")
P12_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P12_ASC_SMALL.csv")
P13_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P13_ASC_SMALL.csv")
P14_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P14_ASC_SMALL.csv")
P15_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P15_ASC_SMALL.csv")
P16_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P16_ASC_SMALL.csv")
P17_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P17_ASC_SMALL.csv")
P18_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P18_ASC_SMALL.csv")
P19_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P19_ASC_SMALL.csv")
P20_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P20_ASC_SMALL.csv")
P21_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P21_ASC_SMALL.csv")
P22_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P22_ASC_SMALL.csv")
P23_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P23_ASC_SMALL.csv")
P24_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P24_ASC_SMALL.csv")
P25_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P25_ASC_SMALL.csv")
P26_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P26_ASC_SMALL.csv")
P27_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P27_ASC_SMALL.csv")
P28_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P28_ASC_SMALL.csv")
P29_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P29_ASC_SMALL.csv")
P30_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P30_ASC_SMALL.csv")
P31_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P31_ASC_SMALL.csv")
P32_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P32_ASC_SMALL.csv")
P33_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P33_ASC_SMALL.csv")
P34_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P34_ASC_SMALL.csv")
P35_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P35_ASC_SMALL.csv")
P36_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P36_ASC_SMALL.csv")
P37_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P37_ASC_SMALL.csv")
P38_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P38_ASC_SMALL.csv")
P39_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P39_ASC_SMALL.csv")
P40_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P40_ASC_SMALL.csv")
#P41_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P41_ASC_SMALL.csv")
P42_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P42_ASC_SMALL.csv")
P43_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P43_ASC_SMALL.csv")
P44_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P44_ASC_SMALL.csv")
P45_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P45_ASC_SMALL.csv")
P46_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P46_ASC_SMALL.csv")
P47_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P47_ASC_SMALL.csv")
P48_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P48_ASC_SMALL.csv")
P49_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P49_ASC_SMALL.csv")
P50_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P50_ASC_SMALL.csv")
P51_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P51_ASC_SMALL.csv")
P52_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P52_ASC_SMALL.csv")
P53_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P53_ASC_SMALL.csv")
P54_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P54_ASC_SMALL.csv")
P55_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P55_ASC_SMALL.csv")
P56_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P56_ASC_SMALL.csv")
P57_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P57_ASC_SMALL.csv")
P58_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P58_ASC_SMALL.csv")
P59_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P59_ASC_SMALL.csv")
P60_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P60_ASC_SMALL.csv")

# Combining the individual data spreadsheets into one data frame.

alldata_Pred_RT <- rbind (P1_ASC_SMALL, P2_ASC_SMALL, P3_ASC_SMALL,P4_ASC_SMALL, P5_ASC_SMALL,
                  P6_ASC_SMALL, P7_ASC_SMALL, P8_ASC_SMALL, P9_ASC_SMALL, P10_ASC_SMALL,
                  P11_ASC_SMALL, P12_ASC_SMALL, P13_ASC_SMALL, P14_ASC_SMALL, P15_ASC_SMALL,
                  P16_ASC_SMALL,P17_ASC_SMALL, P18_ASC_SMALL, P19_ASC_SMALL,P20_ASC_SMALL,
                  P21_ASC_SMALL, P22_ASC_SMALL, P23_ASC_SMALL,P24_ASC_SMALL, P25_ASC_SMALL, P26_ASC_SMALL,
                  P27_ASC_SMALL, P28_ASC_SMALL, P29_ASC_SMALL, P30_ASC_SMALL, P31_ASC_SMALL,P32_ASC_SMALL,
                  P33_ASC_SMALL, P34_ASC_SMALL, P35_ASC_SMALL, P36_ASC_SMALL, P37_ASC_SMALL, P38_ASC_SMALL,
                  P39_ASC_SMALL,P40_ASC_SMALL,P42_ASC_SMALL, P43_ASC_SMALL, P44_ASC_SMALL,
                  P45_ASC_SMALL, P46_ASC_SMALL, P47_ASC_SMALL,P48_ASC_SMALL, P49_ASC_SMALL, P50_ASC_SMALL,
                  P51_ASC_SMALL, P52_ASC_SMALL, P53_ASC_SMALL, P54_ASC_SMALL, P55_ASC_SMALL,P56_ASC_SMALL,
                  P57_ASC_SMALL, P58_ASC_SMALL,P59_ASC_SMALL, P60_ASC_SMALL)

#view(alldata_Pred_RT)

alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
alldata_Pred_RT$Group_Status[alldata_Pred_RT$Group_Status == 'TRUE'] <- "ASC"
alldata_Pred_RT$Group_Status[alldata_Pred_RT$Group_Status == 'FALSE'] <- "TD"
#view(alldata_Pred_RT)

#Export a CSV of the new data set...
write.csv(alldata_Pred_RT,"//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Prediction\\alldata_Pred_RT.csv", row.names = TRUE)

#Import ID's
alldata_Pred_RT <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Prediction/alldata_Pred_RT.csv")
alldata_EQ <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/EQ_data/alldata_EQ.csv")
alldata_SRS2 <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/SRS2_data/alldata_SRS2.csv")

#view(alldata_EQ)
#view(alldata_SRS2)

all_data_join <- inner_join(alldata_Pred_RT, alldata_EQ, by = "participant")
all_data_join <- inner_join(all_data_join, alldata_SRS2, by = "participant")
#view(all_data_join)


