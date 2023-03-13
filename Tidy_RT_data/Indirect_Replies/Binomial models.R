#Binomial models
# Indirect_Replies tidy script

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
library(ggstatsplot)

#Set seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)

library(readr)
alldata_IR_RT <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT.csv", 
                          col_types = cols(Group_Status = col_factor(levels = c("ASC",                                                                                 "TD"))))


#NO NA's so we don't need to throw away any 0 values like we do in other scripts when there has been a problem
#with data collection using the eyetracker.
#sum(is.na(alldata_IR_RT$RT1))
#sum(is.na(alldata_IR_RT$RT2))
#sum(is.na(alldata_IR_RT$RT3))
#sum(is.na(alldata_IR_RT$RT4))
#sum(is.na(alldata_IR_RT$RT5))
#sum(is.na(alldata_IR_RT$RT6))

#Create ms. over second so as to be comparable with previous studies
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT1ms = RT1*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT2ms = RT2*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT3ms = RT3*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT4ms = RT4*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT5ms = RT5*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT6ms = RT6*1000)

#Now Let's add in individual differences
#Import Individual difference measures
#Reduced_IDs_IR <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Reduced_IDs_IR.csv")
#View(Reduced_IDs_IR)
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Reduced_IDs_IR.csv")

library(readr)
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Reduced_IDs_IR.csv", 
                           col_types = cols(participant = col_number(), 
                                            Age = col_number(), Gender = col_factor(levels = c("Female", 
                                                                                               "Male")), Comprehension_accuracy_IR = col_number(), 
                                            SRS_total_score_raw = col_number(), 
                                            SRS_total_score_t = col_number(), 
                                            EQ = col_number(), Total_RAN = col_number(), 
                                            Total_reading_cluster = col_number()))

all_data_join <- inner_join(alldata_IR_RT, Reduced_IDs_IR, by = "participant")


model_SRS <- glmer(Group_Status ~ SRS_total_score_t + (1 | participant), data = all_data_join, family = "binomial")
summary(model_SRS)

model_EQ <- glmer(Group_Status ~ EQ + (1 | participant), data = all_data_join, family = "binomial")
summary(model_EQ)





