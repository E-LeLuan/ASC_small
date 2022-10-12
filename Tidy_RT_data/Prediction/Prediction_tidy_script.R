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

#Rename condition_number to more meaningful numbers
alldata_Pred_RT$condition_number <- recode(alldata_Pred_RT$condition_number, "1" = "facilitated", "2" = "unfacilitated")
#view(alldata_Pred_RT)

#NO NA's so we don't need to throw away any 0 values like we do in other scripts when there has been a problem
#with data collection using the eyetracker.
#sum(is.na(alldata_Pred_RT$RT1))
#sum(is.na(alldata_Pred_RT$RT2))
#sum(is.na(alldata_Pred_RT$RT3))
#sum(is.na(alldata_Pred_RT$RT4))
#sum(is.na(alldata_Pred_RT$RT5))
#sum(is.na(alldata_Pred_RT$RT6))


################Lognormal analysis as Weibull is closest to lognormal and gamma#############################

# Let's have a look at region 3 Which is our Prediction region

#Violin plots
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT3, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT3, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Violin plots by group_status
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT3, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT3, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Descriptives
alldata_Pred_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3), sd(RT3))



# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops participant and item random effects
modelRT3 <- lmer(RT3 ~ condition_number + (1 | participant) + (1 | item_number), data = alldata_Pred_RT,
                 REML = TRUE) 
summary(modelRT3)

model.nullRT3 <- lmer(RT3 ~ (1 | participant) + (1 | item_number), alldata_Pred_RT) 

anova(modelRT3,model.nullRT3)

#add in group_stATUS and shows TD driving the effect
#Maximal model with no singularity of fit error drops participant and item random effects
modelRT3GS <- lmer(RT3 ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = alldata_Pred_RT,
                   REML = TRUE) 
summary(modelRT3GS)

#All the data for this model looks pretty normal.
check_model(modelRT3)
qqnorm(residuals(modelRT3))
qqline(residuals(modelRT3))
descdist(alldata_Pred_RT$RT3)




# Let's have a look at region 4 Which is our critical/ Question region

#Violin plots
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT4, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT4, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Violin plots by group_status
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT4, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT4, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Descriptives
alldata_Pred_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4), sd(RT4))



# Model assuming normality of residuals maximal structure

#set condition as a factor doesnt make model run
#alldata_Pred_RT$condition_number <- as.factor(alldata_Pred_RT$condition_number)
#alldata_Pred_RT$participant <- as.factor(alldata_Pred_RT$participant)
#alldata_Pred_RT$item_number <- as.factor(alldata_Pred_RT$item_number)
#Changing to numeric doesnt help either!!!!!!1
#alldata_Pred_RT <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Prediction/alldata_Pred_RT.csv", 
 #                           col_types = cols(RT1 = col_number(), 
  #                                           RT2 = col_number(), RT3 = col_number(), 
   #                                          RT4 = col_number(), RT5 = col_number(), 
    #                                         RT6 = col_number(), Comp_Question_RT = col_number(), 
     #                                        item_number = col_number()))


#model.nullR4 <- lmer(RT4 ~ (1 + condition_number | participant) + (1 + condition_number | item_number), alldata_Pred_RT) 
#Maximal model with no singularity of fit error drops item random effects
modelRT4 <- lmer(RT4 ~ condition_number + (1 + condition_number | participant) + (1 | item_number), data = alldata_Pred_RT,
                 REML = TRUE) 
summary(modelRT4)

model.nullRT4 <- lmer(RT4 ~ (1 + condition_number | participant) + (1 | item_number), alldata_Pred_RT) 

anova(modelRT4,model.nullRT4)

#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing.
#Maximal model with no singularity of fit error drops item random effects
modelRT4GS <- lmer(RT4 ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), data = alldata_Pred_RT,
                 REML = TRUE) 
summary(modelRT4GS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop

# It Worked!!!!!

#anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelRT4)
#qqnorm(residuals(modelRT4))
#qqline(residuals(modelRT4))
descdist(alldata_Pred_RT$RT4)





# Let's have a look at region 5 Which is our post-critical/ REply region

#Violin plots
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT5, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT5, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Violin plots by group_status
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT5, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT5, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Descriptives
alldata_Pred_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5), sd(RT5))



# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops item random effects
modelRT5 <- lmer(RT5 ~ condition_number + (1 | participant) + (1 | item_number), data = alldata_Pred_RT,
                 REML = TRUE) 
summary(modelRT5)

model.nullRT5 <- lmer(RT5 ~ (1 | participant) + (1 | item_number), alldata_Pred_RT) 

anova(modelRT5,model.nullRT5)

#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing mechanisms for ASC and TD.
#Maximal model with no singularity of fit error drops item random effects
modelRT5GS <- lmer(RT5 ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = alldata_Pred_RT,
                   REML = TRUE) 
summary(modelRT5GS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop

# It Worked!!!!!

#anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelRT5)
qqnorm(residuals(modelRT5))
qqline(residuals(modelRT5))
descdist(alldata_Pred_RT$RT5)


## Let's have a look at total reading time across all regions

alldata_Pred_RT <- alldata_Pred_RT %>% group_by(participant) %>%
  mutate(TT = (RT1 + RT2 + RT3 + RT4 + RT5 + RT6))

#view(alldata_Pred_RT)
#IT WORKED!!!!!
#Violin plots
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT5, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = TT, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Violin plots by group_status
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Descriptives
alldata_Pred_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(TT), sd(TT))



# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops item random effects
modelTT <- lmer(TT ~ condition_number + (1 | participant) + (1 + condition_number | item_number), data = alldata_Pred_RT,
                 REML = TRUE) 
summary(modelTT)

model.nullTT <- lmer(TT ~ (1 | participant) + (1 + condition_number | item_number), alldata_Pred_RT) 

anova(modelTT,model.nullTT)

#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing mechanisms for ASC and TD.
#Maximal model with no singularity of fit error drops item random effects
modelTTGS <- lmer(TT ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = alldata_Pred_RT,
                   REML = TRUE) 
summary(modelTTGS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop

# It Worked!!!!!

#anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelTT)
qqnorm(residuals(modelTT))
qqline(residuals(modelTT))
descdist(alldata_Pred_RT$TT)



################Lognormal analysis as Weibull is closest to lognormal and gamma#############################
#With Gamma we can include more random effects including maximal structure with random slopes for particiapnt and item 

#Nothing significant with Gamma (if i did it right not sure if after family = gamma i shouldn't have (link = "log") or (link = "inverse")) 

GammaRT3 <- glmer(RT3 ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                  family = Gamma (link = "inverse"), data = alldata_Pred_RT)
summary(GammaRT3)

GammaRT4 <- glmer(RT4 ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                  family = Gamma (link = "inverse"), data = alldata_Pred_RT)
summary(GammaRT4)

GammaRT5 <- glmer(RT5 ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                  family = Gamma (link = "inverse"), data = alldata_Pred_RT)
summary(GammaRT5)

GammaRTT <- glmer(TT ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                  family = Gamma (link = "inverse"), data = alldata_Pred_RT)

summary(GammaRTT)

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


