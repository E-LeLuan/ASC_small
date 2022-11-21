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

#Set seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)

library(readr)
P1_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P1_ASC_SMALL.csv")
P2_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P2_ASC_SMALL.csv")
P3_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P3_ASC_SMALL.csv")
P4_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P4_ASC_SMALL.csv")
P5_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P5_ASC_SMALL.csv")
P6_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P6_ASC_SMALL.csv")
P7_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P7_ASC_SMALL.csv")
P8_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P8_ASC_SMALL.csv")
P9_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P9_ASC_SMALL.csv")
P10_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P10_ASC_SMALL.csv")
P11_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P11_ASC_SMALL.csv")
P12_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P12_ASC_SMALL.csv")
P13_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P13_ASC_SMALL.csv")
P14_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P14_ASC_SMALL.csv")
P15_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P15_ASC_SMALL.csv")
P16_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P16_ASC_SMALL.csv")
P17_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P17_ASC_SMALL.csv")
P18_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P18_ASC_SMALL.csv")
P19_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P19_ASC_SMALL.csv")
P20_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P20_ASC_SMALL.csv")
P21_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P21_ASC_SMALL.csv")
P22_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P22_ASC_SMALL.csv")
P23_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P23_ASC_SMALL.csv")
P24_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P24_ASC_SMALL.csv")
P25_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P25_ASC_SMALL.csv")
P26_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P26_ASC_SMALL.csv")
P27_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P27_ASC_SMALL.csv")
P28_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P28_ASC_SMALL.csv")
P29_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P29_ASC_SMALL.csv")
P30_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P30_ASC_SMALL.csv")
P31_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P31_ASC_SMALL.csv")
P32_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P32_ASC_SMALL.csv")
P33_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P33_ASC_SMALL.csv")
P34_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P34_ASC_SMALL.csv")
P35_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P35_ASC_SMALL.csv")
P36_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P36_ASC_SMALL.csv")
P37_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P37_ASC_SMALL.csv")
P38_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P38_ASC_SMALL.csv")
P39_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P39_ASC_SMALL.csv")
P40_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P40_ASC_SMALL.csv")
#P41_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P41_ASC_SMALL.csv")
P42_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P42_ASC_SMALL.csv")
P43_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P43_ASC_SMALL.csv")
P44_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P44_ASC_SMALL.csv")
P45_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P45_ASC_SMALL.csv")
P46_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P46_ASC_SMALL.csv")
P47_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P47_ASC_SMALL.csv")
P48_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P48_ASC_SMALL.csv")
P49_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P49_ASC_SMALL.csv")
P50_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P50_ASC_SMALL.csv")
P51_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P51_ASC_SMALL.csv")
P52_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P52_ASC_SMALL.csv")
P53_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P53_ASC_SMALL.csv")
P54_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P54_ASC_SMALL.csv")
P55_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P55_ASC_SMALL.csv")
P56_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P56_ASC_SMALL.csv")
P57_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P57_ASC_SMALL.csv")
P58_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P58_ASC_SMALL.csv")
P59_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P59_ASC_SMALL.csv")
P60_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P60_ASC_SMALL.csv")

# Combining the individual data spreadsheets into one data frame.

alldata_IR_RT <- rbind (P1_ASC_SMALL, P2_ASC_SMALL, P3_ASC_SMALL,P4_ASC_SMALL, P5_ASC_SMALL,
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

view(alldata_IR_RT)

alldata_IR_RT <- alldata_IR_RT%>%
  mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
alldata_IR_RT$Group_Status[alldata_IR_RT$Group_Status == 'TRUE'] <- "ASC"
alldata_IR_RT$Group_Status[alldata_IR_RT$Group_Status == 'FALSE'] <- "TD"
#view(alldata_IR_RT)

#Rename condition_number to more meaningful numbers
alldata_IR_RT$condition_number <- recode(alldata_IR_RT$condition_number, "1" = "Negative", "2" = "Positive", "3" = "Neutral")
view(alldata_IR_RT)

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


################Lognormal analysis as Weibull is closest to lognormal and gamma#############################

# Let's have a look at region 2 Which is our Indirect_Replies manipulation region
# Postitive, negative, neutral

#view(alldata_IR_RT)

alldata_IR_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT2ms), sd(RT2ms))

#Violin plots
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = condition_number)) + ggtitle("Reaction Time Region 2") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = condition_number)) + ggtitle("Reaction Time Region 2") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Violin plots by group_status
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = Group_Status)) + ggtitle("Reaction Time Region 2") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = Group_Status)) + ggtitle("Reaction Time Region 2") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)


# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops participant and item random effects
modelRT2ms <- lmer(RT2ms ~ condition_number + (1 | participant) + (1 | item_number), data = alldata_IR_RT,
                   REML = TRUE) 
summary(modelRT2ms)

model.nullRT2ms <- lmer(RT3ms ~ (1 | participant) + (1 | item_number), alldata_IR_RT) 

anova(modelRT2ms,model.nullRT2ms)


#All the data for this model looks pretty normal.
check_model(modelRT2ms)
qqnorm(residuals(modelRT2ms))
qqline(residuals(modelRT2ms))
descdist(alldata_IR_RT$RT2ms)

#Now Let's add in individual differences
#Import Individual difference measures
alldata_SRS2 <- read_csv("SRS2_data/alldata_SRS2.csv")
alldata_EQ <- read_csv("EQ_data/alldata_EQ.csv")

all_data_join <- inner_join(alldata_IR_RT, alldata_SRS2, by = "participant")
all_data_join <- inner_join(all_data_join, alldata_EQ, by = "participant")

#View(all_data_join)

# Scale the ID measures...
all_data_join$total_t_score <- scale(all_data_join$total_t_score)
all_data_join$EQ_score <- scale(all_data_join$EQ_score)

# Model including covariates
model_alldatacov_RT2ms <- lmer(RT2ms ~ total_t_score + EQ_score + condition_number + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_RT2ms)

#model failed to converge will have to simplify more






# Let's have a look at region 3 Which is our Indirect_Replies region
# Let's have a look at region 3 Which is our Indirect_Replies region

#view(alldata_IR_RT)

alldata_IR_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

#Violin plots
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = condition_number)) + ggtitle("Reaction Time Region 3") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = condition_number)) + ggtitle("Reaction Time Region 3") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Violin plots by group_status
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Reaction Time Region 3") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)


# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops participant and item random effects
modelRT3ms <- lmer(RT3ms ~ condition_number + (1 | participant) + (1 | item_number), data = alldata_IR_RT,
                   REML = TRUE) 
summary(modelRT3ms)

model.nullRT3ms <- lmer(RT3ms ~ (1 | participant) + (1 | item_number), alldata_IR_RT) 

anova(modelRT3ms,model.nullRT3ms)

#add in group_stATUS and shows TD driving the effect
#Maximal model with no singularity of fit error drops participant and item random effects
modelRT3msGS <- lmer(RT3ms ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = alldata_IR_RT,
                     REML = TRUE) 
summary(modelRT3msGS)

#All the data for this model looks pretty normal.
check_model(modelRT3ms)
qqnorm(residuals(modelRT3ms))
qqline(residuals(modelRT3ms))
descdist(alldata_IR_RT$RT3ms)

#Now Let's add in individual differences
#Import Individual difference measures
alldata_SRS2 <- read_csv("SRS2_data/alldata_SRS2.csv")
alldata_EQ <- read_csv("EQ_data/alldata_EQ.csv")

all_data_join <- inner_join(alldata_IR_RT, alldata_SRS2, by = "participant")
all_data_join <- inner_join(all_data_join, alldata_EQ, by = "participant")

#View(all_data_join)

# Scale the ID measures...
all_data_join$total_t_score <- scale(all_data_join$total_t_score)
all_data_join$EQ_score <- scale(all_data_join$EQ_score)

# Model including covariates
model_alldatacov_RT3ms <- lmer(RT3ms ~ total_t_score + EQ_score + condition_number + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_RT3ms)

#model failed to converge will have to simplofy more



# Let's have a look at region 4 Which is our critical/ Question region

#Violin plots
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_IR_RT %>% 
#  ggplot(aes(x = condition_number, y = RT4ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = FALSE)

#Violin plots by group_status
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)

#Boxplt
#alldata_IR_RT %>% 
#  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = FALSE)

#Descriptives
alldata_IR_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))



# Model assuming normality of residuals maximal structure

#set condition as a factor doesnt make model run
#alldata_IR_RT$condition_number <- as.factor(alldata_IR_RT$condition_number)
#alldata_IR_RT$participant <- as.factor(alldata_IR_RT$participant)
#alldata_IR_RT$item_number <- as.factor(alldata_IR_RT$item_number)
#Changing to numeric doesnt help either!!!!!!1
#alldata_IR_RT <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Indirect_Replies/alldata_IR_RT.csv", 
#                           col_types = cols(RT1 = col_number(), 
#                                           RT2 = col_number(), RT3 = col_number(), 
#                                          RT4 = col_number(), RT5 = col_number(), 
#                                         RT6 = col_number(), Comp_Question_RT = col_number(), 
#                                        item_number = col_number()))


#model.nullR4 <- lmer(RT4ms ~ (1 + condition_number | participant) + (1 + condition_number | item_number), alldata_IR_RT) 
#Maximal model with no singularity of fit error drops item random effects
modelRT4ms <- lmer(RT4ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), data = alldata_IR_RT,
                   REML = TRUE) 
summary(modelRT4ms)

model.nullRT4ms <- lmer(RT4ms ~ (1 + condition_number | participant) + (1 | item_number), alldata_IR_RT) 

anova(modelRT4ms,model.nullRT4ms)


#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing.
#Maximal model with no singularity of fit error drops item random effects
modelRT4msGS <- lmer(RT4ms ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), data = alldata_IR_RT,
                     REML = TRUE) 
summary(modelRT4msGS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop
# It Worked!!!!!
#All the data for this model looks pretty normal.
check_model(modelRT4ms)
#qqnorm(residuals(modelRT4ms))
#qqline(residuals(modelRT4ms))
descdist(alldata_IR_RT$RT4ms)

#Lets add ID's
# Model including covariates
model_alldatacov_RT4ms <- lmer(RT4ms ~ condition_number + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_RT4ms)




# Let's have a look at region 5 Which is our post-critical/ Reply region

#Violin plots
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = condition_number)) + ggtitle("Reaction Time Region 5") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Violin plots by group_status
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reaction Time Region 5") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reaction Time Region 5") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Descriptives
alldata_IR_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))



# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops item random effects
modelRT5ms <- lmer(RT5ms ~ condition_number + (1 | participant) + (1 | item_number), data = alldata_IR_RT,
                   REML = TRUE) 
summary(modelRT5ms)

model.nullRT5ms <- lmer(RT5ms ~ (1 | participant) + (1 | item_number), alldata_IR_RT) 

anova(modelRT5ms,model.nullRT5ms)

#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing mechanisms for ASC and TD.
#Maximal model with no singularity of fit error drops item random effects
modelRT5msGS <- lmer(RT5ms ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = alldata_IR_RT,
                     REML = TRUE) 
summary(modelRT5msGS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop

# It Worked!!!!!

#anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelRT5ms)
qqnorm(residuals(modelRT5ms))
qqline(residuals(modelRT5ms))
descdist(alldata_IR_RT$RT5ms)


## Let's have a look at total reading time across all regions

alldata_IR_RT <- alldata_IR_RT %>% group_by(participant) %>%
  mutate(TT = (RT1ms + RT2ms + RT3ms + RT4ms + RT5ms + RT6ms))

#view(alldata_IR_RT)
#IT WORKED!!!!!
#Violin plots
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = TT, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Violin plots by group_status
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Descriptives
alldata_IR_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(TT), sd(TT))



# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops item random effects
modelTT <- lmer(TT ~ condition_number + (1 | participant) + (1 + condition_number | item_number), data = alldata_IR_RT,
                REML = TRUE) 
summary(modelTT)

model.nullTT <- lmer(TT ~ (1 | participant) + (1 + condition_number | item_number), alldata_IR_RT) 

anova(modelTT,model.nullTT)

#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing mechanisms for ASC and TD.
#Maximal model with no singularity of fit error drops item random effects
modelTTGS <- lmer(TT ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = alldata_IR_RT,
                  REML = TRUE) 
summary(modelTTGS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop

# It Worked!!!!!

#anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelTT)
qqnorm(residuals(modelTT))
qqline(residuals(modelTT))
descdist(alldata_IR_RT$TT)



################Lognormal analysis as Weibull is closest to lognormal and gamma#############################
#With Gamma we can include more random effects including maximal structure with random slopes for particiapnt and item 

#Nothing significant with Gamma (if i did it right not sure if after family = gamma i shouldn't have (link = "log") or (link = "inverse")) 

GammaRT3ms <- glmer(RT3ms ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                    family = Gamma (link = "inverse"), data = alldata_IR_RT)
summary(GammaRT3ms)

GammaRT4ms <- glmer(RT4ms ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                    family = Gamma (link = "inverse"), data = alldata_IR_RT)
summary(GammaRT4ms)

GammaRT5ms <- glmer(RT5ms ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                    family = Gamma (link = "inverse"), data = alldata_IR_RT)
summary(GammaRT5ms)

GammaRTT <- glmer(TT ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                  family = Gamma (link = "inverse"), data = alldata_IR_RT)

summary(GammaRTT)

#Export a CSV of the new data set...
write.csv(alldata_IR_RT,"//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Indirect_Replies\\alldata_IR_RT.csv", row.names = TRUE)

#Import ID's
alldata_IR_RT <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Indirect_Replies/alldata_IR_RT.csv")
alldata_EQ <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/EQ_data/alldata_EQ.csv")
alldata_SRS2 <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/SRS2_data/alldata_SRS2.csv")

#view(alldata_EQ)
#view(alldata_SRS2)

all_data_join <- inner_join(alldata_IR_RT, alldata_EQ, by = "participant")
all_data_join <- inner_join(all_data_join, alldata_SRS2, by = "participant")
#view(all_data_join)
