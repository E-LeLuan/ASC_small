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
library(ggrepel)

set.seed(1234)

# Importing the data into R.

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
# P41_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P41_ASC_SMALL.csv")
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
P60_ASC_SMALL <- read_csv("Tidy_RT_data/Indirect_Replies/P50_ASC_SMALL.csv")

# Combining the individual data spreadsheets into one data frame.

alldata <- rbind (P1_ASC_SMALL, P2_ASC_SMALL, P3_ASC_SMALL,P4_ASC_SMALL, P5_ASC_SMALL,
                 P6_ASC_SMALL, P7_ASC_SMALL, P8_ASC_SMALL, P9_ASC_SMALL, P10_ASC_SMALL,
                 P11_ASC_SMALL, P12_ASC_SMALL, P13_ASC_SMALL, P14_ASC_SMALL, P15_ASC_SMALL,
                 P16_ASC_SMALL,P17_ASC_SMALL, P18_ASC_SMALL, P19_ASC_SMALL,P20_ASC_SMALL,
                 P21_ASC_SMALL, P22_ASC_SMALL, P23_ASC_SMALL,P24_ASC_SMALL, P25_ASC_SMALL, P26_ASC_SMALL,
                 P27_ASC_SMALL, P28_ASC_SMALL, P29_ASC_SMALL, P30_ASC_SMALL, P31_ASC_SMALL,P32_ASC_SMALL,
                 P33_ASC_SMALL, P34_ASC_SMALL, P35_ASC_SMALL, P36_ASC_SMALL, P37_ASC_SMALL, P38_ASC_SMALL,
                 P39_ASC_SMALL,P40_ASC_SMALL, P42_ASC_SMALL, P43_ASC_SMALL, P44_ASC_SMALL,
                 P45_ASC_SMALL, P46_ASC_SMALL, P47_ASC_SMALL,P48_ASC_SMALL, P49_ASC_SMALL, P50_ASC_SMALL,
                 P51_ASC_SMALL, P52_ASC_SMALL, P53_ASC_SMALL, P54_ASC_SMALL, P55_ASC_SMALL,P56_ASC_SMALL,
                 P57_ASC_SMALL, P58_ASC_SMALL,P59_ASC_SMALL, P60_ASC_SMALL)

# Make sure we've combined are data correctly. 
view(alldata)  

# alldata <- alldata %>% 
#  add_column(Group_status = NA)

# Create a new column to specify group status ASC vs TD.
alldata <- alldata%>%
  mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
alldata$Group_Status[alldata$Group_Status == 'TRUE'] <- "ASC"
alldata$Group_Status[alldata$Group_Status == 'FALSE'] <- "TD"
  #recode(Group_Status, TRUE = "ASC", FALSE = "TD", default = NA)}


# Assign condition labels, 1 = positive, 2 = negative, 3 = neutral
alldata$condition_number <- recode(alldata$condition_number, "1" = "positive", "2" = "negative", "3" = "neutral")

# Double check it has combined and relabeled correctly.
view(alldata)

#Let's have a look at region 4

# Turn seconds into milliseconds to be comparable with previous studies
alldata <- alldata%>%
  mutate(RT2ms = RT2*1000)
alldata <- alldata%>%
  mutate(RT3ms = RT3*1000)
alldata <- alldata%>%
  mutate(RT4ms = RT4*1000)

# Double check it has combined and relabeled correctly.
view(alldata)


# Descriptives
alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT2ms), sd(RT2ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

view(alldata)


#Visualization

alldata %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = Group_Status)) + ggtitle("Manipulation") +
  labs(y = "Reading time in ms.", x = "Manipulation") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Question") +
  labs(y = "Reading time in ms.", x = "Question") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Indirect Reply") +
  labs(y = "Reading time in ms.", x = "Indirect Reply") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')


# Some serious outliers may need removing at this point --> need to figure out how to do this!

alldata %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Prediciton") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_boxplot()

# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")


# Struggling to make the code work from here on.
# Model the data

#alldata$participant <- as.factor(alldata$participant)
#alldata$condition_number <- as.factor(alldata$condition_number)
#alldata$Group_Status <- as.factor(alldata$Group_Status)
#alldata$RT4ms <- as.factor(alldata$RT4ms)
#alldata$item_number <- as.factor(alldata$item_number)

# Model assuming normality of residuals maximal structure


# Region 2- Manipulation
#model.nullR3 <- lmer(RT3ms ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR2 <- lmer(RT2ms ~ condition_number * Group_Status + (1 | participant) + (1 | item_number), alldata) 
summary(modelR2)
check_model(modelR2)
qqnorm(residuals(modelR2))
qqline(residuals(modelR2))
descdist(alldata$RT2ms)
#ranef(modelR4)


# Need to look at the gamma distribution but not sure how to anylsye that or what package need help with it.

# Seperate analysis based on group
modelR3_TD <- lmer(RT3ms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelR3_TD)                   

modelR3_ASC <- lmer(RT3ms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelR3_ASC)                   

# Region 3- Question

#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR3 <- lmer(RT3ms ~ condition_number * Group_Status + (1 + condition_number | participant) + (1 + condition_number | item_number), alldata) 
summary(modelR3)
check_model(modelR3)
qqnorm(residuals(modelR3))
qqline(residuals(modelR3))
descdist(alldata$RT3ms)
#ranef(modelR4)

# No singular fit 
modelR3 <- lmer(RT3ms ~ condition_number * Group_Status + (1 | participant) + (1 | item_number), alldata) 
summary(modelR3)
check_model(modelR3)
qqnorm(residuals(modelR3))
qqline(residuals(modelR3))
descdist(alldata$RT3ms)



# Seperate analysis based on group
modelR3_TD <- lmer(RT3ms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelR3_TD)                   

modelR4_ASC <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelR4_ASC)      


#Region 4 Indirect Reply

#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR4 <- lmer(RT4ms ~ condition_number * Group_Status + (1 | participant) + (1 | item_number), alldata) 
summary(modelR4)
check_model(modelR4)
qqnorm(residuals(modelR4))
qqline(residuals(modelR4))
descdist(alldata$RT4ms)
#ranef(modelR4)


# Seperate analysis based on group
modelR4_TD <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelR4_TD)                   

modelR4_ASC <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelR4_ASC)  


#Total time
# Create Total time variable 
alldata <- alldata%>%
  mutate(TT = RT1+RT2+RT3+RT4+RT5+RT6) %>% 
  mutate(TTms = TT*1000)

view(alldata)

#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelTT <- lmer(TTms ~ condition_number * Group_Status + (1 | participant) + (1 + condition_number | item_number), alldata) 
summary(modelTT)
check_model(modelTT)
qqnorm(residuals(modelTT))
qqline(residuals(modelTT))
descdist(alldata$TT)
#ranef(modelR4)

# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

# Seperate analysis based on group
modelTT_TD <- lmer(TTms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelTT_TD)                   

modelTT_ASC <- lmer(TTms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelTT_ASC) 
