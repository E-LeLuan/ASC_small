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

set.seed(42)

# Importing the data into R.

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
# P41_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P41_ASC_SMALL.csv")
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
P60_ASC_SMALL <- read_csv("Tidy_RT_data/Prediction/P50_ASC_SMALL.csv")

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


# Assign condition labels, 1 = facilitated, 2 = unfacilitated
# alldata$condition_number <- recode(alldata$condition_number, "1" = "Facilitated", "2" = "unfacilitated")

view(alldata)

#Let's have a look at region 4

# Turn seconds into milliseconds to be comparable with previous studies
alldata <- alldata%>%
  mutate(RT3ms = RT3*1000)
alldata <- alldata%>%
  mutate(RT4ms = RT4*1000)
alldata <- alldata%>%
  mutate(RT5ms = RT5*1000)

# Double check it has combined and relabeled correctly.
view(alldata)

# RT3 & RT4ms descriptive
alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

#Plots
alldata %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Prediciton") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Question") +
  labs(y = "Reading time in ms.", x = "Question") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reply") +
  labs(y = "Reading time in ms.", x = "Reply") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

# Model assuming normality of residuals maximal structure


# Region 3- Prediction
# With interaction
model.nullR3 <- lmer(RT3ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR3 <- lmer(RT3ms ~ condition_number*Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR3)

anova(model.nullR3, modelR3)

#remove group status
model.nullR3b <- lmer(RT3ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR3b <- lmer(RT3ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR3b)

anova(model.nullR3b, modelR3b)

# No singualr fit
model.nullR3c <- lmer(RT3ms ~ (1 | participant) + (1 | item_number), alldata) 
modelR3c <- lmer(RT3ms ~ condition_number*Group_Status + (1 | participant) + (1 | item_number), alldata) 

summary(modelR3c)

anova(model.nullR3c, modelR3c)

check_model(modelR3)
qqnorm(residuals(modelR3))
qqline(residuals(modelR3))
descdist(alldata$RT3ms)


# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

#Subset descriptives
ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

# Separate models for groups

#ASC
model.nullR3ASC <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), ASC_Group) 
modelR3ASC <- lmer(RT4ms ~ condition_number + (1 + condition_number| participant) + (1 | item_number), ASC_Group) 

summary(modelR3ASC)

anova(model.nullR3ASC, modelR3ASC)

#TD

model.nullR3TD <- lmer(RT3ms ~ (1 + condition_number | participant) + (1 | item_number), TD_Group) 
modelR3TD <- lmer(RT3ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), TD_Group) 

summary(modelR4TD)

anova(model.nullR3TD, modelR3TD)




# Region 4- Question
# With interaction
model.nullR4 <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR4 <- lmer(RT4ms ~ condition_number*Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR4)

anova(model.nullR4, modelR4)

#remove group status
model.nullR4b <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR4b <- lmer(RT4ms ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR4b)

anova(model.nullR4b, modelR42)

check_model(modelR4)
qqnorm(residuals(modelR4))
qqline(residuals(modelR4))
descdist(alldata$RT4ms)

# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

#Subset descriptives
ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

# Separate models for groups

#ASC
model.nullR4ASC <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), ASC_Group) 
modelR4ASC <- lmer(RT4ms ~ condition_number + (1 + condition_number| participant) + (1 | item_number), ASC_Group) 

summary(modelR4ASC)

anova(model.nullR4ASC, modelR4ASC)

#TD

model.nullR4TD <- lmer(RT4ms ~ (1 | participant) + (1 | item_number), TD_Group) 
modelR4TD <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 

summary(modelR4TD)

anova(model.nullR4TD, modelR4TD)


# Region 5- Reply
# With interaction ignore singular fit
model.nullR5 <- lmer(RT5ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR5 <- lmer(RT3ms ~ condition_number*Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR5)

anova(model.nullR5, modelR5)

#remove group status and singular fit
model.nullR5b <- lmer(RT5ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR5b <- lmer(RT5ms ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR5b)

anova(model.nullR5b, modelR5b)

check_model(modelR5)
qqnorm(residuals(modelR5))
qqline(residuals(modelR5))
descdist(alldata$RT5ms)


# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

#Subset descriptives
ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

# Separate models for groups

#ASC
model.nullR5ASC <- lmer(RT5ms ~ (1 + condition_number| participant) + (1 | item_number), ASC_Group) 
modelR5ASC <- lmer(RT5ms ~ condition_number + (1 + condition_number| participant) + (1 | item_number), ASC_Group) 

summary(modelR5ASC)

anova(model.nullR5ASC, modelR5ASC)

#TD

model.nullR5TD <- lmer(RT5ms ~ (1 + condition_number | participant) + (1 | item_number), TD_Group) 
modelR5TD <- lmer(RT5ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), TD_Group) 

summary(modelR5TD)

anova(model.nullR5TD, modelR5TD)

# Create Total time variable 

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
