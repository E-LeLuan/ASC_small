#Indirect Replies Final Analysis
library(readr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggdist)
library(ggthemes)
library(ggpubr)
library(ggstatsplot)

alldata_IR_RT <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT.csv", 
                          col_types = cols(RT1 = col_number(), 
                                           RT2 = col_number(), RT3 = col_number(), 
                                           RT4 = col_number(), RT5 = col_number(), 
                                           RT6 = col_number(), RT1ms = col_number(), 
                                           RT2ms = col_number(), RT3ms = col_number(), 
                                           RT4ms = col_number(), RT5ms = col_number(), 
                                           RT6ms = col_number(), TT = col_number()))
#View(alldata_IR_RT)

#Add Individual Difference Measures
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Reduced_IDs_IR.csv", 
                           col_types = cols(SRS_total_score_raw = col_number(), 
                                            SRS_total_score_t = col_number(), 
                                            EQ = col_number(), Total_RAN = col_number(), 
                                            Total_reading_cluster = col_number()))
#Combined ID's with Reaction Time Data
all_data_join <- inner_join(alldata_IR_RT, Reduced_IDs_IR, by = "participant")
#View(all_data_join)
# Scale the ID measures...
all_data_join$SRS_total_score_raw <- scale(all_data_join$SRS_total_score_raw)
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)

#Lets have a look at the models we have reported (note: Tidy_IR_sript.R in Indirect_Replies folder has more exploration of the data)

#Region 2-> Sentiment Manipulation ROI
# Model including covariates + GS and condition interaction
model_int2 <- lmer(RT2ms ~ condition_number*Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_int2)
# Summary Stats using emmeans package
library(emmeans)
SER21 = emmeans(model_int2, specs = 'condition_number')
summary(SER21)
SER22 = emmeans(model_int2, specs = 'condition_number', 'Group_Status')
summary(SER22)
#visual Aid
alldata_IR_RT %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = Group_Status)) + ggtitle("Reaction Time Region 2") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)


#Region 4-> Critical Reply ROI
#Remove outliers
ggbetweenstats(all_data_join, condition_number, RT4ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT4ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT4ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT4ms > (Q[1] - 2.0*iqr) & all_data_join$RT4ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT4ms, outlier.tagging = TRUE) 
#Model including group status interaction and IDs with ouliers removed as crazy fixations of 29,460 ms
model_int4 <- lmer(RT4ms ~ condition_number*Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_int4)

#compare_means(RT4ms ~ Group_Status, data = all_data_join, 
#              group.by = "condition_number")
#compare_means(RT4ms ~ condition_number, data = all_data_join, 
#              group.by = "Group_Status")
#t.test(RT4ms ~ Group_Status, data = all_data_join, group.by = "condition_number")

## Summary Stats using emmeans package
SER41 = emmeans(model_int4, specs = 'condition_number')
summary(SER41)
SER42 = emmeans(model_int4, specs = 'condition_number', 'Group_Status')
summary(SER42)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)
# Boxplot just Reading information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "RT4ms",
  fill = "condition_number", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Critical Reply Region", y = "Reading time in Milliseconds", x = "Indirect Reply Sentiment")
myplot3
#Raincloud plot
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Critical Reply Region") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Indirect Reply Sentiment") + 
  coord_flip()

#Region 5-> Post-Critical Wrap-Up ROI
#Remove outliers
ggbetweenstats(all_data_join, condition_number, RT5ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT5ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT5ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT5ms > (Q[1] - 2.0*iqr) & all_data_join$RT5ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT5ms, outlier.tagging = TRUE) 
# Model including group status interaction and IDs with ouliers removed as crazy fixations 181,155
model_int5 <- lmer(RT5ms ~  condition_number * Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_int5)
#Summary Stats using emmeans
SE5 = emmeans(model_int5, specs = 'condition_number')
summary(SE5)
SE5 = emmeans(model_int5, specs = 'condition_number', 'Group_Status')
summary(SE5)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reaction Time Region 5") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)
# Boxplot just Reading information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "RT5ms",
  fill = "condition_number", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Post-Critical Wrap-Up Region", y = "Reading Time in Milliseconds", x = "Indirect Reply Sentiment")
myplot3
#Raincloud plot
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Post-Critical Wrap-Up Region") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Indirect Reply Sentiment") + 
  coord_flip()

#Region TT-> Total Time accross all regions
all_data_join <- all_data_join %>% group_by(participant) %>%
  mutate(TT = (RT1ms + RT2ms + RT3ms + RT4ms + RT5ms + RT6ms))
#Remove outliers
ggbetweenstats(all_data_join, condition_number, TT, outlier.tagging = TRUE)
Q <- quantile(all_data_join$TT, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$TT)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$TT > (Q[1] - 2.0*iqr) & all_data_join$TT < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, TT, outlier.tagging = TRUE) 
# Model including group status interaction and IDs with ouliers removed as crazy fixations 1,299,978
model_intTT <- lmer(TT ~ condition_number * Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_intTT)
#Summary Stats using emmeans
SETT = emmeans(model_intTT, specs = 'condition_number')
summary(SETT)
SETT = emmeans(model_intTT, specs = 'condition_number', 'Group_Status')
summary(SETT)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

