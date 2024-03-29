#Prediction Final Analysis Small Study

#Load Packages
library(readr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggdist)
library(ggthemes)
library(ggpubr)
library(ggstatsplot)

#Import reading time and individual difference measures
alldata_Pred_RT <- read_csv("Tidy_RT_data/Prediction/alldata_Pred_RT.csv", 
                            col_types = cols(participant = col_number(), 
                                             RT1 = col_number(), RT2 = col_number(), 
                                             RT3 = col_number(), RT4 = col_number(), 
                                             RT5 = col_number(), RT6 = col_number(), 
                                             RT1ms = col_number(), RT2ms = col_number(), 
                                             RT3ms = col_number(), RT4ms = col_number(), 
                                             RT5ms = col_number(), RT6ms = col_number()))
Reduced_IDs_Pred <- read_csv("Tidy_RT_data/Reduced_IDs_Pred.csv", 
                             col_types = cols(participant = col_number(), 
                                              SRS_total_score_raw = col_number(), 
                                              SRS_total_score_t = col_number(), 
                                              EQ = col_number(), Total_RAN = col_number(), 
                                              Total_reading_cluster = col_number()))
#View(Reduced_IDs_Pred)

#Combine the two datasets
all_data_join <- inner_join(alldata_Pred_RT, Reduced_IDs_Pred, by = "participant")
#View(all_data_join)

# Scale the ID measures...
all_data_join$SRS_total_score_raw <- scale(all_data_join$SRS_total_score_raw)
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)


#Region 3 <- Prediction Manipulation Region  
model_int3 <- lmer(RT3ms ~ condition_number * Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_int3)
#Summary stats of the model using emmeans
SER3 = emmeans(model_int3, specs = 'condition_number')
summary(SER3)
SER3 = emmeans(model_int3, specs = 'condition_number', 'Group_Status')
summary(SER3)
#Violin plots by group_status
all_data_join %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)
# Boxplot just Reading information
myplot3 <- ggboxplot(
  eliminated, x = "Group_Status", y = "RT3ms",
  fill = "condition_number", palette = "jco",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Prediction Manipulation Region", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3
# Reading and group information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "RT3ms",
  fill = "Group_Status", palette = "jco",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Prediction Manipulation Region", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3
#remove outliers 
ggbetweenstats(all_data_join, condition_number, RT3ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT3ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT3ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT3ms > (Q[1] - 2.0*iqr) & all_data_join$RT3ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT3ms, outlier.tagging = TRUE) 
#Raincloud plot
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Prediction Manipulation Region") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Prediction") + 
  coord_flip()
# Box plot facetted by "Group_Status"
p <- ggboxplot(eliminated, x = "condition_number", y = "RT3ms",
               color = "Group_Status", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = Group_Status))
p + stat_compare_means(aes(group = Group_Status), label = "p.signif")




#Region 4 <- Critical Question Region
#remove outliers 
ggbetweenstats(all_data_join, condition_number, RT4ms, outlier.tagging = TRUE)
#Q <- quantile(all_data_join$RT4ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
#iqr <- IQR(all_data_join$RT4ms)
#up <-  Q[2]+2.0*iqr # Upper Range  
#low<- Q[1]-2.0*iqr # Lo
#eliminated<- subset(all_data_join, all_data_join$RT4ms > (Q[1] - 2.0*iqr) & all_data_join$RT4ms < (Q[2]+2.0*iqr))
#ggbetweenstats(eliminated, condition_number, RT4ms, outlier.tagging = TRUE)
# Model including interaction
model_int4 <- lmer(RT4ms ~ condition_number * Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 + condition_number | participant) +  (1 | item_number), data = all_data_join, REML = TRUE)
summary(model_int4)
#WRMT-III removed #Same lack of results suggesting WRMT-III influences overall reading not predictive processing
model_int4noWRMT <- lmer(RT4ms ~ condition_number * Group_Status + SRS_total_score_t + EQ + Total_RAN + (1 + condition_number | participant) +  (1 | item_number), data = all_data_join, REML = TRUE)
summary(model_int4noWRMT)
#Summary statistics using emmeans
SER4 = emmeans(model_int4, specs = 'condition_number')
summary(SER4)
SER4 = emmeans(model_int4, specs = 'condition_number', 'Group_Status')
summary(SER4)
all_data_join%>% 
  group_by(condition_number, Group_Status) %>%
  summarise(mean(RT4ms), sd(RT4ms))
library(jtools)
jtools::summ(model_int4)
#remove outliers to make plots clearer 
Q <- quantile(all_data_join$RT4ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT4ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT4ms > (Q[1] - 2.0*iqr) & all_data_join$RT4ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT4ms, outlier.tagging = TRUE)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)
# Boxplot just Reading information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "RT4ms",
  fill = "condition_number", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Critical Question Region", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3
# Reading and group information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "RT4ms",
  fill = "Group_Status", palette = "jco",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Critical Question Region", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3

all_data_join %>%
  group_by(condition_number, Group_Status) %>%
  summarise(mean(RT4ms), sd(RT4ms))


#library(performance)
#check_model(model_int4)
#check_model(model_int4)
#qqnorm(residuals(model_int4))
#qqline(residuals(model_int4))


#Raincloud plot
all_data_join %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Critical Question Region") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Prediction") + 
  coord_flip()
# Box plot 
compare_means(RT4ms ~ Group_Status, data = eliminated,
              group.by = "condition_number")
#Difference between conditions boxplot
p <- ggboxplot(eliminated, x = "condition_number", y = "RT4ms",
               color = "Group_Status", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = condition_number))
p + stat_compare_means(aes(group = condition_number), label = "p.signif")
# Create subset data lists TD
TD_Group <- filter(all_data_join, Group_Status == "TD")
#TD only model
model_TD <- lmer(RT4ms ~ condition_number + Total_reading_cluster + (1 | participant) +  (1 | item_number), data = TD_Group, REML = TRUE)
summary(model_TD)
#TD_Group <- filter(eliminated, Group_Status == "TD")
# Boxplot just Reading information
#myplotTD <- ggboxplot(
#  eliminated, x = "condition_number", y = "RT4ms",
#  fill = "condition_number", palette = "jco", legend = "none",
#  ggtheme = theme_pubr(border = TRUE)) + 
#  labs(title = "Critical Reply Region", y = "Reading time in Milliseconds", x = "Prediciton")
#myplotTD
# Box plot 
compare_means(RT4ms ~ condition_number, data = TD_Group)
#Difference between conditions boxplot
#p <- ggboxplot(TD_Group, x = "condition_number", y = "RT4ms",
#               color = "condition_number", palette = "jco",
#               add = "jitter")
#p + stat_compare_means(aes(group = condition_number))
#p + stat_compare_means(aes(group = condition_number), label = "p.signif")

# Create subset data lists ASC
ASC_Group <- filter(all_data_join, Group_Status == "ASC")
model_ASC <- lmer(RT4ms ~ condition_number + Total_reading_cluster + (1 | participant) +  (1 | item_number), data = ASC_Group, REML = TRUE)
summary(model_ASC)
ASC_Group <- filter(eliminated, Group_Status == "ASC")
compare_means(RT4ms ~ condition_number, data = ASC_Group)
#myplotASC <- ggboxplot(
#  eliminated, x = "condition_number", y = "RT4ms",
#  fill = "condition_number", palette = "jco", legend = "none",
#  ggtheme = theme_pubr(border = TRUE)) + 
#  labs(title = "Critical Reply Region", y = "Reading time in Milliseconds", x = "Prediciton")
#myplotASC





 # Region 5 <- Post-Critical Reply
model_int5 <- lmer(RT5ms ~ condition_number * Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number), data = all_data_join, REML = TRUE)
summary(model_int5)
#Summary statistic for the model R5
SER5 = emmeans(model_int5, specs = 'condition_number')
summary(SER5)
SER5 = emmeans(model_int5, specs = 'condition_number', 'Group_Status')
summary(SER5)
#remove outliers to make plots clearer 
Q <- quantile(all_data_join$RT5ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT5ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT5ms > (Q[1] - 2.0*iqr) & all_data_join$RT5ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT5ms, outlier.tagging = TRUE)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reaction Time Region R5") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)
# Boxplot just Reading information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "RT5ms",
  fill = "condition_number", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Post-Critical Reply Region", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3
# Reading and group information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "RT5ms",
  fill = "Group_Status", palette = "jco",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Post-Critical Reply Region", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3
#Raincloud plot
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Post-Critical Reply Region") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Prediction") + 
  coord_flip()
# Create subset data lists TD
TD_Group <- filter(all_data_join, Group_Status == "TD")
#TD only model
model_TD <- lmer(RT5ms ~ condition_number + (1 | participant) +  (1 | item_number), data = TD_Group, REML = TRUE)
summary(model_TD)
# Create subset data lists ASC
ASC_Group <- filter(all_data_join, Group_Status == "ASC")
#TD only model
model_ASC <- lmer(RT5ms ~ condition_number + (1 | participant) +  (1 | item_number), data = ASC_Group, REML = TRUE)
summary(model_ASC)





#Total Time measure of interest
all_data_join <- all_data_join %>% group_by(participant) %>%
  mutate(TT = (RT1ms + RT2ms + RT3ms + RT4ms + RT5ms + RT6ms))

# Model including covariates + GS
model_intTT <- lmer(TT ~ condition_number*Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number), data = all_data_join, REML = TRUE)
summary(model_intTT)
#Summary stats using emmeans
SETT = emmeans(model_intTT, specs = 'condition_number')
summary(SETT)
SETT = emmeans(model_intTT, specs = 'condition_number', 'Group_Status')
summary(SETT)
#Violin plots by group_status
all_data_join %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Total Time") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)




#remove outliers to make plots clearer 
Q <- quantile(all_data_join$TT, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$TT)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$TT > (Q[1] - 2.0*iqr) & all_data_join$TT < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, TT, outlier.tagging = TRUE)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Total Time Measure") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)
# Boxplot just Reading information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "TT",
  fill = "condition_number", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Total Time Measure", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3
# Reading and group information
myplot3 <- ggboxplot(
  eliminated, x = "condition_number", y = "TT",
  fill = "Group_Status", palette = "jco",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Total Time Measure", y = "Reading time in Milliseconds", x = "Prediciton")
myplot3
#Raincloud plot
eliminated %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Total Time Measure") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Prediction") + 
  coord_flip()
# Create subset data lists TD
TD_Group <- filter(all_data_join, Group_Status == "TD")
#TD only model
model_TD <- lmer(TT ~ condition_number + (1 | participant) +  (1 | item_number), data = TD_Group, REML = TRUE)
summary(model_TD)
# Create subset data lists ASC
ASC_Group <- filter(all_data_join, Group_Status == "ASC")
#TD only model
model_ASC <- lmer(TT ~ condition_number + (1 | participant) +  (1 | item_number), data = ASC_Group, REML = TRUE)
summary(model_ASC)
