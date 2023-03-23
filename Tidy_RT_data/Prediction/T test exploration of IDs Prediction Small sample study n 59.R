#T test exploration of IDs Prediction Small sample study n = 59
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

Reduced_IDs_Pred <- read_csv("Tidy_RT_data/Reduced_IDs_Pred.csv", 
                             col_types = cols(participant = col_number(), 
                                              SRS_total_score_raw = col_number(), 
                                              SRS_total_score_t = col_number(), 
                                              EQ = col_number(), Total_RAN = col_number(), 
                                              Total_reading_cluster = col_number()))
#View(Reduced_IDs_Pred)

Reduced_IDs_Pred <- Reduced_IDs_Pred%>%
  mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
Reduced_IDs_Pred$Group_Status[Reduced_IDs_Pred$Group_Status == 'TRUE'] <- "ASC"
Reduced_IDs_Pred$Group_Status[Reduced_IDs_Pred$Group_Status == 'FALSE'] <- "TD"
#view(Reduced_IDs_Pred)

#Remove row 61 as missing data if you wanted to remove a columnjust remove the , so [-61]
Reduced_IDs_Pred <- Reduced_IDs_Pred[-61,]
Reduced_IDs_Pred <- Reduced_IDs_Pred[-41,]

# Scale the ID measures...
#all_data_join$SRS_total_score_raw <- scale(all_data_join$SRS_total_score_raw)
#all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
#all_data_join$EQ <- scale(all_data_join$EQ)
#all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
#all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)

#T.tests, mean, and sd





#EQ
compare_means(EQ ~ Group_Status, data = Reduced_IDs_Pred, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_Pred, x = "Group_Status", y = "EQ",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on EQ", x = "Group", y = "EQ")

t.test(EQ ~ Group_Status, data = Reduced_IDs_Pred)
#wilcox.test(EQ ~ Group_Status, data = Reduced_IDs_Pred)

#EQ
Reduced_IDs_Pred %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(EQ),
            sdASC = sd(EQ))

alldata_Pred_RT_comp <- read_csv("Tidy_RT_data/Prediction/alldata_Pred_RT_comp.csv")
#Comp Accuracy

t.test(total_perc ~ Group_Status, data = alldata_Pred_RT_comp)
alldata_Pred_RT_comp %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(total_perc),
            sdASC = sd(total_perc))







#SRS-2
compare_means(SRS_total_score_t ~ Group_Status, data = Reduced_IDs_Pred, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_Pred, x = "Group_Status", y = "SRS_total_score_t",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test") %>% 
  labs(title = "Overall score on SRS-2", x = "Group", y = "Total t Score")


(t.test(SRS_total_score_t ~ Group_Status, data = Reduced_IDs_Pred))
#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")

Reduced_IDs_Pred %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(SRS_total_score_t),
            sdASC = sd(SRS_total_score_t))

#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")







#Total RAN
compare_means(Total_RAN ~ Group_Status, data = Reduced_IDs_Pred, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_Pred, x = "Group_Status", y = "Total_RAN",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test") %>% 
  labs(title = "Overall score on RAN", x = "Group", y = "Total RAN")


(t.test(Total_RAN ~ Group_Status, data = Reduced_IDs_Pred))
#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")

Reduced_IDs_Pred %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Total_RAN),
            sdASC = sd(Total_RAN))

#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")
#SRS-2
compare_means(Total_RAN ~ Group_Status, data = Reduced_IDs_Pred, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_Pred, x = "Group_Status", y = "Total_RAN",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test") %>% 
  labs(title = "Overall score on RAN", x = "Group", y = "Total_RAN")


(t.test(Total_RAN ~ Group_Status, data = Reduced_IDs_Pred))
#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")

Reduced_IDs_Pred %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Total_RAN),
            sdASC = sd(Total_RAN))

#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")





#WRMT-III
compare_means(Total_reading_cluster ~ Group_Status, data = Reduced_IDs_Pred, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_Pred, x = "Group_Status", y = "Total_reading_cluster",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on WRMT-III", x = "Group", y = "Total_reading_cluster")

t.test(Total_reading_cluster ~ Group_Status, data = Reduced_IDs_Pred)
#wilcox.test(Total_reading_cluster ~ Group_Status, data = Reduced_IDs_Pred)

#Total_reading_cluster
Reduced_IDs_Pred %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Total_reading_cluster),
            sdASC = sd(Total_reading_cluster))



#Age
t.test(Age ~ Group_Status, data = Reduced_IDs_Pred)
Reduced_IDs_Pred %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Age),
            sdASC = sd(Age))



#Gender
#Gender
#total number 34 female 25 male
sum(Reduced_IDs_Pred$Gender == 'Female')
sum(Reduced_IDs_Pred$Gender == 'Male')

Reduced_IDs_Pred %>% 
  group_by(Group_Status) %>%
  count(Reduced_IDs_Pred$Gender == 'Female')



#Comprehension Accuracy
alldata_Pred_RT_comp <- read_csv("Tidy_RT_data/Prediction/alldata_Pred_RT_comp.csv")
#Comp Accuracy

t.test(total_perc ~ Group_Status, data = alldata_Pred_RT_comp)
alldata_Pred_RT_comp %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(total_perc),
            sdASC = sd(total_perc))



