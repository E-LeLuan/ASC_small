#ID reduction down to only used variables no exploratory

library(Matrix)
library(tidyverse)
library(readr)

library(readr)
All_IDs <- read_csv("Tidy_RT_data/All_IDs.csv")
View(All_IDs)


Reduced_IDs_IR <- All_IDs[ , c("participant", "Age", "Gender", "Comprehension_accuracy_IR", "SRS_total_score_raw", "SRS_total_score_t", 
                               "Total_SRS_Category", "EQ", "Total_RAN", "Total_reading_cluster")]

view(Reduced_IDs_IR)


#write.csv (Reduced_IDs_IR,"//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data\\Reduced_IDs_IR", row.names = TRUE)

# Lets do some t.tests
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Reduced_IDs_IR.csv")
View(Reduced_IDs_IR)

#Add in group status
Reduced_IDs_IR <- Reduced_IDs_IR%>%
  mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
Reduced_IDs_IR$Group_Status[Reduced_IDs_IR$Group_Status == 'TRUE'] <- "ASC"
Reduced_IDs_IR$Group_Status[Reduced_IDs_IR$Group_Status == 'FALSE'] <- "TD"
view(Reduced_IDs_IR)

#Remove row 61 as missing data if you wanted to remove a columnjust remove the , so [-61]
Reduced_IDs_IR <- Reduced_IDs_IR[-61,]
Reduced_IDs_IR <- Reduced_IDs_IR[-41,]
#view(Reduced_IDs_IR)

library(ggpubr)
#EQ
compare_means(EQ ~ Group_Status, data = Reduced_IDs_IR, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_IR, x = "Group_Status", y = "EQ",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on EQ", x = "Group", y = "EQ")

t.test(EQ ~ Group_Status, data = Reduced_IDs_IR)

Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(EQ),
            sdASC = sd(EQ))

#Age
t.test(Age ~ Group_Status, data = Reduced_IDs_IR)
Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Age),
            sdASC = sd(Age))

#Gender
t.test(Gender ~ Group_Status, data = Reduced_IDs_IR)
#total number 34 female 25 male
sum(Reduced_IDs_IR$Gender == 'Female')
sum(Reduced_IDs_IR$Gender == 'Male')

Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  count(Reduced_IDs_IR$Gender == 'Female')

#What kind of analysis should I do on gender????

#Comprehension Accuracy
t.test(Comprehension_accuracy_IR ~ Group_Status, data = Reduced_IDs_IR)
Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Comprehension_accuracy_IR),
            sdASC = sd(Comprehension_accuracy_IR))

#WRMT-III
t.test(Total_reading_cluster ~ Group_Status, data = Reduced_IDs_IR)
Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Total_reading_cluster),
            sdASC = sd(Total_reading_cluster))

compare_means(Total_reading_cluster ~ Group_Status, data = Reduced_IDs_IR, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_IR, x = "Group_Status", y = "Total_reading_cluster",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on WRMT-III", x = "Group", y = "Total Reading Score")


#SRS-2 total t score
t.test(SRS_total_score_t ~ Group_Status, data = Reduced_IDs_IR)
Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(SRS_total_score_t),
            sdASC = sd(SRS_total_score_t))

compare_means(SRS_total_score_t ~ Group_Status, data = Reduced_IDs_IR, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_IR, x = "Group_Status", y = "SRS_total_score_t",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on SRS-2", x = "Group", y = "Total t Score")


#RAN
t.test(Total_RAN ~ Group_Status, data = Reduced_IDs_IR)
Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(Total_RAN),
            sdASC = sd(Total_RAN))
compare_means(Total_RAN ~ Group_Status, data = Reduced_IDs_IR, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_IR, x = "Group_Status", y = "Total_RAN",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on RAN", x = "Group", y = "RAN score in ms.")
