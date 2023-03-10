#Remove extra data to put all ID's together and reduce file Size
library(readr)
alldata_EQ <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/EQ_data/alldata_EQ.csv")
View(alldata_EQ)

library(tidyverse)

EQscore <- alldata_EQ[ , c("participant", "EQ_score")]

EQscore <- EQscore %>% 
  distinct(participant, EQ_score, .keep_all = TRUE)

write.csv (EQscore,"//nask.man.ac.uk/home$/Desktop/ASC_small/EQ_data\\EQscore", row.names = TRUE)

view(EQscore)

#This leaves us with 59 lines instead of 3540. Now we can simply transfer this over to our spreadsheet with copy paste. Removes 
# any human error element. 

library(readr)
alldata_EQ <- read_csv("EQ_data/alldata_EQ.csv", 
                       col_types = cols(EQ_score = col_number()))
View(alldata_EQ)

library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
#pairwise comparisons
#pwc <- alldata_EQ %>%
#  pairwise_t_test(EQ_score ~ Group_Status, p.adjust.method = "bonferroni")
#pwc


compare_means(EQ_score ~ Group_Status, data = alldata_EQ, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(alldata_EQ, x = "Group_Status", y = "EQ_score",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
# Change method
p + stat_compare_means(method = "t.test")

t.test(EQ_score ~ Group_Status, data = alldata_EQ)


agg_tbl <- alldata_EQ %>% group_by(Group_Status) %>% 
  summarise(MeanEQ = mean(EQ_score, SD),
            .groups = 'drop')
df2 <- agg_tbl %>% as.data.frame()
view(df2)



