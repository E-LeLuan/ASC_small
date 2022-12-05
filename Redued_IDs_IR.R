#ID reduction down to only used variables no exploratory

library(Matrix)
library(tidyverse)
library(readr)

all_IDs <- read_csv("All_IDs.csv")
view(all_IDs)


Reduced_IDs_IR <- all_IDs[ , c("participant", "Age", "Gender", "Comprehension_accuracy_IR", "SRS_total_score_raw", "SRS_total_score_t", 
                               "Total_SRS_Category", "EQ", "Total_RAN", "Total_reading_cluster")]

Reduced_IDs_IR <- Reduced_IDs_IR %>%
  distinct(participant, Age, Gender, Comprehension_accuracy_IR, SRS_total_score_raw, SRS_total_score_t, Total_SRS_Category, EQ, Total_RAN, 
           Total_reading_cluster, .keep_all = TRUE)

view(Reduced_IDs_IR)


write.csv (Reduced_IDs_IR,"//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data\\Reduced_IDs_IR", row.names = TRUE)
