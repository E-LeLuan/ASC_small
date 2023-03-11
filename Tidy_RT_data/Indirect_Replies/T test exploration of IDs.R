library(readr)
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Indirect_Replies/Reduced_IDs_IR.csv")
#View(Reduced_IDs_IR)
alldata_IR_RT_comp <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT_comp.csv")
View(alldata_IR_RT_comp)

alldata_IR_RT_comp <- alldata_IR_RT_comp %>%
  distinct(participant, .keep_all = TRUE)
