#SRS2 auto tidy script (attempt)


library(tidyverse)

set.seed(1234)

# Importing the data into R.

library(readr)
P1_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P1_ASC_SMALL_SRS2.csv")
P2_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P2_ASC_SMALL_SRS2.csv")
P3_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P3_ASC_SMALL_SRS2.csv")
P4_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P4_ASC_SMALL_SRS2.csv")
P5_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P5_ASC_SMALL_SRS2.csv")
P6_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P6_ASC_SMALL_SRS2.csv")
P7_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P7_ASC_SMALL_SRS2.csv")
P8_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P8_ASC_SMALL_SRS2.csv")
P9_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P9_ASC_SMALL_SRS2.csv")
P10_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P10_ASC_SMALL_SRS2.csv")
P11_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P11_ASC_SMALL_SRS2.csv")
P12_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P12_ASC_SMALL_SRS2.csv")
P13_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P13_ASC_SMALL_SRS2.csv")
P14_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P14_ASC_SMALL_SRS2.csv")
P15_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P15_ASC_SMALL_SRS2.csv")
P16_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P16_ASC_SMALL_SRS2.csv")
P17_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P17_ASC_SMALL_SRS2.csv")
P18_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P18_ASC_SMALL_SRS2.csv")
P19_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P19_ASC_SMALL_SRS2.csv")
P20_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P20_ASC_SMALL_SRS2.csv")
P21_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P21_ASC_SMALL_SRS2.csv")
P22_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P22_ASC_SMALL_SRS2.csv")
P23_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P23_ASC_SMALL_SRS2.csv")
P24_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P24_ASC_SMALL_SRS2.csv")
P25_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P25_ASC_SMALL_SRS2.csv")
P26_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P26_ASC_SMALL_SRS2.csv")
P27_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P27_ASC_SMALL_SRS2.csv")
P28_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P28_ASC_SMALL_SRS2.csv")
P29_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P29_ASC_SMALL_SRS2.csv")
P30_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P30_ASC_SMALL_SRS2.csv")
P31_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P31_ASC_SMALL_SRS2.csv")
P32_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P32_ASC_SMALL_SRS2.csv")
P33_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P33_ASC_SMALL_SRS2.csv")
P34_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P34_ASC_SMALL_SRS2.csv")
P35_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P35_ASC_SMALL_SRS2.csv")
P36_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P36_ASC_SMALL_SRS2.csv")
P37_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P37_ASC_SMALL_SRS2.csv")
P38_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P38_ASC_SMALL_SRS2.csv")
P39_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P39_ASC_SMALL_SRS2.csv")
P40_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P40_ASC_SMALL_SRS2.csv")
#P41_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P41_ASC_SMALL_SRS2.csv")
P42_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P42_ASC_SMALL_SRS2.csv")
P43_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P43_ASC_SMALL_SRS2.csv")
P44_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P44_ASC_SMALL_SRS2.csv")
P45_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P45_ASC_SMALL_SRS2.csv")
P46_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P46_ASC_SMALL_SRS2.csv")
P47_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P47_ASC_SMALL_SRS2.csv")
P48_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P48_ASC_SMALL_SRS2.csv")
P49_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P49_ASC_SMALL_SRS2.csv")
P50_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P50_ASC_SMALL_SRS2.csv")
P51_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P51_ASC_SMALL_SRS2.csv")
P52_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P52_ASC_SMALL_SRS2.csv")
P53_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P53_ASC_SMALL_SRS2.csv")
P54_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P54_ASC_SMALL_SRS2.csv")
P55_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P55_ASC_SMALL_SRS2.csv")
P56_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P56_ASC_SMALL_SRS2.csv")
P57_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P57_ASC_SMALL_SRS2.csv")
P58_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P58_ASC_SMALL_SRS2.csv")
P59_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P59_ASC_SMALL_SRS2.csv")
P60_ASC_SMALL_SRS2 <- read_csv("SRS2_data/P60_ASC_SMALL_SRS2.csv")


# Rename the variables form Participants 1-3 that don't match with participants 4-60. I Gave
# up on this section and changed the 3 files manually in excel.
## SRS_2_item -> SRS2_item
## scoring_type -> SRS2__scoring_type
## treatment_subscale -> SRS2_treatment_subscale
#P1_ASC_SMALL_SRS2 %>%
#  rename(SRS2_item = SRS_2_item, 
#         SRS2_scoring_type = scoring_type,
#         SRS2_treatment_subscale = treatment_subscale) 

#P2_ASC_SMALL_SRS2 %>%
 # rename(SRS2_item = SRS_2_item, 
#         SRS2_scoring_type = scoring_type,
#         SRS2_treatment_subscale = treatment_subscale) 

#P3_ASC_SMALL_SRS2 %>%
#  rename(SRS2_item = SRS_2_item, 
#         SRS2_scoring_type = scoring_type,
#         SRS2_treatment_subscale = treatment_subscale) 

# *******Andrews Solution in that code chunk, you're not mapping the result back onto a data frame******

#new_df <- P1_ASC_SMALL_SRS2 %>%
 # rename(SRS2_item = SRS_2_item,
  #       SRS2_scoring_type = scoring_type,
   #      SRS2_treatment_subscale = treatment_subscale)

# Combining the individual data spreadsheets into one data frame.

alldata <- rbind (P1_ASC_SMALL_SRS2, P2_ASC_SMALL_SRS2, P3_ASC_SMALL_SRS2,P4_ASC_SMALL_SRS2, P5_ASC_SMALL_SRS2,
                  P6_ASC_SMALL_SRS2, P7_ASC_SMALL_SRS2, P8_ASC_SMALL_SRS2, P9_ASC_SMALL_SRS2, P10_ASC_SMALL_SRS2,
                  P11_ASC_SMALL_SRS2, P12_ASC_SMALL_SRS2, P13_ASC_SMALL_SRS2, P14_ASC_SMALL_SRS2, P15_ASC_SMALL_SRS2,
                  P16_ASC_SMALL_SRS2,P17_ASC_SMALL_SRS2, P18_ASC_SMALL_SRS2, P19_ASC_SMALL_SRS2,P20_ASC_SMALL_SRS2,
                  P21_ASC_SMALL_SRS2, P22_ASC_SMALL_SRS2, P23_ASC_SMALL_SRS2,P24_ASC_SMALL_SRS2, P25_ASC_SMALL_SRS2, P26_ASC_SMALL_SRS2,
                  P27_ASC_SMALL_SRS2, P28_ASC_SMALL_SRS2, P29_ASC_SMALL_SRS2, P30_ASC_SMALL_SRS2, P31_ASC_SMALL_SRS2,P32_ASC_SMALL_SRS2,
                  P33_ASC_SMALL_SRS2, P34_ASC_SMALL_SRS2, P35_ASC_SMALL_SRS2, P36_ASC_SMALL_SRS2, P37_ASC_SMALL_SRS2, P38_ASC_SMALL_SRS2,
                  P39_ASC_SMALL_SRS2,P40_ASC_SMALL_SRS2,P42_ASC_SMALL_SRS2, P43_ASC_SMALL_SRS2, P44_ASC_SMALL_SRS2,
                  P45_ASC_SMALL_SRS2, P46_ASC_SMALL_SRS2, P47_ASC_SMALL_SRS2,P48_ASC_SMALL_SRS2, P49_ASC_SMALL_SRS2, P50_ASC_SMALL_SRS2,
                  P51_ASC_SMALL_SRS2, P52_ASC_SMALL_SRS2, P53_ASC_SMALL_SRS2, P54_ASC_SMALL_SRS2, P55_ASC_SMALL_SRS2,P56_ASC_SMALL_SRS2,
                  P57_ASC_SMALL_SRS2, P58_ASC_SMALL_SRS2,P59_ASC_SMALL_SRS2, P60_ASC_SMALL_SRS2)
                

# check it yup all good
#view(alldata)

alldata <- alldata%>%
mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
alldata$Group_Status[alldata$Group_Status == 'TRUE'] <- "ASC"
alldata$Group_Status[alldata$Group_Status == 'FALSE'] <- "TD"

# check it yup all good
#view(alldata)

# SRS2 scoring ignore none as these are filler items we are not interested in. 

#We want to add a column called SRS2_tidy that converts the values to the appropriate score. 
# What the code should do is take items that are positively scored and turn a 1 into a 2, 
# turn a 2 into a 1, and turn 3 and 4 into a 0. 
# What the code should do is take items that are negatively scored and turn a 4 into a 2, 
# turn a 3 into a 1, and turn 1 and 2 into a 0. 

alldata <- alldata %>% mutate(SRS2_tidy = case_when 
                              (SRS2_scoring_type == 'positive' &  SRS2_resp == 1 ~ 0, 
                                SRS2_scoring_type == 'positive' &  SRS2_resp == 2 ~ 1,
                                SRS2_scoring_type == 'positive' &  SRS2_resp == 3 ~ 2,
                                SRS2_scoring_type == 'positive' &  SRS2_resp == 4 ~ 3,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 1 ~ 3,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 2 ~ 2,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 3 ~ 1,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 4 ~ 0,))
view(alldata)

# It worked!!!!!

# Then we can add this column for each participant to get their individual SRS2 score
# THIS IS NOT RIGHT! SRS-2 has way more nuances then the EQ so cannot use this sum code.
SRS2_score <- alldata %>%
  group_by(participant) %>%
  summarise_at(vars(SRS2_tidy), list(SRS2_score = sum))

alldata <- inner_join(alldata, SRS2_score, by = "participant")

view(alldata)
