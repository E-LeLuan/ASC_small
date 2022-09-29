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
#view(alldata)

# It worked!!!!!

# Then we can add this column for each participant to get their individual RAW SRS2 score by treatment 
#subScale and total score and DSM-5 compatible subscales

# Total score first
alldata <- alldata %>% group_by(participant) %>%
  mutate(total_RAW_score = sum(SRS2_tidy))

# Now individual treatment subscale raw scores
                                
alldata <- alldata %>% group_by(participant, SRS2_treatment_subscale) %>% 
  mutate(treatment_RAW_score = sum(SRS2_tidy))

#Now DSM-5 compatible subscale scores
#Sort into your subscales of RRB and SCI DSM5 subscales
alldata <- alldata%>%
  mutate(DSM5_group = SRS2_treatment_subscale == 'rrb')
# Rename TRUE FALSE to more meaningful labels.
alldata$DSM5_group [alldata$DSM5_group == 'TRUE'] <- "RRB"
alldata$DSM5_group [alldata$DSM5_group == 'FALSE'] <- "SCI"
#Now get the RAW DSM5 scores
alldata <- alldata %>% group_by(participant, DSM5_group) %>% 
  mutate(DSM5_RAW_score = sum(SRS2_tidy))

#view(alldata)

# It worked!!!!!





# ************************************HELP FROM HERE DOWN PLEASE******************************************




# Now let's try the total t- scores. We need to create a variable 'total_t_scores' changing the total_raw_score of
# participants to a specified value based on a range as seen below.

# Let'd find the minimum vale so we don't do any extra work
min(alldata$total_RAW_score)
max(alldata$total_RAW_score)
#output min is 18 and the max is 149 so we don't need to input values below for anything less

#Why has it only done this for one participants data???????? grrrrrr
#alldata['total_t_score'][alldata['total_t_score']> 17 & alldata['total_t_score'] <19] <- 42


# Also Doesn't work!!! 
#alldata <- alldata %>% mutate(total_t_score = case_when(total_RAW_score > 17 & alldata$total_raw_score < 19 ~ '42'))

#It only works for the first participant!! Grrrrrr
alldata <- alldata %>%
  mutate(total_t_score = case_when(total_RAW_score >= 17 & total_RAW_score <= 19 ~ '42',
                                   total_RAW_score >= 20 & total_RAW_score <= 21 ~ '43',
                                   total_RAW_score >= 25 & total_RAW_score <= 27 ~ '45',
                                   total_RAW_score >= 28 & total_RAW_score <= 30 ~ '46',
                                   total_RAW_score >= 31 & total_RAW_score <= 33 ~ '47',
                                   total_RAW_score >= 34 & total_RAW_score <= 36 ~ '48',
                                   total_RAW_score >= 37 & total_RAW_score <= 38 ~ '49',
                                   total_RAW_score >= 39 & total_RAW_score <= 41 ~ '50',
                                   total_RAW_score >= 42 & total_RAW_score <= 44 ~ '51',
                                   total_RAW_score >= 45 & total_RAW_score <= 47 ~ '52',
                                   total_RAW_score >= 48 & total_RAW_score <= 50 ~ '53',
                                   total_RAW_score >= 51 & total_RAW_score <= 53 ~ '54',
                                   total_RAW_score >= 54 & total_RAW_score <= 56 ~ '55',
                                   total_RAW_score >= 57 & total_RAW_score <= 58 ~ '56',
                                   total_RAW_score >= 59 & total_RAW_score <= 61 ~ '57',
                                   total_RAW_score >= 62 & total_RAW_score <= 64 ~ '58',
                                   total_RAW_score >= 65 & total_RAW_score <= 67 ~ '59',
                                   total_RAW_score >= 68 & total_RAW_score <= 70 ~ '60',
                                   total_RAW_score >= 71 & total_RAW_score <= 73 ~ '61',
                                   total_RAW_score >= 74 & total_RAW_score <= 75 ~ '62',
                                   total_RAW_score >= 76 & total_RAW_score <= 78 ~ '63',
                                   total_RAW_score >= 79 & total_RAW_score <= 81 ~ '64',
                                   total_RAW_score >= 82 & total_RAW_score <= 84 ~ '65',
                                   total_RAW_score >= 85 & total_RAW_score <= 87 ~ '66',
                                   total_RAW_score >= 88 & total_RAW_score <= 90 ~ '67',
                                   total_RAW_score >= 91 & total_RAW_score <= 92 ~ '68',
                                   total_RAW_score >= 93 & total_RAW_score <= 95 ~ '69',
                                   total_RAW_score >= 96 & total_RAW_score <= 98 ~ '70',
                                   total_RAW_score >= 99 & total_RAW_score <= 101 ~ '71',
                                   total_RAW_score >= 102 & total_RAW_score <= 104 ~ '72',
                                   total_RAW_score >= 105 & total_RAW_score <= 107 ~ '73',
                                   total_RAW_score >= 108 & total_RAW_score <= 109 ~ '74',
                                   total_RAW_score >= 110 & total_RAW_score <= 112 ~ '75',
                                   total_RAW_score >= 113 & total_RAW_score <= 115 ~ '76',
                                   total_RAW_score >= 116 & total_RAW_score <= 118 ~ '77',
                                   total_RAW_score >= 119 & total_RAW_score <= 121 ~ '78',
                                   total_RAW_score >= 122 & total_RAW_score <= 124 ~ '79',
                                   total_RAW_score >= 125 & total_RAW_score <= 127 ~ '80',
                                   total_RAW_score >= 128 & total_RAW_score <= 129 ~ '81',
                                   total_RAW_score >= 130 & total_RAW_score <= 132 ~ '82',
                                   total_RAW_score >= 133 & total_RAW_score <= 135 ~ '83',
                                   total_RAW_score >= 136 & total_RAW_score <= 138 ~ '84',
                                   total_RAW_score >= 139 & total_RAW_score <= 141 ~ '85',
                                   total_RAW_score >= 142 & total_RAW_score <= 144 ~ '86',
                                   total_RAW_score >= 145 & total_RAW_score <= 146 ~ '87',
                                   total_RAW_score >= 147 & total_RAW_score <= 149 ~ '88',
                                   total_RAW_score >= 150 & total_RAW_score <= 152 ~ '89',
                                   total_RAW_score >= 153 ~ '90'))


# It Worked!!!!!!#

#Now lets see according to the t score where do these participants traits sit on the autism spectrum condition
alldata <- alldata %>%
  mutate(clinical_range = case_when(total_t_score>= 0 & total_t_score <= 59 ~ 'Within Normal Limits',
                                   total_t_score >= 60 & total_t_score <= 65 ~ 'Mild',
                                   total_t_score >= 66 & total_t_score <= 75 ~ 'Moderate',
                                   total_t_score >= 76 & total_t_score <= 90 ~ 'Severe'))

view(alldata)
