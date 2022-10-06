#EQ auto tidy script (attempt)


library(tidyverse)


set.seed(1234)

# Importing the data into R.

library(readr)
P1_ASC_SMALL_EQ <- read_csv("EQ_data/P1_ASC_SMALL_EQ.csv")
P2_ASC_SMALL_EQ <- read_csv("EQ_data/P2_ASC_SMALL_EQ.csv")
P3_ASC_SMALL_EQ <- read_csv("EQ_data/P3_ASC_SMALL_EQ.csv")
P4_ASC_SMALL_EQ <- read_csv("EQ_data/P4_ASC_SMALL_EQ.csv")
P5_ASC_SMALL_EQ <- read_csv("EQ_data/P5_ASC_SMALL_EQ.csv")
P6_ASC_SMALL_EQ <- read_csv("EQ_data/P6_ASC_SMALL_EQ.csv")
P7_ASC_SMALL_EQ <- read_csv("EQ_data/P7_ASC_SMALL_EQ.csv")
P8_ASC_SMALL_EQ <- read_csv("EQ_data/P8_ASC_SMALL_EQ.csv")
P9_ASC_SMALL_EQ <- read_csv("EQ_data/P9_ASC_SMALL_EQ.csv")
P10_ASC_SMALL_EQ <- read_csv("EQ_data/P10_ASC_SMALL_EQ.csv")
P11_ASC_SMALL_EQ <- read_csv("EQ_data/P11_ASC_SMALL_EQ.csv")
P12_ASC_SMALL_EQ <- read_csv("EQ_data/P12_ASC_SMALL_EQ.csv")
P13_ASC_SMALL_EQ <- read_csv("EQ_data/P13_ASC_SMALL_EQ.csv")
P14_ASC_SMALL_EQ <- read_csv("EQ_data/P14_ASC_SMALL_EQ.csv")
P15_ASC_SMALL_EQ <- read_csv("EQ_data/P15_ASC_SMALL_EQ.csv")
P16_ASC_SMALL_EQ <- read_csv("EQ_data/P16_ASC_SMALL_EQ.csv")
P17_ASC_SMALL_EQ <- read_csv("EQ_data/P17_ASC_SMALL_EQ.csv")
P18_ASC_SMALL_EQ <- read_csv("EQ_data/P18_ASC_SMALL_EQ.csv")
P19_ASC_SMALL_EQ <- read_csv("EQ_data/P19_ASC_SMALL_EQ.csv")
P20_ASC_SMALL_EQ <- read_csv("EQ_data/P20_ASC_SMALL_EQ.csv")
P21_ASC_SMALL_EQ <- read_csv("EQ_data/P21_ASC_SMALL_EQ.csv")
P22_ASC_SMALL_EQ <- read_csv("EQ_data/P22_ASC_SMALL_EQ.csv")
P23_ASC_SMALL_EQ <- read_csv("EQ_data/P23_ASC_SMALL_EQ.csv")
P24_ASC_SMALL_EQ <- read_csv("EQ_data/P24_ASC_SMALL_EQ.csv")
P25_ASC_SMALL_EQ <- read_csv("EQ_data/P25_ASC_SMALL_EQ.csv")
P26_ASC_SMALL_EQ <- read_csv("EQ_data/P26_ASC_SMALL_EQ.csv")
P27_ASC_SMALL_EQ <- read_csv("EQ_data/P27_ASC_SMALL_EQ.csv")
P28_ASC_SMALL_EQ <- read_csv("EQ_data/P28_ASC_SMALL_EQ.csv")
P29_ASC_SMALL_EQ <- read_csv("EQ_data/P29_ASC_SMALL_EQ.csv")
P30_ASC_SMALL_EQ <- read_csv("EQ_data/P30_ASC_SMALL_EQ.csv")
P31_ASC_SMALL_EQ <- read_csv("EQ_data/P31_ASC_SMALL_EQ.csv")
P32_ASC_SMALL_EQ <- read_csv("EQ_data/P32_ASC_SMALL_EQ.csv")
P33_ASC_SMALL_EQ <- read_csv("EQ_data/P33_ASC_SMALL_EQ.csv")
P34_ASC_SMALL_EQ <- read_csv("EQ_data/P34_ASC_SMALL_EQ.csv")
P35_ASC_SMALL_EQ <- read_csv("EQ_data/P35_ASC_SMALL_EQ.csv")
P36_ASC_SMALL_EQ <- read_csv("EQ_data/P36_ASC_SMALL_EQ.csv")
P37_ASC_SMALL_EQ <- read_csv("EQ_data/P37_ASC_SMALL_EQ.csv")
P38_ASC_SMALL_EQ <- read_csv("EQ_data/P38_ASC_SMALL_EQ.csv")
P39_ASC_SMALL_EQ <- read_csv("EQ_data/P39_ASC_SMALL_EQ.csv")
P40_ASC_SMALL_EQ <- read_csv("EQ_data/P40_ASC_SMALL_EQ.csv")
#P41_ASC_SMALL_EQ <- read_csv("EQ_data/P41_ASC_SMALL_EQ.csv")
P42_ASC_SMALL_EQ <- read_csv("EQ_data/P42_ASC_SMALL_EQ.csv")
P43_ASC_SMALL_EQ <- read_csv("EQ_data/P43_ASC_SMALL_EQ.csv")
P44_ASC_SMALL_EQ <- read_csv("EQ_data/P44_ASC_SMALL_EQ.csv")
P45_ASC_SMALL_EQ <- read_csv("EQ_data/P45_ASC_SMALL_EQ.csv")
P46_ASC_SMALL_EQ <- read_csv("EQ_data/P46_ASC_SMALL_EQ.csv")
P47_ASC_SMALL_EQ <- read_csv("EQ_data/P47_ASC_SMALL_EQ.csv")
P48_ASC_SMALL_EQ <- read_csv("EQ_data/P48_ASC_SMALL_EQ.csv")
P49_ASC_SMALL_EQ <- read_csv("EQ_data/P49_ASC_SMALL_EQ.csv")
P50_ASC_SMALL_EQ <- read_csv("EQ_data/P50_ASC_SMALL_EQ.csv")
P51_ASC_SMALL_EQ <- read_csv("EQ_data/P51_ASC_SMALL_EQ.csv")
P52_ASC_SMALL_EQ <- read_csv("EQ_data/P52_ASC_SMALL_EQ.csv")
P53_ASC_SMALL_EQ <- read_csv("EQ_data/P53_ASC_SMALL_EQ.csv")
P54_ASC_SMALL_EQ <- read_csv("EQ_data/P54_ASC_SMALL_EQ.csv")
P55_ASC_SMALL_EQ <- read_csv("EQ_data/P55_ASC_SMALL_EQ.csv")
P56_ASC_SMALL_EQ <- read_csv("EQ_data/P56_ASC_SMALL_EQ.csv")
P57_ASC_SMALL_EQ <- read_csv("EQ_data/P57_ASC_SMALL_EQ.csv")
P58_ASC_SMALL_EQ <- read_csv("EQ_data/P58_ASC_SMALL_EQ.csv")
P59_ASC_SMALL_EQ <- read_csv("EQ_data/P59_ASC_SMALL_EQ.csv")
P60_ASC_SMALL_EQ <- read_csv("EQ_data/P60_ASC_SMALL_EQ.csv")

# Combining the individual data spreadsheets into one data frame.

alldata <- rbind (P1_ASC_SMALL_EQ, P2_ASC_SMALL_EQ, P3_ASC_SMALL_EQ,P4_ASC_SMALL_EQ, P5_ASC_SMALL_EQ,
                  P6_ASC_SMALL_EQ, P7_ASC_SMALL_EQ, P8_ASC_SMALL_EQ, P9_ASC_SMALL_EQ, P10_ASC_SMALL_EQ,
                  P11_ASC_SMALL_EQ, P12_ASC_SMALL_EQ, P13_ASC_SMALL_EQ, P14_ASC_SMALL_EQ, P15_ASC_SMALL_EQ,
                  P16_ASC_SMALL_EQ,P17_ASC_SMALL_EQ, P18_ASC_SMALL_EQ, P19_ASC_SMALL_EQ,P20_ASC_SMALL_EQ,
                  P21_ASC_SMALL_EQ, P22_ASC_SMALL_EQ, P23_ASC_SMALL_EQ,P24_ASC_SMALL_EQ, P25_ASC_SMALL_EQ, P26_ASC_SMALL_EQ,
                  P27_ASC_SMALL_EQ, P28_ASC_SMALL_EQ, P29_ASC_SMALL_EQ, P30_ASC_SMALL_EQ, P31_ASC_SMALL_EQ,P32_ASC_SMALL_EQ,
                  P33_ASC_SMALL_EQ, P34_ASC_SMALL_EQ, P35_ASC_SMALL_EQ, P36_ASC_SMALL_EQ, P37_ASC_SMALL_EQ, P38_ASC_SMALL_EQ,
                  P39_ASC_SMALL_EQ,P40_ASC_SMALL_EQ,P42_ASC_SMALL_EQ, P43_ASC_SMALL_EQ, P44_ASC_SMALL_EQ,
                  P45_ASC_SMALL_EQ, P46_ASC_SMALL_EQ, P47_ASC_SMALL_EQ,P48_ASC_SMALL_EQ, P49_ASC_SMALL_EQ, P50_ASC_SMALL_EQ,
                  P51_ASC_SMALL_EQ, P52_ASC_SMALL_EQ, P53_ASC_SMALL_EQ, P54_ASC_SMALL_EQ, P55_ASC_SMALL_EQ,P56_ASC_SMALL_EQ,
                  P57_ASC_SMALL_EQ, P58_ASC_SMALL_EQ,P59_ASC_SMALL_EQ, P60_ASC_SMALL_EQ)

#view(alldata)

alldata <- alldata%>%
  mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
alldata$Group_Status[alldata$Group_Status == 'TRUE'] <- "ASC"
alldata$Group_Status[alldata$Group_Status == 'FALSE'] <- "TD"
view(alldata)

# EQ scoring ignore none as these are filler items we are not interested in. 

#We want to add a column called EQ_tidy that converts the values to the appropriate score. 
# What the code should do is take items that are positively scored and turn a 1 into a 2, 
# turn a 2 into a 1, and turn 3 and 4 into a 0. 
# What the code should do is take items that are negatively scored and turn a 4 into a 2, 
# turn a 3 into a 1, and turn 1 and 2 into a 0. 

alldata <- alldata %>% mutate(EQ_tidy = case_when 
                              (EQ_scoring_type == 'positive' &  EQ_resp == 1 ~ 2, 
                                EQ_scoring_type == 'positive' &  EQ_resp == 2 ~ 1,
                                EQ_scoring_type == 'positive' &  EQ_resp == 3 ~ 0,
                                EQ_scoring_type == 'positive' &  EQ_resp == 4 ~ 0,
                                EQ_scoring_type == 'none' ~ 0,
                                EQ_scoring_type == 'negative' &  EQ_resp == 1 ~ 0,
                                EQ_scoring_type == 'negative' &  EQ_resp == 2 ~ 0,
                                EQ_scoring_type == 'negative' &  EQ_resp == 3 ~ 1,
                                EQ_scoring_type == 'negative' &  EQ_resp == 4 ~ 2,))
view(alldata)

# It worked!!!!!

# Then we can add this column for each participant to get their individual EQ score
EQ_score <- alldata %>%
  group_by(participant) %>%
  summarise_at(vars(EQ_tidy), list(EQ_score = sum))

alldata <- inner_join(alldata, EQ_score, by = "participant")

view(alldata)


#Export a CSV of the new data set...
write.csv(alldata,"//nask.man.ac.uk/home$/Desktop/ASC_small/EQ_data\\alldata.csv", row.names = TRUE)

#Let's look at the difference between the two groups
alldata %>% 
  group_by(Group_Status) %>%
  summarise(mean(EQ_score), sd(EQ_score))

#Much lower empathy scores for the ASC group compared to the TD group Let's have a look at this with a t test
ASC_EQ_mean <- rnorm(60, mean = 24.5, sd = 10.1)
TD_EQ_mean <- rnorm(60, mean = 50.9, sd = 11.8)
t.test(ASC_EQ_mean, TD_EQ_mean, var.equal = TRUE)

#Two Sample t-test

#data:  ASC_EQ_mean and TD_EQ_mean
#t = -17.655, df = 118, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -35.76538 -28.55137
#sample estimates:
#  mean of x mean of y 
#20.21738  52.37576 

### alldata <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/EQ_data/alldata.csv")


