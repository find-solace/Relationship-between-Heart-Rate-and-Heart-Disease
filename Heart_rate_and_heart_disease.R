library(tidyverse)
hd_dataset <- read_csv("processed_cleveland.csv")
head(hd_dataset)

sum(is.null(hd_dataset))

#Classifying non-zero values in the num column as 1, to achieve a binary category
#as per the dataset's codebook.

hd_dataset <- hd_dataset %>% mutate(hd = ifelse(num >0, 1, 0))

class(hd_dataset$sex)

#Recoding the sex variable as a factor with levels.

hd_dataset <- hd_dataset %>% 
  mutate(sex = factor(sex, levels = c(0, 1), labels = c("female", "male")))

#Identifying the predictors of heart disease using the sex and age variables

#Sex:Chi-squared test
contingency_table <- table(hd_dataset$sex, hd_dataset$hd)
contingency_table

hd_sex <- chisq.test(contingency_table)




