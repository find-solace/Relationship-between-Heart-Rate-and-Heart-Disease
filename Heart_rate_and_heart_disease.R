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

#Identifying the predictors of heart disease using the sex, age, and thalach variables

#Sex:Chi-squared test
contingency_table <- table(hd_dataset$sex, hd_dataset$hd)
contingency_table

hd_sex <- chisq.test(contingency_table)

#age: t-test
hd_age <- t.test(age ~ hd, data = hd_dataset)

#thalach: t-test
hd_thalach <- t.test(thalach ~ hd, data = hd_dataset)


## Visualizing the Relationship between age and hd

#re-coding hd as "No disease and disease"

hd_dataset <- hd_dataset %>%
  mutate(hd_labelled = ifelse(hd == 0, "No Disease", "Disease"))

ggplot(data = hd_dataset, aes(x = hd_labelled, y = age)) + geom_boxplot()

#The median for individuals with the disease is at a higher age compared to the median
#for individuals with no disease, which is at a lower age.
#Most individuals with hd are also older than 50 years.

##Visualizing the relationship between sex and hd

ggplot(data = hd_dataset, aes(x = hd_labelled, fill = sex)) + 
  geom_bar(position = "fill") + ylab("Sex %")

#More than three quarters of individuals with hd are male. The gender proportion is nearly equal
#among individuals with no disease, but males are also slightly more.

##Visualizing the relationship between thalach and hd
ggplot(data = hd_dataset, aes(x = hd_labelled, y = thalach)) + geom_boxplot()

#The disease is distributed more among individuals with lower thalach,
#while individuals with no disease generally have higher thalach.























