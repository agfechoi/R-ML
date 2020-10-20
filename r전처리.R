install.packages("dplyr")
library(dplyr)
library(ggplot2)
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5,4,3,NA,1))
df
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))
df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)
mean(df$score)
mean(df$score, na.rm = T)
sum(df$score, na.rm = T)

midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
summary(midwest)
midwest <- midwest %>% mutate(prop_of_kid = (poptotal-popadults)/poptotal*100) %>% 
  arrange(desc(prop_of_kid))

 
midwest <- midwest %>% 
  mutate(how_prop_kid = ifelse(prop_of_kid >=40, "large",
                               ifelse(prop_of_kid >= 30, "middle", "small")))

View(midwest)
table(midwest$how_prop_kid)


midwest <- midwest %>% 
  mutate(prop_of_asian = (popasian/poptotal)*100) %>% 
  arrange(desc(prop_of_asian))

midwest %>% select(state, county, prop_of_asian) %>% 
  tail(10)

outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
outlier
table(outlier$sex)
table(outlier$score)
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
outlier$score <- ifelse(outlier$score == 6, NA, outlier$score)
outlier
library(dplyr)
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))
library(ggplot2)
boxplot(mpg$hwy)$stats
mpg$hwy
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))
