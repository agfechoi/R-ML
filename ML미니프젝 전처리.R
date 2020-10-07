getwd()
setwd("C:/RM")
library(dplyr)

ufc <- read.csv("data.csv", header = TRUE)

head(ufc)
NoFemale <- ufc %>% filter(Y2 != "Women's Flyweight" & 
                              Y2 != "Women's Strawweight" &
                              Y2 != "Women's Bantamweight" &
                              Y2 != "Women's Featherweight" &)
onlyY<- select(NoFemale, contains("Y")) 
head(onlyY)
perfectY <- na.omit(onlyY)
head(perfectY)
perfectY <- perfectY %>% filter(Y1 != "Draw")
perfectY <- perfectY %>% mutate(Y = ifelse(Y1 == "Red", 1, 0))
write.csv(perfectY, "testdata1.csv")
