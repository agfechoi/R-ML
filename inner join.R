getwd()
setwd("C:/Users/user/Desktop/mp defined")
setwd("C:/Users/user/Desktop/제발망플")

gangnam <- read.csv("mp강남구.csv", header = TRUE)
mapo <- read.csv("mp마포구.csv", header = TRUE)

head(gangnam)
head(mapo)

gangnamed <- read.csv("강남구ed.csv", header = TRUE)

mapoed <- read.csv("마포구ed.csv", header = TRUE)

head(gangnamed)
head(mapoed)

library(dplyr)

allgangnam <- gangnam %>% inner_join(gangnamed)

head(coco)
dim(coco)

allmapo <- mapo %>% inner_join(mapoed)
dim(mapo)
dim(mapoed)
dim(allmapo)
