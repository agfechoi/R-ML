getwd()
setwd("C:/RM")
#회귀 트리로 와인 퀄리티 구분하기~!~!
wine <- read.csv("whitewines.csv")
head(wine)
str(wine)
#트리의 장점은 전처리 없이 많은 데이터를 다룰 수 있다
#이게 무슨말이냐면 속성의 정규화나 표준화가 필요없다는 것이다.
hist(wine$quality)
#6을 중심으로한 정규분포를 그리고 있다
summary(wine)

#데이터를 훈련/테스트 데이터로 나눈다
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

#회귀 트리 모델 구현하는 패키지
install.packages("rpart")
library(rpart)
# lm이랑 문법이 같다
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

#회귀 트리 모델 시각화
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

#모델 성능 평가
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart) #테스트 데이터를 훈련데이터로 훈련한 트리에 넣은 결과
summary(wine_test$quality) #원래 테스트 데이터에 들어있는 퀄리티
#이렇게 비교하면 최상과 최하 와인은 정확하게 식별하지 못하지만
#1분위수, 3분위수 사이의 와인은 그럭저럭 잘 식별함을 알 수 있다.

#예측된 값과 실제 quality 값의 상관도는 모델의 성능을 나타내는 간단한
#방법을 제시한다. cor()함수는 예측된 값과 해당하는 실제값을 잘 비교한다.
cor(p.rpart, wine_test$quality)
#0.54정도의 상관관계는 어느정도 받아들일 수 있지만 상관관계는 예측값과
#실제 값이 얼마나 강하게 관련됐는지를 측정한다. 이는 예측값과 실제 값이
#얼마나 차이가 있는지 측정하는 것은 아니다.

#평균 절대 오차를 이용한 성능 측정 (MAE,, mean absolute error)
#평균적으로 예측값과 실제값이 얼마나 차이가 있는지 고려하는 방법이다.
#n은 예측의 개수이고 e는 예측 i에 대한 오차를 지시한다.
#본질적으로 이 공식은 오차에 대한 절댓값의 평균이다.

#젠장 뭐라는지 모르겠군

#모델 성능 향상(여기서 뭔가 뻑났다. 값이 잘못나오는거 같다.)
install.packages("RWeka")
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)

p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)

#선형 회귀를 사용한 의료비 예측
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
head(insurance)
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)
hm <- cor(insurance[c("age", "bmi", "children", "expenses")])
library(corrplot)
corrplot.mixed(hm)
pairs(insurance[c("age", "bmi", "children", "expenses")])
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])
ins_model <- lm(expenses ~ ., data = insurance)
ins_model
summary(ins_model)
