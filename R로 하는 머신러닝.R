#PCA차원축소!

data("USArrests")
rownames(USArrests) #행이름들 쫙 한번 봐보기
names(USArrests) #열이름들 쫙 한번 봐보기
View(USArrests)
apply(USArrests, 2, var) #각 피쳐들의 분산값을 본다
#그 결과 assault의 분산값이 젤 크다는 걸 알 수 있다
#하지만 각 값들이 차이가 많이 나므로 정규화를 해주어야한다.
#그래야 각 피처들끼리 비교가 잘된다.
USArrests_pca <- prcomp(USArrests, scale = TRUE) #정규화 식
summary(USArrests_pca)
#맨 밑에 cumulative proportion이 누적 분산값이다. 보통 85%정도에서 끊는다고 한다
#고로 PC1, PC2선에서 끊는다. PC2까지 주성분 추출한다는 소리라고 한다.
print(USArrests_pca)
#pc1과 pc2만 보면 된다. 여기서 pc1을 보면 murder, assault, rape가 모두
#비슷한 -0.5~~값을 공유한다는걸 알 수 있고 pc2에서는 urbanpop만 외따로 값이
#있다는 것을 볼 수 있다.

screeplot(USArrests_pca, type = "lines", pch = 1,
          npcs = length(USArrests_pca$sdev))
#그림으로 한번 봐도 완만해지는 선의 전 점을 잡는다 즉 2점 즉 pc2점까지 추출

biplot(USArrests_pca)
#그림을 이렇게 보고도 할수 있는데 좀더 쉽게 보기위해 그림을 좀 돌려보기로한다.

USArrests_pca$rotation <- -USArrests_pca$rotation #그림을 마이너스 먹여서 돌린다
USArrests_pca$x <- -USArrests_pca$x #x축에 마이너스 먹인다.
biplot(USArrests_pca) #보기 쉽게 돌린 그림
#이 그림을 통해서 알 수 있는것
#x축과 y축이 pc1과 pc2다. 
#아까 print에서 본거처럼 pc1에서 murder assault rape 방향이 비슷하다
#urban pop은 방향이 다르다 고로 저 셋과 urban pop은 상관관계가 낮다고 볼수있다
#california, nevada, florida가 범죄율 관련 상위그룹인걸 알 수 있다.
#pc1기준 north dakota가 최하위(pc1값이 제일 낮으므로)
#urban pop이 제일 높은 지역은 california이고 젤 낮은 지역은 미시시피다.
#인구, 범죄율 평균을 보고싶다면 가운데를 보면된다. 버지니아 와이오밍

#--------------------------------------------------------------------------
#Logistic regression
library(survival) #데이터 불러올 라이브러리
data(colon) #암 데이터
str(colon) #어떻게 생겻나 훑어보기
is.na(colon) #결측치가 있나 확인, 근데 이렇게 봐서는 다 볼수가 없음
table(is.na(colon)) #82개의 결측치가 있다는 것을 확인
colon1 <- na.omit(colon) #결측치가 있는 행 삭제
table(is.na(colon1)) #말끔히 결측치가 사라졌다.

result <- glm(status ~ rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg, family = binomial, data = colon1)
summary(result)
#유의수준이 0.05보다 작은것을 체크
#사실 뭐 다중공선성을 체크해야되는게 맞는게 생략해버리신거 같다

#이제 이걸 오즈비, 그림으로 봐보자
install.packages("moonBook")
library(moonBook)
exp(cbind(OR=coef(result), confint(result)))
#extent의 오즈비값이 큰걸 볼 수 있다.
ORplot(result)
#왜 함수를 못찾을까..


#--------------------------------------------------------------------------
library(survival)
data(colon)
str(colon)
colon1 <- na.omit(colon) #결측치 제거
result <- glm(status ~ rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg,
              family = binomial, data = colon1) #로지스틱 회귀분석!
summary(result) #결과보기

f.model <- glm(status ~ ., data = colon1) #f.model변수에 로지스틱 회귀분석 넣기
r.model1 <- step(f.model, direction = "backward") 
#backward 방식으로 로지스틱 회귀분석 최적의 모형을 찾는다.
#(AIC가 최소가 되는 값 찾기) AIC => 222.27
r.model2 <- step(f.model, direction = "forward")
#forward 방식으로 로지스틱 회귀분석 최적의 모형을 찾는다.
#(AIC)값이 229.88
r.model3 <- step(f.model, direction = "both")
#stepwise 방식으로 로지스틱 회귀분석 최적의 모형을 찾는다.
#AIC값이 222.27
#AIC값이 backward방식으로 했을때와 stepwise로 했을때가 똑같이 제일 낮다.
#---------------------------------------------------------------------------
#더미변수 만들기

cust_id <- c("c01", "c02", "c03", "c04", "c05", "c06", "c07")
age <- c(25, 41, 31, 30, 49, 53, 27)
cust_profile <- data.frame(cust_id, age, stringsAsFactors = FALSE)
cust_profile #고객 프로필 데이터로 데이터프레임 생성
sapply(cust_profile, class) #cust_id는 문자형 age는 뉴메릭

#연령대 이항변수화
cust_profile <- transform(cust_profile,
                          age_20 = ifelse(age>=20 & age<30,1,0),
                          age_30 = ifelse(age>=30 & age<40,1,0),
                          age_40 = ifelse(age>=40 & age<50,1,0),
                          age_50 = ifelse(age>=50 & age<60,1,0))

cust_profile #이항변수화 완료

install.packages("dummies") #저렇게 일일히 안하고 더미변수 만들어주는 패키지
library(dummies)
data(iris)
head(iris,10)
d <- dummy.data.frame(iris) #dummy.data.frame함수 안에 넣으면 된다..
head(d)

#----------------------------------------------------------------------
##릿지회귀
#규제 선형 회귀 유형중 하나이다. 
#릿지 회귀는 L2규제라고 하고 W의 제곱에 대해 페널티를 부여하는 방식이다.
#이 과정을 통해 계수 값의 크기를 조정하는 듯 하다(내 개인적추측)
# 가중치들의 제곱합을 최소화하는 것을 추가적인 제약 조건으로 한다.
# 가중치란 아까 말한 W다.
# 람다값이 하이퍼 파라미터라고 한다. 그러니까 최적의 람다값을 찾는거다
#람다 값이 커지면 정규화정도가 커지고 가중치 값들이 작아진다.
#람다 값이 작아지면 정규화 정도가 작아지고 가중치 값은 커진다.
#람다 값이 0이 되면 일반적인 선형 회귀모형이 된다.
#릿지회귀는 회귀 계수를 0으로 만들지 않는다.

install.packages("ISLR")
library(ISLR) #Hitters 데이터 불러오기 위한 패키지??
data(Hitters) #야구 타자들의 데이터
dim(Hitters) #322행 20열 그러니까 타자 322명과 20개의 피처(열)
table(is.na(Hitters)) #결측치 확인
Hitters <- na.omit(Hitters) #결측치 제거
table(is.na(Hitters)) #결측지 제거 확인

install.packages("glmnet") #릿지 돌릴 수 있게 해주는 패키지
library(glmnet)

x <- model.matrix(Salary~.,Hitters)[,-1] #맨앞 열을 지운 모양이다.
y <- Hitters$Salary
set.seed(1) #랜덤 지정,, 별루 안중요함. 그저 랜덤값을 동일하게 나오게 만들기 위한 것
train <- sample(1:nrow(x), nrow(x)/2) #트레인데이터 지정 1부터 n행까진데 반으로 나눈듯
test <- (-train) #test데이터 지정하는데 train데이터 제외한 나머지로 지정

y.test <- y[test] #이게 뭘 말하는거지??
# y.test에 test데이터에서 셀러리만 뽑아주는거 같은 느낌이다.

#이제부터 릿지인데 이해가 안가는 부분이다.
cv.ridge <- cv.glmnet(x[train,], y[train], alpha=0,
                      lambda = 10^seq(10, -2, length=100))
#릿지의 경우 알파값이 0이다. 이따가 라쏘는 알파값을 1로 해준다.
#람다에 왜 저렇게 해주는지는 설명 안해준다..ㅠ
plot(cv.ridge)
#그림을 통해 람다 값에 따라 평균제곱오차가 변한다는 걸 알 수 있다..
#cv lambda를 왜쓰냐 cv는 cross validation의 약자
#축소 추정을 위해서는 조절 모수를 선택해야 하는데 그 방법으로
#교차검증을 하구 그 방법중 하나가 cv.glmnet인 것이다.
bestlam <- cv.ridge$lambda.min
bestlam #제일 좋은 람다값 403.7017을 알았다....(어쩌라는걸까)

ridge.pred <- predict(glmnet(x[train,],y[train], alpha = 0,
                             lambda = cv.ridge$lambda.min), newx=x[test,])
#만든 모델로 테스트데이터를 넣어서 돌려본다.
ridge.pred #뭐가 잔뜩 나왓다.
#릿지회귀를 돌린 이후 값들이겟지.. 아마도 회귀계수 값들이 조정됬을거다.

mean((y[test]-ridge.pred)^2) #평균오차제곱의 값. MSE값
#MSE값을 비교하는건가 보다. 릿지, 라쏘, 엘라스틱 넷 그런다음 그 세개 값중에
#MSE값이 젤 낮은걸 채택하는 것이다.
coef(glmnet(x[train,],y[train],alpha=0, lambda = cv.ridge$lambda.min))
#회귀계수 값을 본다.

##--------------------------------------------------------------------------
#라쏘 회귀

library(ISLR)
data(Hitters)
table(is.na(Hitters))
Hitters <- na.omit(Hitters)
table(is.na(Hitters))

library(glmnet)

x <- model.matrix(Salary ~ ., Hitters)[,-1]
y <- Hitters$Salary
set.seed(1)

train <- sample(1:nrow(x),nrow(x)/2)
test <- (-train)
y.test <- y[test]

cv.lasso <- cv.glmnet(x[train,],y[train], alpha =1,
                      lambda = 10^seq(10, -2, length=100))
#이건 lasso이므로 알파값이 1이다. 아까 릿지에서는 알파값이 0이었다.
plot(cv.lasso)

bestlam <- cv.lasso$lambda.min
bestlam
#라쏘로 돌렸더니 최적의 람다값이 6.13정도로 나왔다. 음 그런가보다.
lasso.pred <- predict(glmnet(x[train,],y[train], alpha = 1,
                             lambda = cv.lasso$lambda.min), newx=x[test,])
lasso.pred
#얘를 토대로 이제 평균오차제곱(MSE)을 구할 수 있다.
mean((y[test]-lasso.pred)^2) #y[test] (실제값) 빼기 예측값(lasso.pred)
#그것들의 제곱의 평균인가 암튼 그 값
coef(glmnet(x[train,],y[train], alpha = 1, lambda = bestlam))

#아까 릿지로 구한 MSE값이 라쏘로 구한 MSE값보다 작다
#MSE값이 작을 수록 실제값과 예측값의 차이가 적다는 거고 그건 잘 예측했다는
#소리인거니까 작은값이 더 좋은거다.
#즉 결과값을 비교해 봤을때 릿지로 구하는게 더 낫다는 소리다.

#--------------------------------------------------------------------
##엘라스틱 넷 회귀
#가중치의 절대값의 합과 제곱합을 동시에 제약조건으로 가진다
#서로 상관관계가 높은 피처들의 경우 이들 중 중요 피처만을 셀렉하고
#다른 피터들은 모두 회귀계수를 0으로 만든다.
#이러한 성향이 alpha값에 따라 회귀계수 값이 급격히 변동할 수 있는데
#엘라스틱넷은 이를 보완하기 위해 릿지를 라쏘회귀에 추가한 것이다.

cv.eln <- cv.glmnet(x[train,],y[train], alpha=0.5,
                    lambda = 10^seq(10, -2, length=100))
plot(cv.eln)
bestlam <- cv.eln$lambda.min
bestlam #이번에는 최적의 람다값이 32.74정도가 나왔다.

eln.pred <- predict(glmnet(x[train,],y[train],alpha=0.5,
                    lambda = bestlam), newx = x[test,])
eln.pred
mean((y[test]-eln.pred)^2)#엘라스틱 넷으로 구한 평균오차제곱값
#요 값이 아까 릿지 라쏘보다 더 값이 작다.
#고로 엘라스틱으로 구한게 제일 나았다. 이경우 엘라스틱넷이 값이 젤 조앗다.


#--------------------------------------------------------------------------
#Decision Tree!
#데이터에 있는 규칙을 학습을 통해 자동으로 찾아내는 트리 기반 분류 규칙!
#의사결정 규칙을 나무 구조로 나타내어 전체 자료를 몇 개의 소집단으로 분류
#하거나 예측을 수행하는 분석방법!

#Decision Tree 분리기준은 크게 두가지가 있다.
#1. 분류나무 - 목표변수가 이산형
#2. 회귀나무 - 목표변수가 연속형

#Decision Tree 형성과정
#분석의 목적과 자료구조에 따라 적절한 분리기준과 정지규칙을 지정하여
#의사결정나무를 얻는다.

# 1.성장단계 2.가지치기 3.타당성평가 4.해석 및 예측

#Decision Tree 장단점
#장점 : 쉽다, 직관적이다, 예측속도가 빠르다, 고차원 데이터 처리가 쉽다.
#피처 스케일링이나 정규화 등의 사전가공을 안해줘도 괜찮은 편이다.
#상대적으로 적은 데이터량으로도 높은 정확도를 낸다.

#단점 : 과적합으로 알고리즘 성능이 떨어짐
#설명변수간의 중요도 판단이 불가능
#분류 경계선 부군의 자료값에 대해서 오차가 크다.

data(iris)
idx <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
#idx변수에 iris데이터를 행을 기준으로 둘로 나누는데 7대3으로 나눈다
table(idx) #150행이 102대 48로 나뉘었다.
#나뉜걸 훈련데이터와 테스트데이터로 나눈다.
train_1 <- iris[idx == 1, ]
test_1 <- iris[idx == 2, ]

#tree함수
#제일 간단한 트리 방법이다. 
install.packages("tree")
library(tree)
iris.tree1 <- tree(Species ~ ., data = train_1)
plot(iris.tree1) #틀만 나오고 내용이 안나온다.
text(iris.tree1) #이렇게 하면 글자까지 들어가서 나온다.

#ctree함수
#ctree함수는 트리형성과정 중 가지치기 과정이 필요없는 장점이 있는 함수다.
install.packages("party")
library(party)
iris.tree2 <- ctree(Species ~ ., data = train_1)
plot(iris.tree2) #막대그래프로 각각 값을 보여준다.
plot(iris.tree2, type = "simple") #막대그래프를 풀어서 글로 보여준다.
table(predict(iris.tree2), train_1$Species)
#행렬로 정확도를 보여준다. 대체로 그럭저럭 괜찮은거 같다.

test_1$pred <- predict(iris.tree2, newdata = test_1)
#테스트 데이터를 가지구 iris.tree2모델에 집어넣어서 분류를 해본다.
table(test_1$pred, test_1$Species)
#행렬로 정확도를 보여준다. 이번에도 대체로 그럭저럭 괜찮은거 같다.

#rpart함수
#얘는 tree의 성장단계를 착실히 따른다..
library(rpart)
iris.tree3 <- rpart(Species ~ ., data = train_1, method = "class",
                    parms = list(split="informative"))
#parms는 파라미터 설정인데 list는 분류고 informative로 분류한다는 소린가

iris.tree3$cptable
#nsplit은 나무의 크기를 나타냄
#터미널 노드의 개수
#cp는 복잡성 파라미터
#rel error는 나무 오류율
#xerror는 교차검증 오류율
#xstd 표준오차 어쩌구
# 가장 작은 cp인 0.01을 가지구 가지치기를 해보기루 한다.
plotcp(iris.tree3)
#가장 큰 cp를 가진 나무를 선택을 해준다.

##가지치기
pruned.iris.tr <- prune(iris.tree3, cp=0.01)
#iris.tree3$cptable이나 plotcp(iris.tree3)에서 본걸 토대로 cp=0.01값을 가져와서
#가지치기한다.

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(x=pruned.iris.tr, type=2, extra = 104, fallen.leaves = FALSE,
           box.palette = "auto", main="Decision Tree")
#x값을 가지치기한 데이터로 놓고 type이랑 extra는 그냥 보기 설정이다
#fallen.leaves는 터미널 노드를 그릴지 말지를 정한다.
#box.palette는 색깔 어떻게 칠할건지 main은 위에 제목

##해석 및 예측단계
table(train_1$Species, predict(pruned.iris.tr, type = "class"),
      dnn = c("Actual", "predicted"))

pre2 <- predict(pruned.iris.tr, newdata = test_1, type = "class")
table(test_1$Species, pre2, dnn = c("Actual", "Predicted"))
#예측모형 만들고 행렬로 정확도 본다.

##Ensemble Learning(앙상블 학습)
#여러개의 분류기를 생성하고 그 예측을 결합함으로써 보다 정확한 최종 예측을
#도출하는 기법
#왜 앙상블을 쓰냐?
#분류분석 문제를 해결하기 위해,, 분류분석문제는 예를들어 디시젼 트리같은 경우
#훈련데이터에 과적합되어서 테스트데이터 분류를 잘 못하는경우가 생긴다.
#이런건 과적합이라고 하는데 이러한 문제를 해결하기 위해서 앙상블 학습을 한다.
#여러 개의 결정 트리를 결합하여 하나의 결정트리 보다 더 좋은 성능을 내는
#머신러닝 기법
##앙상블 학습 유형
#1.Bagging(배깅), 2.Boosting(부스팅), 3.stacking(스태킹), 4.Voting(보팅)
#이중에 1번 2번이 자주 쓰인다고 한다.

##Bagging(Bootstrap aggregating)
#주어진 자료에서 여러 개의 붓스트랩 자료를 생성, 각 부스트랩 자료에 예측모델
#을 만든 후 결합하여 최종 예측 모형을 만드는 방법
#부스트랩 : 주어진 자료에서 동일한 크기의 표본을 랜덤 복원추출로 뽑은 자료

##특징
#배깅에서는 가지치기 없이 최대로 성장한 의사결정나무들을 활용한다.
#각 샘플에서 나타난 결과를 중간값으로 맞춰주기 때문에 분산을 줄이고
#과적합을 피할 수 있게 된다.

##부스트랩 알고리즘
#1.raw data에서 boostrap데이터 추출
#2.추출을 반복하여 n개의 데이터 생성
#3. 각 데이터를 각각 모델링하여 모델 생성
#4. 단일 모델을 결합하여 배깅 모델 생성

install.packages("ipred")
library(ipred)
data(iris)
str(iris)
set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
#훈련데이터, 테스트데이터로 나눈다.
trainData <- iris[ind == 1,]
testData <- iris[ind == 2,]

iris.ba <- bagging(Species ~ ., data = trainData, nbag=10, coob=TRUE)
#nbag은 의사결정 트리의 갯수를 의미
#coob=True는 결과값 마지막에 out of bag estimate of misclassification error
#를 띄울거냐 말꺼냐임
iris.ba
#그럼 이제 예측을 해보자
#훈련 데이터로 예측 시작!
trainPred <- predict(iris.ba, newdata = trainData, type = "class")
#iris.ba를 모델로 새로운 데이터는 트레인 데이터로

#행렬로 얼마나 잘 분류했는지 봐보자
table(trainPred, trainData$Species)
#완벽!
#그럼 이제 테스트 데이터로 예측을 해보자
testPred <- predict(iris.ba, newdata = testData, type = "class")
table(testPred, testData$Species)
#쪼금 틀리긴 했는데 디시젼 트리보단 괜찮게 한다!

##------------------------------------------------------------------------
#Random Forest(랜덤 포레스트)
#Bagging의 대표적인 알고리즘이다. 
#위의 Bagging을 쓰기보다 랜덤 포레스트를 쓰는게 훨씬일반적이라고 한다.

#장점
#분류, 회귀 모두 사용가능!
#예측력이 매우 높다!
#의사결정 트리의 과대적합 단점을 보완!
#의사결정 트리의 직관적인 장점을 그대로 가져왔다!

#단점
#결과 해석이 좀 어렵다

##랜덤 포레스트 알고리즘
#1. Bagging을 사용하여 Boostrap을 만든다.
#2. Boostrap을 이용하여 Decision Tree를 학습시킨 후 예측을 수행한다.
#3. 1,2를 충분히 반복하여 여러개의 디시젼 트리에 대한 예측을 모은다
#4. 회귀트리의 경우 모인 예측들의 평균을 취하고
#분류트리의 경우 다중투표를 통해 가장 많은 투표를 받은 범주를 채택
#최종적으로 최적의 앙상블 모델 결과 도출


install.packages("randomForest") #랜덤 포레스트 돌리기 위한 패키지
library(randomForest)
data(iris)
#기계적으로 데이터를 훈련용 테스트용으로 나눈다.
idx <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
train.data <- iris[idx == 1,]
test.data <- iris[idx == 2,]

#이제 랜덤포레스트 가자
rf <- randomForest(Species ~ ., data = train.data, ntree=100, mtry=2,
                   proximity=TRUE, importance=TRUE)
#ntree는 트리개수 설정, mtry는 각 노드 설정시 설명변수 후보 개수 설정
#importance는 변수 중요성을 나타내주는 거라는데 나오는지 잘 모르겠다.

rf
#type을통해 분류인걸 체크, 나무수 100개 오차율 6.03% 확인

rf1 <- randomForest(Species ~ ., data = train.data)
#이렇게 번잡한 설정을 제외하고 돌릴 수도 있다. 그러면 알아서 설정하고 돌림

rf1
#그 결과 오히려 오차율이 더 좋아졌다. 5.17%.. 그럴수도 있지.

#오차율 계산하기
table(predict(rf), train.data$Species)
plot(rf)
#트리 수가 늘면서 에러가 점점 낮아지는걸 볼 수 있다.
legend("topright", colnames(rf$err.rate), col=1:4, cex = 0.8, fill = 1:4)
#보기쉽게 레전드를 박아놓는다

#변수 중요도
importance(rf) #숫자가 제일 높은거 보면 된다. 그런거 같다
varImpPlot(rf) #그림으로 볼수도 있다. 위에서부터 중요도가 높은 순이다.

#test data예측~
pre.rf <- predict(rf, newdata = test.data)
table(pre.rf, test.data$Species)
#제법 괜찮다!

##-----------------------------------------------------------------------
#Boosting(부스팅)
#여러개의 약한 학습기를 순차적으로 학습-예측하면서 잘못 예측한 데이터에
#가중치를 부여해 오류를 개선해 나가면서 학습하는 방식
#1.Raw Data에 동일가중치로 모델 생성
#2.생성된 모델로 인한 오분류 데이터 수집
#3.오분류 데이터에 높은 가중치 부여
#4.과정 반복을 통해 모델의 정확도 향상!

#Boosting의 종류
#1. Adaboost(Adaptive Boost), 2. GBM(Gradient Boost Machine), 
#3. XGBoost(eXtra Gradient Boost), 4. LightGBM, 5. CatBoost

##Adaboost(Adaptive Boost)
#약한 학습기의 오류 데이터에 가중치를 부여하면서 부스팅을 수행하는 알고리즘

#장점
#오류율이 낮고 코드가 쉽다
#가장 좋은 분류기를 ㄱ지고 분류하며 매개변수가 없다.

#단점
#분류 정밀도가 높지만 학습 데이터의 노이즈에 쉽게 영향을 받음

#실습
install.packages("adabag") #adaboost를 쓰기위한 패키지
library(adabag)
library(caret) #createDataPartition을 쓰기 위한 라이브러리
indexes = createDataPartition(iris$Species, p=.90, list = FALSE)

#훈련데이터와 테스트데이터를 나누는 새로운 방법 
#list=False면 행렬로 뽑고 true면 리스트로 뽑는다.

train = iris[indexes,] #train에 아까 나눈 그 데이터를 넣는다. 90%
test = iris[-indexes,] #test에 아까 나누고 남은 데이터를 넣는다. 10%

model <- boosting(Species ~ ., data = train, boos = TRUE, mfinal = 50)
#boos => 해당 반복에 대한 관측치에 가중치를 사용하여 훈련 세트에 
#부스트랩 샘플을 추출하는 경우 = TRUE
#mfinal => 부스팅이 실행되는 반복회수(정수값) 기본값은 100

model #부스팅 결과
model$importance #부스팅 결과 중 각 열의 중요도 비교 높을수록 중요도 높음
print(model$trees[1]) #트리수
print(names(model)) #항목수

table(model$class, train$Species) #잘 분류되었나 확인 :: 완벽!

#만들어낸 부스팅 모델을 토대로 이제 테스트 데이터 넣고 예측!
pred <- predict(model, test)
pred # 완벽하진 않은 결과지만 그럭저럭 괜찮다
print(pred$confusion) #혼동행렬만 보고싶을때
print(pred$error) #에러율만 보고 싶을때

#똑같은 결과를 도출하는 다른 코딩 소개,, 함수

model2 <- predict.boosting(model, newdata = test)
model2
#predict.boosting이라는 함수를 쓰면 똑같은 결과를 낼 수 있다.

cvmodel <- boosting.cv(Species ~ ., data = iris, boos = TRUE, mfinal = 10,
                       v=5)
#boosting.cv => 교차검증을 통해 전체 data를 대상으로 돌리는 함수
#부스팅을 가지는 브이폴드 교차검증 실행 함수
#v => 몇개의 교차검증을 실시할건지
#data가 전체 데이터인걸 주목!
print(cvmodel)
print(cvmodel[-1]) #앞에 복잡하고 해석에 의미 없는 "class"를 없애고 보여줌

#--------------------------------------------------------------------------
#GBM(Gradient Boost Machine)

#adaboost와 유사하나 가중치 업데이트를 경사 하강법을 이용하는 것에서 차이
#경사하강법이란?
#분류 실제값, 예측함수 오류식을 실제값-예측함수라고 하면 이 오류식을
#최소화하는 방향성을 가지고 가중치 값을 업데이트한다

##장점
#랜덤 포레스트보다 예측 성능이 조금 뛰어나다 

##단점
# 수행시간이 오래걸림
# 하이퍼 파라미터 튜닝이 좀 어렵다

#실습
install.packages("gbm") #gbm을 하기 위한 패키지
library(gbm)
library(caret)
indexes <- createDataPartition(iris$Species, p=0.9, list = FALSE)
train <- iris[indexes,]
test <- iris[-indexes,]

mod_gbm <- gbm(Species ~ ., data = train, distribution = "multinomial",
               cv.folds = 10, shrinkage = .01, n.minobsinnode = 10,
               n.trees = 200)
## multinomial 다항분포, cv.folds => 교차검증 개수, 
#shrinkage 각 트리 확산 정도, n.minobsinnode => 가지고 있는 data의 실제 숫자


print(mod_gbm)

#만든 모델에 테스트 데이터 넣어서 예측하기
pred <- predict.gbm(object = mod_gbm, newdata = test, n.trees = 200,
                    type = "response")

labels <- colnames(pred)[apply(pred, 1, which.max)]
#가장 많은 예측값을 가진 것을 가져오겠다는 소린데 좀 이해가 안가네.
result <- data.frame(test$Species, labels)
print(result)

cm <- confusionMatrix(test$Species, as.factor(labels))
#as.factor로 왜하냐? 왜냐면 labels는 character로 되어있고 test$species는
#factor로 되어있는데 컨퓨전 매트릭스 함수를 돌리려면 둘다 팩터여야 된다.
#그러니까 labels 앞에 에즈 펙터를 붙여서 팩터로 맞춰주고 돌리는거다.
cm
#분류가 아주 잘 되었군!

#다른 방법으로 해볼 수도 있다고 한다. *참고

tc <- trainControl(method = "repeatedcv", number = 10)
#repeatedcv => 교차검증, #number => 교차검증을 몇번할거냐
model <- train(Species ~ ., data = train, method = "gbm", trControl = tc)

pre <- predict(model, test)

result <- data.frame(test$Species, pre)
result

cm2 <- confusionMatrix(test$Species, pre)
cm2
#똑같이 분류가 아주 잘 되었군!

##--------------------------------------------------------------------------
#XGBoost(eXtra Gradient Boost)