#C5.0 결정트리(디시전트리)를 사용한 위험 은행 대출 확인

#1단계 데이터수집
#2단계 데이터 준비와 탐구
#3단계 데이터에 대한 모델 훈련
#4단계 모델 성능 평가


#1단계, 2단계 데이터수집/데이터 준비와 탐구
getwd()
setwd("C:/RM")
credit <- read.csv("credit.csv")
str(credit)
#팩터와 정수 데이터의 혼합으로 된 1000개의 예제와 17개의 속성이 있다.
View(credit)
table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)
#대출의 총액은 4달에서 72달까지의 기간에 250DM에서 18,420DM까지의 규모다
#일반적으로 18개월에 2,320DM이다.

table(credit$default) #no 700 yes 300 30%의 대출자가 채무 불이행이 됐다.

#데이터 준비: 임의의 훈련 데이터와 테스트 데이터 생성
#1000개의 예제가 있으므로 900대 100으로 예쁘게 나누고 싶지만 데이터를 보면
#무작위가 아니라 대출 총액에 따라 분류해놓아서 랜덤으로 흩어놓아야 한다.

#order함수는 오름차순이나 내림차순으로 리스트를 재정렬한다. 무작위 수를 
#생성하는 함수와 order함수를 같이 쓰면 무작위 리스트를 만들 수 있다.
#무작위 수를 생성하기 위해 0과 1사이에 임의의 수열을 만드는 runif함수를 쓴다.

set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
#runif로 credit행들 천개를 흩어놓고 order함수로 1000개의 무작위 수 벡터를
#반환 한다. 그 다음 credit_rand에 넣는다
# 그 결과 똑같은 데이터들을 담고 있지만 순서가 랜덤으로 된 데이터 셋이 완성
# summary 함수로 그렇게 되었는지 확인해본다.

summary(credit$amount)
summary(credit_rand$amount)
# 둘다 똑같은 최솟값, 최댓값을 가지고 4분위수를 가진다.

head(credit$amount)
head(credit_rand$amount)
#head함수로 보면 그 값들의 순서가 달라진 것을 알 수 있다.

#이제 랜덤으로 되었으니 90대 10으로 훈련세트과 테스트세트로 나눈다.
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

#잘 섞여서 들어갔다면 처음에 디폴트열 비율대로 각각 잘 들어갔을 것이다.
#확인해보자
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
#잘 섞인 것 같다!

#3단계 데이터에 대한 모델 훈련
install.packages("C50")
library(C50)

#분류기 만들기
# m <- c5.0(train, class, trials = 1, costs = NULL)
# train은 훈련 데이터 세트
# class는 훈련 데이터 각 행에 대해 범주를 가진 팩터 벡터 한마디로 보고싶은 값
# trial은 부스팅 반복의 수를 조절하기 위한 선택적 숫자 기본적으론 1
# 이후 부스팅 할때 숫자를 더 늘린다.
# costs는 오차의 타입에 관련되 cost를 명시한거라는데 별로 안중요한거 같다

#예측하기
# p <- predict(m, test, type = "class")
# m은 아까 c5.0함수 돌려서 넣은 모델
# test는 테스트 데이터 세트
# type는 class이거나 prob이다. 예측은 범주 값이거나 예측된 확률을 명시한다.
# 이 함수는 예측된 범주 값의 벡터를 반환하거나 type의 매개변수 값에 따라
# 예측된 환률을 변환한다. 이제 해보자
credit_model <- C5.0(credit_train[-17], credit_train$default)
# 17번째 열이 디폴트 이므로 17번째 열을 빼준다.
#이걸 바로 돌리면 안돌아가는데 심지어 책도 이렇게 써있다...
#에러: C5.0 models require a factor outcome라고 나오는데 이는 뒤에있는
#credit_train$default값이 팩터가 아니기 때문에 저러는거다. 
#그래서 저녀석을 팩터값으로 변환시켜줘야 돌아간다.
credit_train$default<-as.factor(credit_train$default)
#이렇게 바꿔준다..

credit_model1 <- C5.0(credit_train[, -17], credit_train[, 17])
# 이렇게 해도 똑같다.

credit_model
#900개의 예제를 16개의 속성을 사용해서 트리를 만들었는데 트리 사이즈는 67이다.
#이 트리를 자세히 보기위해 summary함수를 써본다
summary(credit_model)
# 괄호안에 숫자들이 말하는 것은 다음과 같다.
# (358/44)의 경우 358개 예제가 결정에 합당했고 44개는 no로 잘못 분류되었다.
# 즉 다시말해서 44명은 실제로 채무 불이행을 했는데 모델의 예측은 반대였다.

#밑에 교차표, 혼동행렬이 나오는데 이를 통해서 이 모델이 약 14%의 오차율로
# 900개의 데이터중 125개를 잘못 분류했다는 것을 알 수 있다.
# 102개는 yes인데 no로 잘못분류(거짓 부정)했고 
# 23개는 no인데 yes로 잘못분류(거짓 긍정)했다.

#결정트리는 다소 훈련데이터에 과적합화하는 경향이 있다. 

#4단계 모델 성능 평가

credit_pred <- predict(credit_model, credit_test)
#그러니까 900개가지고 훈련시켜보고 이제 테스트로 따로 빼둔 100개를 
#동일 모델로 한번 봐보겠다는 것이다.

#gmodels의 CrossTable()함수를 사용하여 실제 범주 값을 비교한다.
library(gmodels)
CrossTable(credit_test$default, credit_pred)
#정확도가 73% 오차율이 27%가 나왔다. 
#문제는 실제 채무 불이행이 32명인데 그중에 16명만 제대로 맞추고 16명을
#놓쳤다는 것이다. 은행 입장에서 50%정도의 예측률은 매우 실망스럽고
# 고비용 실수에 해당한다. 이 모델 믿고 빌려줬다가 안갚으면 은행에겐 큰 손해니까
# 그러므로 이 모델이 더 정교해질 수 있도록 수정해야한다.

# 그 방법으로 부스팅이 있다. 
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, 
                       trials = 10) #trials=10으로 부스팅을 한다!

credit_boost10
#10회 반복을 통해 아까와 결과가 달라진 것을 알 수 있다.
# 아까는 나무사이즈가 67이었는데 이젠 약 52개다
summary(credit_boost10)
# 밑에 혼동행렬을 보면 29개의 실수를 하여 3.2%의 오차율을 가진 것을 알수잇다
# 아까 부스팅 하기 이전 14% 보다 더 나아진 것이다!

#테스트 데이터에도 넣어서 그 결과가 더 나아졌는지 봐보자
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10)
#부스팅 이전 테스트데이터한것과 비교해보면 25%의 오차율로 아까보다 2%떨여졋다
#하지만 채무불이행 여부는 아까처럼 16/16으로 50%의 예측력은 여전히 문제다

#채무를 불이행 할 것 같은 사람에게 대출을 해주면 고비용 실수가 된다.
#은행입장에서는 모델의 정확도가 다소 떨어지더라도 고비용 실수를 줄이는 것이
#더 이득이 될 수 있다

#고로 채무를 이행할 수 있었는데 못이행할 것이라고 하는 실수에는 신경쓰지 않고
#채무를 이행할 수 없는데 이행 할 수 있다고 판단하는 실수에 가중 벌점을 
#주어서 더 엄격하게 모델을 바꾸는게 은행입장에서는 더 좋다.
#즉 살짝 애매한 사람들까지 전부 불이행 할것 같다고 해버리는 것이다.

#대출 채무 불이행이 잃어버린 기회에 4배라고 책정을 하는 매트릭스를 만든다.
error_cost <- matrix(c(0,1,4,0), nrow=2)
error_cost
#제대로 분류할때는 0비용 거짓 부정은 4의 비용 거짓 긍정은 1의 비용으로 놓는다
#이 표를 아까 모델에 적용시켜보자

credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred)
#이 결과 오차율은 32%로 껑충 뛰었지만 은행이 싫어하는 실수가 줄었다
#아까는 32개중에 16개 맞추고 16개 못맞추었지만
#이번에는 26개맞추고 6개 틀렸다. 은행입장에서 고비용 실수를 줄이게 된것이다.
