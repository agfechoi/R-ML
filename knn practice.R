library(class) #k근접 이웃법
library(kknn) #가중 k 근접 이웃법
library(e1071) #SVM
library(caret) #인자 조정 선별별
library(MASS) #데이터를 담고있는 라이브러리리
library(reshape2) #박스플롯을 생성하는데 도움
library(ggplot2) #박스플롯 생성
library(kernlab) #SVM 피처 선택에 도움줌

data(Pima.tr)
str(Pima.tr)
data(Pima.te)
str(Pima.te)

Pima <- rbind(Pima.tr, Pima.te)
str(Pima)
head(Pima)
pima.melt <- melt(Pima, id.vars = "type")
head(pima.melt)
# variable 별로 두 열로해서 x축은 type y축은 value로 박스플롯으로 보여줘!
ggplot(data = pima.melt, aes(x = type, y = value)) +
  geom_boxplot() + facet_wrap(~ variable, ncol = 2)
#그림들의 결과 y축이 모두 같은 값을 공유하고 있는데 이러면 안댄다.
#표준화를 통해서 단위들이 의미없게 만들어야 제대로 볼 수 있을 것이다.

#표준화 과정
# 맨 마지막 열인 type만 빼고 나머지열값들 모두 표준화하는 것.
# 데이터 프레임에 비율변환을 가하면 데이터 타입이 자동으로 행렬로 변하므로
# data.frame함수를 씌워서 다시 데이터 프레임형식으로 바꿔준다.
pima.scale <- data.frame(scale(Pima[, -8]))
str(pima.scale)
# 표준화가 되었으니 표준화 시킨데이터에 반응변수(type)을 포함시킨다.
pima.scale$type <- pima$type
head(pima.scale)
# 다시한번 melt함수에 넣어 박스플롯으로 본다.
pima.scale.melt <- melt(pima.scale, id.var = "type")
ggplot(data = pima.scale.melt, aes(x = type, y = value)) + geom_boxplot() +
         facet_wrap(~ variable, ncol = 2)
#표준화된 박스플롯그림들을 통해 glu, age 등이 type에 따라 다르다는 것을
#추측해볼 수 있다.

#이 데이터를 훈련세트와 테스트세트로 나누기 전에 cor()을 이용하여
#피처간의 상관관계를 보도록 하자
library(corrplot)       
hi <- cor(pima.scale[-8]) #type은 상관관계를 볼 필요가 없으니 뺀다.
corrplot.mixed(hi)
#그림으로 본 결과 npreg/age와 skin/bmi가 상관관계가 높다고 볼 수 있다.
# 이들의 다중 공선성을 확인해보도록 한다. (내가 개인적으로 한것.)
hello <- glm(type ~ npreg + skin + bmi + age, family = binomial, data = pima.scale)
library(car)
vif(hello) #확인 결과 모두 1점대로 문제 없다!

#그럼 이제 이 데이터를 훈련세트와 테스트세트로 나눠보도록 하자
#이때 반응값인 type가 고루고루 나뉘는걸 확인해줘야 한다.
#만약 어느 한쪽 데이터에 특정 type값이 너무 치우쳐 들어가면(ex 이경우 yes값)
#편향되게 훈련되게 됨으로 테스트 데이터로 돌려도 원하는 결과가 안나올 수 있다
table(pima.scale$type)
#비율이 2:1 정도다. 7:3(보통 이 비율)비율로 잘라서 훈련/테스트세트로 나누자

set.seed(502)
ind <- sample(2, nrow(pima.scale), replace = TRUE, prob = c(0.7, 0.3))
train <- pima.scale[ind == 1, ]
test <- pima.scale[ind == 2, ]
str(train)
str(test)
#훈련 세트와 테스트 세트가 잘 나뉘었으므로 이제부터 knn과 SVM 모형으로 
#분류해보고 그 모델들을 평가해볼 수 있다.

# knn 모형화
# knn에서는 k값을 설정하는것이 중요하다. k값은 몇개씩 묶을건지라고 생각하면
# 편하다.

grid1 <- expand.grid(.k = seq(2, 20, by = 1))

#매개변수 선택을 위해 caret 패키지의 trainControl함수의 교차검증법을 이용한다
control <- trainControl(method = "cv") #cv가 cross validation의 약자다.

set.seed(502)

knn.train <- train(type ~ ., data = train, method = "knn",
                   trControl = control, tuneGrid = grid1)
knn.train #출력 결과를 통해 최적의 k값이 15라고 나온다.
#이때 정확도와 함께 kappa값이 나오는데 kappa값은 카파 통계량을 의미하며 이는
#두 평가자가 관찰값을 분류할때 서로 도의하는 정도를 재는 척도로 쓰인다.
#이 값이 클수록 평가자들의 성능이 높은 것이고 가능한 최댓값은 1이다.

#knn으로 확인하고 성능을 검사해본다.
#knn(타입값을 제외한 훈련데이터, 타입값을 제외한 테스트데이터,
#훈련데이터의 타입, 적절한 k값)
knn.test <- knn(train[, -8], test[, -8], train[, 8], k = 19)

#혼동행렬을 만들고 이것으로 정확도와 카파 통계량을 계산해보자.
table(knn.test, test$type)
(78+27)/147 # no-no + yes+yes / 전체.. 즉, 제대로 한것 나누기 전체 = 정확도
#70퍼센트정도가 나온다.

#kappa계산
prob.agree <- (76+27) / 147 #정확도도
prob.chance <- ((76+27) / 147) * ((76+17) / 147)
prob.chance
kappa <- (prob.agree - prob.chance) / (1 - prob.chance)
kappa #카파 통계량 값이 0.46정도이다.
# k 값 || 동의의 강도
#< 0.2 || 약함
#< 0.4 || 약간 동의
#< 0.6 || 어느정도 동의
#< 0.8 || 상당히 동의
#< 1.0 || 매우 동의

#그러므로 이 경우 어느정도 동의 수준이다.

