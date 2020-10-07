getwd()

#knn 모델 사용 과정
#1. 데이터 수집
#2. 데이터 정규화
#3. 훈련데이터랑 테스트데이터로 나누기
#4. 모델 트레이닝
#5. 모델 평가

#1 데이터 수집
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = TRUE) #여기서 true
#로 해줘야 이따가 B,M을 이름바꿀 수 있슴.
head(wbcd) # 잘나오는지, 어떻게 되있는지 확인
str(wbcd) #전체적으로 어떻게 구성되있는지 확인 569개행 31개열 diagnosis빼고
# 모두 numeric
wbcd <- wbcd[-1] #첫번째 열이 id인데 이건 아무의미 없는 변수이므로 제거한다.
table(wbcd$diagnosis) #타겟값의 분포 확인한다.
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
#사실 이부분은 안해줘도 되는데 확실히 보기 위해 값을 풀네임으로 바꿔준것
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
#타겟값 비율본거임.
#변수들의 값들이 다 제각각이므로 정규화과정을 거쳐야한다.
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#2 정규화
# 정규화 함수를 만들고 lapply로 함수를 적용시키면 정규화가 완성.
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_n <- as.data.frame(scale(wbcd[2:31]))
summary(wbcd_n$area_mean)#암거나 넣고 확인하면 정규화가 잘 되었음을 알 수 있다.

#3 훈련데이터, 테스트데이터로 나누기
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#타겟값은 따로 빼주어야한다. knn모델 돌리고 나중에 잘 된건지 비교할때 쓴다.
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
#각각 첫번째 열이 diagnosis 타겟값이므로 콤마뒤에 1이 온거다.
library(class)
#knn을 돌리기 위한 라이브러리
p <- knn(train, test, class, k)
#knn 돌리는 기본 문법 knn(훈련데이터, 테스트데이터, 타겟값, k값)
wbcd_test_pred <- knn(wbcd_train, wbcd_test, cl=wbcd_train_labels, k=21)
wbcd_test_pred

library(gmodels) #crosstable로 잘 훈련이 되었는지 아닌지 보기위한 라이브러리
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred)
