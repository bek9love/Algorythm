스크립트= 메모장
install.packages('dplyr')
library(tidyverse)      
library(ggplot2)

## 개체별 비유곡선 그리고 추세선 추가
data <- read.csv("/home/rstudio/bek9love/milk_yield_curve_15094.csv", header = TRUE, sep = ",")
library(ggplot2)
p <- ggplot(data = data, aes(x = DIM, y = MY)) +
  geom_point() + geom_smooth(method = "loess", se = FALSE) +  # 선형 추세선 추가
  labs(x = "DIM (d)", y = "Milk Yield (kg)", title = "젖소의 착유일수별 산유량")
print(p)


## 머신러닝 기초 연습
install.packages("GGally")
install.packages('randomForest')
install.packages('kernlab')
library(GGally)

diris <- iris
p <- ggpairs(diris, title="iris Correlation",ggplot2::aes(colour=Species, alpha=0.5))
p

set.seed (50)

smp_size <- 0.75 * nrow(diris)
train_ind <- sample(nrow(diris), size = smp_size)

train <- diris[train_ind, ]
test <- diris[-train_ind, ]

library(nnet) # 다항로지스틱 회귀, 신경망 모델링
library(rpart) # 의사결정 나무 모델링
library(randomForest) # Random Forest 모델링
library(kernlab) # 서포트벡터 모델링

## ModelName <- 모델링기법 ( 결과값 ~ 영향인자1 + 영향인자2 ... , data = training 데이터셋)
## 검증값 <- predict(ModelName, newdata = test 데이터셋, type = "class")

# 다항 로지스틱 회귀
multi_logit_m <- multinom(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
                          data = train)
multi_logit_p <- predict(multi_logit_m, newdata = test, type = "class")

#Decision tree
rpart_m <- rpart(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
                 data = train)
rpart_p <- predict(rpart_m, newdata = test, type = "class")

#Random Forest
rf_m <- randomForest(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
                     data = train)
rf_p <- predict(rf_m, newdata = test, type = "class")

#Support Vector
svm_m <- ksvm(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
              data=train)
svm_p <- predict(svm_m, newdata=test)

# nnet (Deep Learning)
nnet_m <- nnet(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
               data=train, size = 3)
nnet_p <- predict(nnet_m, newdata=test, type = "class")

multi_logit_m

plot(rpart_m)
text(rpart_m)

## xtabs(~ 비교할결과 1 + 비교할 결과 2)

# 데이터 비교
xtabs(~multi_logit_p + test$Species)
xtabs(~rpart_p + test$Species)
xtabs(~rf_p + test$Species)
xtabs(~svm_p + test$Species)
xtabs(~nnet_p + test$Species)

