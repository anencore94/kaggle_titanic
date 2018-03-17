# 20180305 - 20180311

# Intro. ----

# Set working directory and import datafiles  
setwd("C:/kaggle/01.titanic")
train <- read.csv("C:/kaggle/01.titanic/train.csv")
test<- read.csv("C:/kaggle/01.titanic/test.csv")

# See data
View(train)
View(test)
str(train)

# data type 중 num은 NA도 포함 가능한게 int랑 다른점
# By defalut, R imports all text string as factors

# dataframe의 column에 접근하려면 $ 사용
# table은 vector의 각 value의 빈도수를 알려줌
table(train$Survived)

# 퍼센트는 prop.table()
prop.table(table(train$Survived))

# just to break the ice, "everyone dies" prediction을 test에 입력시켜보자
# test라는 dataframe에는 Survived라는 column이 없는데 이렇게 쓰기만 해줘도 새로 생기고 0을 418번 대입

test$Survived <- rep(0,418)

# 그리고 test에서 PassengerId랑 Survived column만 뽑아서 새로 저장한다음 output file로 보내자.
# data.frame 함수는 new dataframe을 생성하는 함수
# write.csv 함수는 dataframe 을 CSV file로 보내는 함수, 그런데 row.names의 default가 row number를 부여하므로
# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "theyallperish.csv", row.names = FALSE)



# Part 2. ----
# Part 2 : The Gender-Class Model

# "women and children fist"
# 2.1 gender 분석 ----
summary(train$Sex)

prop.table(table(train$Sex, train$Survived)) # 전체 proportion
prop.table(table(train$Sex, train$Survived),1) # row-wise proportion
prop.table(table(train$Sex, train$Survived),2) # row-wise proportion
# 즉, 여자면 살 확률이 더 높다. 이를 이용해 test를 다시 만들어보자.

test$Survived <- 0  # test에 Survived column(열)을 만들고 다 0 대입
View(test$Survived)

test$Survived[test$Sex == "female"] <- 1  # [](square bracket)은 dataframe의 subset을 만드는 함수
View(test$Survived)
# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "femaleallalive.csv", row.names = FALSE)

# 2.2 Age 분석 ----
summary(train$Age)

# 간단하게 NA는 Mean age로 생각

# Child라는 variable을 도입하자.
train$Child <-   0
train$Child[train$Age < 18] <- 1  # Child이면 1
# 이때, NA는 모든 boolean test에 대해 FALSE값을 가지므로 Child 값은 0을 가짐(어른) = 우리가 원했던 것과 일치

# aggregate : subset으로 나누고 각 subset에서 원하는 variable의 원하는 function값 볼 수 있음
aggregate(Survived ~ Child+Sex, data = train, FUN = sum)
# 생존자면 1, 사망자는 0이었으므로 sum function에 의한 값이 곧 생존자 수

#우리가 원하는건 숫자가 아니라 비율이므로 다음과 같은 function 사용
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# test에 적용
test$Child <-   0
test$Child[test$Age < 18] <- 1
test$Survived[test$Sex== 'male' & test$Child==1] <- 1
# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "femaleandboyalive.csv", row.names = FALSE)
# female만 다 산건 76.5퍼, female+boy가 다 산것은 75.1퍼 => 더 안좋은 예측임

# 2.3 Fare&Pclass ----

#이제 요금 (Fare)와 좌석등급Pclass를 보자.
#먼저 요금variable을 더 구분하기 쉽게 등급을 나누자.
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#그 다음 Fare2, Pclass, Sex를 기준으로 분석해보면
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# female중에서 Fare 20이상 & Pclass 3인 경우는 female임에도 생존률이 현저히 떨어진다.
# 이에 대한 이유는 정확히 알 수 없지만 명확하게 그런 경향을 보이므로 이를 반영하자.
# perhaps expensive cabins were located close to the iceberg impact site, or further from exit stairs?

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "PclassandFare.csv", row.names = FALSE)




# Part 3. ----

# Part 3 : Decision Tree
# Greedy algorithm이 optimal은 당연히 아니지만, 간단하고, 소요시간이 적음

# rpart 패키지 for “Recursive Partitioning and Regression Trees” and uses the CART decision tree algorithm.
library(rpart)

#aggregate 함수와 비슷하게 rpart (?rpart참조), method = 'class'는 0 or 1로 분석하는 method
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)

# 그러나 이 plot은 한눈에 알아보기 어려우므로 다음 패키지를 사용
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)  
# 모양 진짜 이쁘고, 알아보기 쉽게 나옴. 그리고 Embarked, SibSp 등의 앞에서 안 썼던 variable도 활용해줌 


# predict 하나로 앞에서 했던 단순 반복작업 필요없이
# train data로 rpart fit한 걸 test에 적용하는 함수 predict(개편함, ?predict 참조) 
# fit 이라는 object를 test라는 dataframe에 씀
Prediction <- predict(fit, test, type = "class")
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# 3. extra ----
?rpart.control
# 여기서 알 수 있듯이 minsplit = 20, cp = 0.01로 default 되어있는데, 이걸 줄여서 더 정확도를 높여보자.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#그런데 이렇게하면 정확도가 더 떨어짐 => Why?? overfitting!!!
# overfitting은 sample에 불과한 train에 너무 맞춰져 있어서
# 진짜 예측해야하는 일반적인 다른 경우에 적용시킬 경우에 아까보다 더 정확도가 떨어짐
# rpart.control에서 다른 parameter를 더 변경시켜서 정확도 조금 더 높일 수는 있겠지만 
# 일단 넘어가서 다른 방법을 공부해보자.



# 4. Feature Engineering 
# Part 4 : Feature Engineering


# 'simple model with great features >> complicated algo. with poor features'
# engineered feature may be easier for a machine learning algo. than the original one.

# part 3 에서 무시했던 특성 중 name을 봐보자.
train$Name
train$Name[1]
View(train$Name[1])
# Master.는 unmarried boy, Countess는 백작부인

#?rbind
#m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
#m
#m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
#m


# Part 4의 방법을 사용하기 위해서 train, test를 다시 불러오자.
train <- read.csv("C:/kaggle/01.titanic/train.csv")
test<- read.csv("C:/kaggle/01.titanic/test.csv")
test$Survived <-NA
combi <-  rbind(train,test) # train 밑 열부터 열대로 test를 붙임 
View(combi)

# 글자는 r에서 자동으로 factor로 저장되어있으므로 이 타입을 text string으로 바꾸자.
combi$Name <-  as.character(combi$Name)
combi$Name[1]

# 이제 string으 쪼개보자.
strsplit(combi$Name[1], split='[,.]')

strsplit(combi$Name[1], split= '[,.]')[[1]]
strsplit(combi$Name[1], split= '[,.]')[[1]][2] # Mr, Mrs 등의 우리가 원하는 데이터 : Title이라 하자.

combi$Title <-  sapply(combi$Name, FUN = function(x){strsplit(x,split='[,.]')[[1]][2]})
View(combi$Title)
# title 앞에 공백이 항상 있는데 이걸 없애보자.
combi$Title <-  sub(' ', '', combi$Title)

table(combi$Title)

# 비슷한 title들을 묶자. (주관 개입)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <-  'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# 다시 string을 factor로 바꾸자.
combi$Title <- factor(combi$Title)


# familysize 가 크면 사망확률 높다는 가정
combi$FamilySize <-  combi$SibSp + combi$Parch +1 # Sibsp : sibling, spouse// Parch : parents, children

# 성이 같고 familysize까지 같은 경우가 있을 수 있으므로 이를 고려하기 위해 familyId를 만듦
combi$Surname <- sapply(combi$Name, FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "") # paste는 string끼리 붙여야 하므로 as.character
combi$FamilyID[combi$FamilySize<=2] <- 'small' # familysize 작은 것들은 다 small로
table(combi$FamilyID)

# 그런데 familyID에 나타난 familysize가 3이상인데 실제론 맞지 않는 경우들이 꽤 있음 =>이 오류를 처리
famIDs <- data.frame(table(combi$FamilyID))
View(famIDs)
famIDs <-  famIDs[famIDs$Freq <=2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <-  'small'
combi$FamilyID <-  factor(combi$FamilyID)

train <-combi[1:891,]
test <-  combi[892:1309,]
View(test)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

Prediction <- predict(fit, test, type = "class")
# submit <-data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "FeatureEngineering.csv", row.names = FALSE)



# 5. Random Forests ----

# Part 5 : Random Forests

# 5.1 NA 처리 ----
summary(combi$Age)

# Age : NA 인 case를 median으로 넣지 말고 rpart사용 fitting(anova는 continuous variable용)
Agefit <-  rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title +
                   FamilySize, data = combi[!is.na(combi$Age),],
                 method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi$Age) # NA 데이터 다 제거됨

# Embarked : 공백인 데이터를 majority인 S로 바꾸자.
summary(combi$Embarked)
which(combi$Embarked=='')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Fare
summary(combi$Fare)
# NA : 1명뿐
which(is.na(combi$Fare))
combi$Fare[1044] <-  median(combi$Fare,na.rm=TRUE)


# 5.2 Random forest 적용 위해 가공 ----
# Random forest in R can only digest factors with upto 32 levels. => FamilyID 때문에 안됨
# FamilyID2 도입

combi$FamilyID2 <-  combi$FamilyID
combi$FamilyID2 <-  as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize<=3] <-  'small'
combi$FamilyID2 <-  factor(combi$FamilyID2)

str(combi$FamilyID2)
# now 22 level.

## 5.3 Random forest package ----
# install.packages('randomForest')
library(randomForest)


# The number inside isn’t important, you just need to ensure you use the same seed number each time 
# so that the same random numbers are generated inside the Random Forest function.
set.seed(415)


train <-combi[1:891,]
test <-  combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

varImpPlot(fit)
# Accuracy, Gini 둘다 높은게 좋은거 : Title이 중요한 variable임을 알 수 있음

# submit
Prediction <- predict(fit, test)
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "firstforest.csv", row.names = FALSE)


# 6. Another model : Conditional inference trees ----

# install.packages('party')
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))
# mtry : random하게 뽑는 표본수 (default = sqrt(n))
# cforest는 familyID의 level이 23이상이어도 사용가능

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "conditional_inference.csv", row.names = FALSE)

