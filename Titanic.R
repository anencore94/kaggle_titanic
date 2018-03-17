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

# data type �� num�� NA�� ���� �����Ѱ� int�� �ٸ���
# By defalut, R imports all text string as factors

# dataframe�� column�� �����Ϸ��� $ ���
# table�� vector�� �� value�� �󵵼��� �˷���
table(train$Survived)

# �ۼ�Ʈ�� prop.table()
prop.table(table(train$Survived))

# just to break the ice, "everyone dies" prediction�� test�� �Է½��Ѻ���
# test��� dataframe���� Survived��� column�� ���µ� �̷��� ���⸸ ���൵ ���� ����� 0�� 418�� ����

test$Survived <- rep(0,418)

# �׸��� test���� PassengerId�� Survived column�� �̾Ƽ� ���� �����Ѵ��� output file�� ������.
# data.frame �Լ��� new dataframe�� �����ϴ� �Լ�
# write.csv �Լ��� dataframe �� CSV file�� ������ �Լ�, �׷��� row.names�� default�� row number�� �ο��ϹǷ�
# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "theyallperish.csv", row.names = FALSE)



# Part 2. ----
# Part 2 : The Gender-Class Model

# "women and children fist"
# 2.1 gender �м� ----
summary(train$Sex)

prop.table(table(train$Sex, train$Survived)) # ��ü proportion
prop.table(table(train$Sex, train$Survived),1) # row-wise proportion
prop.table(table(train$Sex, train$Survived),2) # row-wise proportion
# ��, ���ڸ� �� Ȯ���� �� ����. �̸� �̿��� test�� �ٽ� ������.

test$Survived <- 0  # test�� Survived column(��)�� ����� �� 0 ����
View(test$Survived)

test$Survived[test$Sex == "female"] <- 1  # [](square bracket)�� dataframe�� subset�� ����� �Լ�
View(test$Survived)
# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "femaleallalive.csv", row.names = FALSE)

# 2.2 Age �м� ----
summary(train$Age)

# �����ϰ� NA�� Mean age�� ����

# Child��� variable�� ��������.
train$Child <-   0
train$Child[train$Age < 18] <- 1  # Child�̸� 1
# �̶�, NA�� ��� boolean test�� ���� FALSE���� �����Ƿ� Child ���� 0�� ����(�) = �츮�� ���ߴ� �Ͱ� ��ġ

# aggregate : subset���� ������ �� subset���� ���ϴ� variable�� ���ϴ� function�� �� �� ����
aggregate(Survived ~ Child+Sex, data = train, FUN = sum)
# �����ڸ� 1, ����ڴ� 0�̾����Ƿ� sum function�� ���� ���� �� ������ ��

#�츮�� ���ϴ°� ���ڰ� �ƴ϶� �����̹Ƿ� ������ ���� function ���
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# test�� ����
test$Child <-   0
test$Child[test$Age < 18] <- 1
test$Survived[test$Sex== 'male' & test$Child==1] <- 1
# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "femaleandboyalive.csv", row.names = FALSE)
# female�� �� ��� 76.5��, female+boy�� �� ����� 75.1�� => �� ������ ������

# 2.3 Fare&Pclass ----

#���� ��� (Fare)�� �¼����Pclass�� ����.
#���� ���variable�� �� �����ϱ� ���� ����� ������.
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#�� ���� Fare2, Pclass, Sex�� �������� �м��غ���
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# female�߿��� Fare 20�̻� & Pclass 3�� ���� female�ӿ��� �������� ������ ��������.
# �̿� ���� ������ ��Ȯ�� �� �� ������ ��Ȯ�ϰ� �׷� ������ ���̹Ƿ� �̸� �ݿ�����.
# perhaps expensive cabins were located close to the iceberg impact site, or further from exit stairs?

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# submit <-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submit, file = "PclassandFare.csv", row.names = FALSE)




# Part 3. ----

# Part 3 : Decision Tree
# Greedy algorithm�� optimal�� �翬�� �ƴ�����, �����ϰ�, �ҿ�ð��� ����

# rpart ��Ű�� for ��Recursive Partitioning and Regression Trees�� and uses the CART decision tree algorithm.
library(rpart)

#aggregate �Լ��� ����ϰ� rpart (?rpart����), method = 'class'�� 0 or 1�� �м��ϴ� method
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)

# �׷��� �� plot�� �Ѵ��� �˾ƺ��� �����Ƿ� ���� ��Ű���� ���
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)  
# ��� ��¥ �̻ڰ�, �˾ƺ��� ���� ����. �׸��� Embarked, SibSp ���� �տ��� �� ��� variable�� Ȱ������ 


# predict �ϳ��� �տ��� �ߴ� �ܼ� �ݺ��۾� �ʿ����
# train data�� rpart fit�� �� test�� �����ϴ� �Լ� predict(������, ?predict ����) 
# fit �̶�� object�� test��� dataframe�� ��
Prediction <- predict(fit, test, type = "class")
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# 3. extra ----
?rpart.control
# ���⼭ �� �� �ֵ��� minsplit = 20, cp = 0.01�� default �Ǿ��ִµ�, �̰� �ٿ��� �� ��Ȯ���� ��������.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#�׷��� �̷����ϸ� ��Ȯ���� �� ������ => Why?? overfitting!!!
# overfitting�� sample�� �Ұ��� train�� �ʹ� ������ �־
# ��¥ �����ؾ��ϴ� �Ϲ����� �ٸ� ��쿡 �����ų ��쿡 �Ʊ�� �� ��Ȯ���� ������
# rpart.control���� �ٸ� parameter�� �� ������Ѽ� ��Ȯ�� ���� �� ���� ���� �ְ����� 
# �ϴ� �Ѿ�� �ٸ� ����� �����غ���.



# 4. Feature Engineering 
# Part 4 : Feature Engineering


# 'simple model with great features >> complicated algo. with poor features'
# engineered feature may be easier for a machine learning algo. than the original one.

# part 3 ���� �����ߴ� Ư�� �� name�� ������.
train$Name
train$Name[1]
View(train$Name[1])
# Master.�� unmarried boy, Countess�� ���ۺ���

#?rbind
#m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
#m
#m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
#m


# Part 4�� ����� ����ϱ� ���ؼ� train, test�� �ٽ� �ҷ�����.
train <- read.csv("C:/kaggle/01.titanic/train.csv")
test<- read.csv("C:/kaggle/01.titanic/test.csv")
test$Survived <-NA
combi <-  rbind(train,test) # train �� ������ ����� test�� ���� 
View(combi)

# ���ڴ� r���� �ڵ����� factor�� ����Ǿ������Ƿ� �� Ÿ���� text string���� �ٲ���.
combi$Name <-  as.character(combi$Name)
combi$Name[1]

# ���� string�� �ɰ�����.
strsplit(combi$Name[1], split='[,.]')

strsplit(combi$Name[1], split= '[,.]')[[1]]
strsplit(combi$Name[1], split= '[,.]')[[1]][2] # Mr, Mrs ���� �츮�� ���ϴ� ������ : Title�̶� ����.

combi$Title <-  sapply(combi$Name, FUN = function(x){strsplit(x,split='[,.]')[[1]][2]})
View(combi$Title)
# title �տ� ������ �׻� �ִµ� �̰� ���ֺ���.
combi$Title <-  sub(' ', '', combi$Title)

table(combi$Title)

# ����� title���� ����. (�ְ� ����)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <-  'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# �ٽ� string�� factor�� �ٲ���.
combi$Title <- factor(combi$Title)


# familysize �� ũ�� ���Ȯ�� ���ٴ� ����
combi$FamilySize <-  combi$SibSp + combi$Parch +1 # Sibsp : sibling, spouse// Parch : parents, children

# ���� ���� familysize���� ���� ��찡 ���� �� �����Ƿ� �̸� �����ϱ� ���� familyId�� ����
combi$Surname <- sapply(combi$Name, FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "") # paste�� string���� �ٿ��� �ϹǷ� as.character
combi$FamilyID[combi$FamilySize<=2] <- 'small' # familysize ���� �͵��� �� small��
table(combi$FamilyID)

# �׷��� familyID�� ��Ÿ�� familysize�� 3�̻��ε� ������ ���� �ʴ� ������ �� ���� =>�� ������ ó��
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

# 5.1 NA ó�� ----
summary(combi$Age)

# Age : NA �� case�� median���� ���� ���� rpart��� fitting(anova�� continuous variable��)
Agefit <-  rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title +
                   FamilySize, data = combi[!is.na(combi$Age),],
                 method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi$Age) # NA ������ �� ���ŵ�

# Embarked : ������ �����͸� majority�� S�� �ٲ���.
summary(combi$Embarked)
which(combi$Embarked=='')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Fare
summary(combi$Fare)
# NA : 1����
which(is.na(combi$Fare))
combi$Fare[1044] <-  median(combi$Fare,na.rm=TRUE)


# 5.2 Random forest ���� ���� ���� ----
# Random forest in R can only digest factors with upto 32 levels. => FamilyID ������ �ȵ�
# FamilyID2 ����

combi$FamilyID2 <-  combi$FamilyID
combi$FamilyID2 <-  as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize<=3] <-  'small'
combi$FamilyID2 <-  factor(combi$FamilyID2)

str(combi$FamilyID2)
# now 22 level.

## 5.3 Random forest package ----
# install.packages('randomForest')
library(randomForest)


# The number inside isn��t important, you just need to ensure you use the same seed number each time 
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
# Accuracy, Gini �Ѵ� ������ ������ : Title�� �߿��� variable���� �� �� ����

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
# mtry : random�ϰ� �̴� ǥ���� (default = sqrt(n))
# cforest�� familyID�� level�� 23�̻��̾ ��밡��

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "conditional_inference.csv", row.names = FALSE)
