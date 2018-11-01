## based on Credit Scoring in R
## German Credit Data dataset (Asincion 2007)
library(readr)
library(dplyr)
library(ROCR)
data_raw <- read_table2("data/german.data", col_names = FALSE)
View(data_raw)

data <- data_raw
data$X1 <- as.factor(data$X1)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.factor(data$X3)
data$X4 <- as.factor(data$X4)
data$X5 <- as.numeric(data$X5)
data$X6 <- as.factor(data$X6)
data$X7 <- as.factor(data$X7)
data$X8 <- as.numeric(data$X8)

cols <-
        c("X1",
          "X3",
          "X4",
          "X6",
          "X7",
          "X9",
          "X10",
          "X12",
          "X14",
          "X15",
          "X17",
          "X19",
          "X20", "X21")
colsnum <-
        c("X2",
          "X5",
          "X8",
          "X11",
          "X13",
          "X16",
          "X18")

data %>% mutate_at(cols, funs(factor(.))) -> data
data %>% mutate_at(colsnum, funs(as.numeric(.))) -> data
data %>% rename (good_bad = X21) -> data
# binning
data$amount <- as.factor(ifelse(data$X5<=2500,'0-2500',ifelse(data$X5<=5000,'2600-5000','5000+')))

# creating training and test sample

d = sort(sample(nrow(data), nrow(data)*.6))
#select training sample
train<-data[d,]
test<-data[-d,]
#train<-subset(train,select=-X21)

m<-glm(good_bad~.,data=train,family=binomial())
# for those interested in the step function one can use m<- step(m) for it I
# recommend against step due to well known issues with itchoosing the optimal
# variables out of sample

#score test data set
test$score<-predict(m,type='response',test)
pred<-prediction(test$score,test$good_bad)
perf <- performance(pred,"tpr","fpr")
plot(perf)
