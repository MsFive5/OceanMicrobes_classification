library(caret)
library(ggplot2)
# step 1
setwd("/Users/xiaoxuwu/Documents/coursera/datasci/datasci_course_materials/assignment5")
data <- read.csv("seaflow_21min.csv",header=TRUE)
summary(data)
# step 2
inTrain = createDataPartition(data$pop, p = 1/2)[[1]]
training=data[inTrain,]
testing=data[-inTrain, ]
#step3
qplot(chl_small,pe, data=data,color=pop)
#step4
library(rpart)
fol <- formula(pop~fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
model <- rpart(fol, method="class", data=training)
print(model)
# step5
predicted <- predict(model, testing, type="class")
sum(testing$pop == predicted)/length(predicted)
# step6
library(randomForest)
model <- randomForest(fol, data=training)
predicted <- predict(model, testing, type="class")
sum(testing$pop == predicted)/length(predicted)
importance(model)
# step7
library(e1071)
model <- svm(fol, data=training)
predicted <- predict(model, testing, type="class")
sum(testing$pop == predicted)/length(predicted)
# step8
table(pred=predicted, true=testing$pop)
# step 9
plot(data$pop)
plot(data$fsc_small)
plot(data$chl_small)
plot(data$chl_big)
plot(data$fsc_perp)
plot(data$fsc_big)
qplot(data$time, data$chl_small)
# step10
data_san <-subset(data, file_id != 208)
inTrain = createDataPartition(data_san$pop, p = 1/2)[[1]]
training=data_san[inTrain,]
testing=data_san[-inTrain, ]
model <- svm(fol, data=training)
predicted <- predict(model, testing, type="class")
sum(testing$pop == predicted)/length(predicted)

