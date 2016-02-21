
#First read the data in
data = read.csv('train.csv')
#Convert IV to factor
data$Choice = factor(data$Choice)

par(mar=c(2, 2, 2, 2))

library(corrplot)
library(glmnet)
library(caTools)
library(caret)
#Correlation plot of all variables except IV
corrplot(cor(data[,-1]), order = "hclust", addrect = 5)
#Stratified splitting into test and train 70:30
set.seed(3)
train_rows = sample.split(data$Choice, SplitRatio=0.7)
train = data[ train_rows,]
test  = data[!train_rows,]
#First logistic model run with all variables as is
x <- subset(train, select = -c(Choice) )
xmat<-data.matrix(model.matrix(~.-1,x))
y<-data.matrix(train$Choice)
#Model Fit
cvfit = cv.glmnet(xmat, y, family = "binomial",alpha =1, type.measure = "class")

plot(cvfit, main = 'Misclassification Rate v/s Lambda Lasso')
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
#Predictions
pred_out = predict(cvfit, newx = xmat, s = "lambda.min", type = "class")
#CM
confusionMatrix(pred_out,y)


#Addition of new variables to identify interaction
train$ratio_Afollow = ifelse(train$A_following_count==0, 0, train$A_follower_count / train$A_following_count)
train$ratio_Amention = ifelse(train$A_mentions_sent==0, 0, train$A_mentions_received / train$A_mentions_sent)
train$ratio_ART = ifelse(train$A_retweets_sent==0, 0, train$A_retweets_received / train$A_retweets_sent)
train$ratio_Bfollow = ifelse(train$B_following_count==0, 0, train$B_follower_count / train$B_following_count)
train$ratio_Bmention = ifelse(train$B_mentions_sent==0, 0, train$B_mentions_received / train$B_mentions_sent)
train$ratio_BRT = ifelse(train$B_retweets_sent==0, 0, train$B_retweets_received / train$B_retweets_sent)

train$ratio_ABfollow = ifelse(train$B_follower_count==0, 0, train$A_follower_count / train$B_follower_count)
train$ratio_ABRT = ifelse(train$B_retweets_received==0, 0, train$A_retweets_received / train$B_retweets_received)
train$ratio_ABmention = ifelse(train$B_mentions_received==0, 0, train$A_mentions_received / train$B_mentions_received)

x <- subset(train, select = -c(Choice))
xmat <- data.matrix(model.matrix(~.-1,x))

cvfit = cv.glmnet(xmat, y, family = "binomial",alpha =1, type.measure = "class")
plot(cvfit, main = 'Misclassification Rate v/s Lambda Lasso')
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
pred_out = predict(cvfit, newx = xmat, s = "lambda.min", type = "class")
confusionMatrix(pred_out,y)


test$ratio_Afollow = ifelse(test$A_following_count==0, 0, test$A_follower_count / test$A_following_count)
test$ratio_Amention = ifelse(test$A_mentions_sent==0, 0, test$A_mentions_received / test$A_mentions_sent)
test$ratio_ART = ifelse(test$A_retweets_sent==0, 0, test$A_retweets_received / test$A_retweets_sent)
test$ratio_Bfollow = ifelse(test$B_following_count==0, 0, test$B_follower_count / test$B_following_count)
test$ratio_Bmention = ifelse(test$B_mentions_sent==0, 0, test$B_mentions_received / test$B_mentions_sent)
test$ratio_BRT = ifelse(test$B_retweets_sent==0, 0, test$B_retweets_received / test$B_retweets_sent)

test$ratio_ABfollow = ifelse(test$B_follower_count==0, 0, test$A_follower_count / test$B_follower_count)
test$ratio_ABRT = ifelse(test$B_retweets_received==0, 0, test$A_retweets_received / test$B_retweets_received)
test$ratio_ABmention = ifelse(test$B_mentions_received==0, 0, test$A_mentions_received / test$B_mentions_received)

xtest <- subset(test, select = -c(Choice))
xtestmat<-data.matrix(model.matrix(~.-1,xtest))
pred_out_test = predict(cvfit, newx = xtestmat, s = "lambda.min", type = "class")
ytest<-data.matrix(test$Choice)
confusionMatrix(pred_out_test,ytest)

xv <- subset(test, select = c(A_follower_count,B_follower_count, Choice))

xv1 <- cbind(xv,pred_out_test)

write.csv(xv1,file="SocialMediaLogistic.csv")
