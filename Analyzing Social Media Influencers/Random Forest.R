Train<-read.csv('Train.csv')
attach(Train)
colnames(data)
data=Train
Influencer<-ifelse(data[,1]==1,"A","B")
data<-cbind(data,Influencer)

colnames(data)
data1<-data[,c(2:24)]

#Ignore.These were other sets that achieved inferior results 

# data3<-data[,c(2:5,11:16,22,23,24)]
# colnames(data[,c(2:5,11:16,22,23,24)])
# data2<-data[,c(2:4,6,11:15,17,22,23,24)]
# colnames(data[,c(2:4,6,11:15,17,22,23,24)])
# data4<-data[,c(2,3,4,13,14,15,24)]
# colnames(data4)
# data5<-data[,c(2,4,5,13,15,16,24)]
# colnames(data5)
# 
# data6<-data[,c(2,4,13,15,24)]
# colnames(data6)
# data7<-data[,c(2:6,13:17,24)]
# data8<-data[,c(2,5,13,16,24)]
# 
# data9<-data[,c(2,4,6,13,15,17,24)]
#data2
#Sample Indexes

set.seed(3)
indexes = sample(1:nrow(data1), size=0.3*nrow(data1))

# Split data
test = data1[indexes,]
dim(test)  # 6 11
train = data1[-indexes,]
dim(train) # 26 11


library(randomForest)

fit <- randomForest(Influencer~.,   data=train,set.seed(1))
prediction<-predict(fit,newdata=test)
prediction
  
 
importance(fit)


colnames(test)

library(caret)

xtab <- table(prediction, test[,23])
confusionMatrix(xtab)

# Ignore
# DS3<-table(prediction, test[,13])
# confusionMatrix(DS3)

# DS2<-table(prediction, test[,13])
# confusionMatrix(DS2)
# 
# DS4<- table(prediction, test[,8])
# confusionMatrix(DS4)
# 
# DS6<- table(prediction, test[,5])
# confusionMatrix(DS6)
# 
# DS7<- table(prediction, test[,11])
# confusionMatrix(DS7)
# 
# DS8<- table(prediction, test[,5])
# confusionMatrix(DS8)
# 
# DS9<- table(prediction, test[,7])
# confusionMatrix(DS9)

F11
#11 features
#78.12%
NF2<-confusionMatrix(xtab)
NF2


D1CN<-confusionMatrix(prediction,test[,23])
D1CN

SocialMediaRF<-cbind(test,prediction)
write.csv(SocialMediaRF,file="SocialMediaRF.csv")
#77.64%
#This is Dataset 1 without removing any factors




confusionMatrix(xtab, prevalence = 0.25) 
library (ROCR);