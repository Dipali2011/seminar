data <- read.csv("C:Users/Lenovo Ideapad 320/Documents/advertising.csv")
View(data)

#str
str(data)

#creating baseline model
table(data$Clicked.on.Ad)
500/1000
#head 
head(data)

#spliting data set
ind <- sample(2,nrow(data),replace =TRUE,prob=c(0.7,0.3) )
data1 <- data[ind==1,]
data2 <- data[ind==2,]


log <- glm(Clicked.on.Ad~ Age+Area.Income+Daily.Internet.Usage,data=data1,family=binomial)
summary(log)  

#Making predictions on Training set
pre <- predict(log,type="response")
pre

#y_pred_num <- ifelse(pred > 0.5, 1, 0)
#y_pred <- factor(y_pred_num, levels=c(0, 1))
#y_act <- testData$Class

s <- ifelse(pre>0.5,1,0)
s1 <- factor(s,levels=c(0,1))
s3 <- data1$Clicked.on.Ad
mean(s1==s3)#accuracy

