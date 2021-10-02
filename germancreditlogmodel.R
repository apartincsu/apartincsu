data <- read.csv("C:/Users/Aaron/Downloads/GermanCredit.csv")
head(data)

#remove first column since it is only observation tally
data <- data[,-1]
head(data)

#view structure of dataset
str(data)

#calculate mean and median loan amounts
mean(data$AMOUNT)
median(data$AMOUNT)

#change categorical variables to factors
data$CHK_ACCT <- factor(data$CHK_ACCT)
F=c(3:9)
data[,F] <- data.frame(apply(data[F], 2, as.factor))
F=c(11:21)
data[,F] <- data.frame(apply(data[F], 2, as.factor))
F=c(23:30)
data[,F] <- data.frame(apply(data[F], 2, as.factor))
str(data)

#partition data into training and validation sets using 60/40 split
set.seed(2)
train.index <- sample(c(1:dim(data)[1]), dim(data)[1]*0.6)
train.df <- data[train.index,]
valid.df <- data[-train.index,]

#run logistic regression using all variables first
logit.reg <- glm(RESPONSE ~ ., data = train.df, family="binomial")
options(scipen=999)
summary(logit.reg)

#plot lift chart using predicted values
logit.reg.pred <- predict(logit.reg, valid.df, type="response")
library(gains)
gain <- gains(valid.df$RESPONSE, logit.reg.pred, groups=length(logit.reg.pred))
plot(c(0, gain$cume.pct.of.total*sum(valid.df$RESPONSE))~c(0,gain$cume.obs), xlab="# cases", ylab="Cumulative")
lines(c(0,sum(valid.df$RESPONSE))~c(0,dim(valid.df)[1]), lty=2)

#plot confusion matrix and accuracy of model
library(caret)
library(e1071)
confusionMatrix(as.factor(ifelse(logit.reg.pred>0.5, 1, 0)), as.factor(valid.df$RESPONSE))

#use stepAIC() function to try to improve the logistic regression model
library(MASS)
logit.reg.step <- stepAIC(logit.reg, trace=FALSE)
summary(logit.reg.step)

#plot lift chart for updated log. reg. model
logit.reg.pred <- predict(logit.reg.step, valid.df, type="response")
library(gains)
gain <- gains(valid.df$RESPONSE, logit.reg.pred, groups=length(logit.reg.pred))
plot(c(0, gain$cume.pct.of.total*sum(valid.df$RESPONSE))~c(0,gain$cume.obs), xlab="# cases", ylab="Cumulative")
lines(c(0,sum(valid.df$RESPONSE))~c(0,dim(valid.df)[1]), lty=2)

#updated confusion matrix and accuracy
confusionMatrix(as.factor(ifelse(logit.reg.pred>0.5, 1, 0)), as.factor(valid.df$RESPONSE))

#updated model provides ~ 1% increase in accuracy