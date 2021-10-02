library(readxl)
install.packages("RSNNS")
period4data <- read_excel("C:/Users/aaron/projectdata/stockportfolioperformance.xlsx", 
                                        sheet = "4th period", col_types = c("skip", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric"))

period3data <- read_excel("C:/Users/aaron/projectdata/stockportfolioperformance.xlsx", 
                                        sheet = "3rd period", col_types = c("skip", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric"))

period2data <- read_excel("C:/Users/aaron/projectdata/stockportfolioperformance.xlsx", 
                          sheet = "2nd period", col_types = c("skip", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric"))

period1data <- read_excel("C:/Users/aaron/projectdata/stockportfolioperformance.xlsx", 
                          sheet = "1st period", col_types = c("skip", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric"))

allperioddata <- read_excel("C:/Users/aaron/projectdata/stockportfolioperformance.xlsx", 
                          sheet = "all period", col_types = c("skip", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric"))

xtrain<-data.matrix(period4data[,c(1,2,3,4,5,6)])
ytrain<-data.matrix(period4data[,c('Annual Return...7','Excess Return...8','Systematic Risk...9','Total Risk...10','Abs. Win Rate...11','Rel. Win Rate...12')])

xtest3<-data.matrix(period3data[,c(1,2,3,4,5,6)])
ytest3<-data.matrix(period3data[,c(13,14,15,16,17,18)])

xtest2<-data.matrix(period2data[,c(1,2,3,4,5,6)])
ytest2<-data.matrix(period4data[,c(13,14,15,16,17,18)])

xtest1<-data.matrix(period1data[,c(1,2,3,4,5,6)])
ytest1<-data.matrix(period4data[,c(13,14,15,16,17,18)])




library(RSNNS)
model <- mlp(xtrain, ytrain, size=6,learnFunc="Std_Backpropagation",learnFuncParams=c(0.2,0), initFunc='Randomize_Weights', initFuncParams=c(-0.3,0.3), hiddenActFunc='Act_Logistic',maxit=50)
summary(model)
model
extractNetInfo(model)
plotIterativeError(model)
predictions1 <- predict(model,xtest3)
colMeans(predictions1)
predictions2 <- predict(model,xtest2)
colMeans(predictions2)
predictions3 <- predict(model,xtest1)
colMeans(predictions3)

install.packages("e1071")
install.packages('Metrics')
library(e1071)
library(Metrics)
fit=svm(factor(ytrain[1:63]) ~ factor(xtrain[1:63]), data=period4data, scale=FALSE, kernel="radial",cost=1)
predictsvm <- predict(fit, xtrain)
predictsvm

tuneSVM <- tune(svm, factor(ytrain[1:63])~factor(xtrain[1:63]), data=period4data, ranges=list(epsilon=seq(0,1,0.1),cost=2^(seq(0.5,8,0.5))))
tunedVals <- tuneSVM$best.model

summary(tunedVals)
