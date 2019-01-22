train=read.csv("D:/STATISTIKA/S1-STATISTIKA/SEMESTER 7/DATA MINING/FP DATA MINING/insurance/datatrain.csv")
head(train)
test=read.csv("D:/STATISTIKA/S1-STATISTIKA/SEMESTER 7/DATA MINING/FP DATA MINING/insurance/datatest.csv")
head(test)
train$sex<-as.factor(train$sex)
train$smoker<-as.factor(train$smoker)
train$region<-as.factor(train$region)
train$log_charges=log(train$charges)
test$log_charges=log(test$charges)
#scatter plot
win.graph()
pairs(~age+sex+bmi+children+smoker+region+charges,data=data,
      main="scatterplot- train",col="darkgreen")


#_______________________________________________#
#Support vector machine-Regression

library(e1071)
#train
model.SVR1<-svm(log_charges~.,data=train,kernel="linear",cost=1,epsilon=0.1)
summary(model.SVR1)
res.SVR1= train$log_charges-predict(model.SVR1,newdata=train)
fit1=predict(model.SVR1,newdata=train)
win.graph()
abline(0,0)
plot(fit1,res.SVR1,ylab="Residuals",xlab="fit",
     main="residual plot",col="red")
#Test
pred.SVR1<- predict(model.SVR1,newdata=test)
err.SVR1<-test$log_charges-pred.SVR1
rmse.SVR1<-sqrt(mean((err.SVR1^2)))
rmse.SVR1
R2<-

#______________cost=10__________________

model.SVR10<-svm(log_charges~.,data=train,kernel="linear",cost=10,epsilon=0.1)
summary(model.SVR10)
res.SVR10= train$log_charges-predict(model.SVR10,newdata=train)
fit10=predict(model.SVR10,newdata=train)
win.graph()
abline(0,0)
plot(fit10,res.SVR10,ylab="Residuals",xlab="fit",
     main="residual plot",col="red")

#Test
pred.SVR10<- predict(model.SVR10,newdata=test)
err.SVR10<-test$log_charges-pred.SVR10
rmse.SVR10<-sqrt(mean((err.SVR10^2)))
rmse.SVR10

#_______________c=100____________________

model.SVR100<-svm(log_charges~.,data=train,kernel="linear",cost=100,epsilon=0.1)
summary(model.SVR100)
res.SVR100= train$log_charges-predict(model.SVR100,newdata=train)
fit100=predict(model.SVR100,newdata=train)
win.graph()
abline(0,0)
plot(fit100,res.SVR100,ylab="Residuals",xlab="fit",
     main="residual plot",col="blue")

#Test
pred.SVR100<- predict(model.SVR100,newdata=test)
err.SVR100<-test$log_charges-pred.SVR100
rmse.SVR100<-sqrt(mean((err.SVR100^2)))
rmse.SVR100

#_______________c=0,001____________________

model.SVR0<-svm(log_charges~.,data=train,kernel="linear",cost=0.001,epsilon=0.1)
summary(model.SVR0)
res.SVR0= train$log_charges-predict(model.SVR0,newdata=train)
fit0=predict(model.SVR0,newdata=train)
win.graph()
abline(0,0)
plot(fit0,res.SVR0,ylab="Residuals",xlab="fit",
     main="residual plot",col="red")

#Test
pred.SVR0<- predict(model.SVR0,newdata=test)
err.SVR0<-test$log_charges-pred.SVR0
rmse.SVR0<-sqrt(mean((err.SVR0^2)))
rmse.SVR0




#______________________________________________#
#regression
ins_model <- lm(log_charges ~ age + sex + bmi + children + smoker + region, data = train)
summary(ins_model)
win.graph()
plot(ins_model)

pred1 <- predict(ins_model, newdata = test)
err.reg<-test$log_charges-pred1
rmsereg<-sqrt(mean((err.reg^2)))
rmsereg
