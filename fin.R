install.packages("ridge")
install.packages("glmnet")
library(glmnet)
library(ridge)
censusdata<-read.csv("acs2015_county_data.csv")
census<-na.omit(censusdata)
PR<-which(census$State=="Puerto Rico")
census<-census[,-c(1,2,3)]
census<-census[-PR,]
set.seed(14)
test<-sample(1:3140,ceiling(0.25*3140),replace=FALSE)
x.test<-census[test,]
x.test<-as.matrix(x.test)
y.test<-census$Income[test]
x.train<-census[-test,]
x.train<-as.data.frame(x.train)
y.train<-census$Income[-test]

x.train<-x.train[,-11]
x.test<-x.test[,-11]#drop income

x.train<-x.train[,-3]
x.test<-x.test[,-3]#drop women
x.train<-as.matrix(x.train)

mod1<-lm(census$Income~.,data = census)
plot(mod1)
mod2<-lm(sqrt(Income)~.,data=census)
plot(mod2)

alias(mod2)

census<-census[,-3] #drop women
plot(lm(sqrt(Income)~.,data=census))

#Lasso, ridge, elastic net
fit.lasso<-glmnet(x.train,y.train,family='gaussian',alpha=1)
fit.ridge<-glmnet(x.train,y.train,family='gaussian',alpha=0)
fit.elnet<-glmnet(x.train,y.train,family='gaussian',alpha=0.5)

for (i in 0:10) {
  assign(paste('fit',i,sep=''),cv.glmnet(x.train,y.train,type.measure='mse',alpha=i/10,family='gaussian'))
}

par(mfrow=c(3,2))

plot(fit.lasso,xvar='lambda')
plot(fit10,main='LASSO')

plot(fit.ridge,xvar='lambda')
plot(fit0,main='RIDGE')

plot(fit.elnet,xvar='lambda')
plot(fit5,main='Elastic Net')

yhat1<-predict(fit1,s=fit1$lambda.1se,newx=x.test)
yhat2<-predict(fit2,s=fit2$lambda.1se,newx=x.test)
yhat3<-predict(fit3,s=fit3$lambda.1se,newx=x.test)
yhat4<-predict(fit4,s=fit4$lambda.1se,newx=x.test)
yhat5<-predict(fit5,s=fit5$lambda.1se,newx=x.test)
yhat6<-predict(fit6,s=fit6$lambda.1se,newx=x.test)
yhat7<-predict(fit7,s=fit7$lambda.1se,newx=x.test)
yhat8<-predict(fit8,s=fit8$lambda.1se,newx=x.test)
yhat9<-predict(fit9,s=fit9$lambda.1se,newx=x.test)
yhat10<-predict(fit10,s=fit10$lambda.1se,newx=x.test)

(mse0<-mean((y.test-yhat0)^2))
(mse1<-mean((y.test-yhat1)^2))
(mse2<-mean((y.test-yhat2)^2))
(mse3<-mean((y.test-yhat3)^2))
(mse4<-mean((y.test-yhat4)^2))
(mse5<-mean((y.test-yhat5)^2))
(mse6<-mean((y.test-yhat6)^2))
(mse7<-mean((y.test-yhat7)^2))
(mse8<-mean((y.test-yhat8)^2))
(mse9<-mean((y.test-yhat9)^2))
(mse10<-mean((y.test-yhat10)^2))

(mean(abs(y.test-yhat0)))
(mean(abs(y.test-yhat1)))
(mean(abs(y.test-yhat2)))
(mean(abs(y.test-yhat3)))
(mean(abs(y.test-yhat4)))
(mean(abs(y.test-yhat5)))
(mean(abs(y.test-yhat6)))
(mean(abs(y.test-yhat7)))
(mean(abs(y.test-yhat8)))
(mean(abs(y.test-yhat9)))
(mean(abs(y.test-yhat10)))
  
  
######################Logistic Regression
cen<-read.csv("acs2015_county_data.csv")
cendata<-na.omit(cen)
cendata$unemploy<-ifelse(cendata$Unemployment>=5.7,cendata$unemploy<-1,cendata$unemploy<-0)
PR<-which(cendata$State=="Puerto Rico")
cendata<-cendata[-PR,]
cendata<-cendata[,-c(1,2,3,6,37)] #get rid of ID,state, county, unemployment women
set.seed(14)
test<-sample(1:3140,ceiling(0.25*3140),replace=FALSE)
x.test2<-cendata[test,]
x.test2<-as.matrix(x.test2)
y.test2<-cendata$unemploy[test]
x.train2<-cendata[-test,]
x.train2<-as.matrix(x.train2)
y.train2<-cendata$unemploy[-test]

x.train2<-x.train2[,-33]
x.test2<-x.test2[,-33]#drop unemploy

fit.lasso2<-glmnet(x.train2,y.train2,family='binomial',alpha=1)
fit.ridge2<-glmnet(x.train2,y.train2,family="binomial",alpha=0)
fit.elnet2<-glmnet(x.train2,y.train2,family='binomial',alpha=0.5)

for (i in 0:10) {
  assign(paste('fit',i,sep=''),cv.glmnet(x.train2,y.train2,type.measure='mse',alpha=i/10,family='binomial'))
}

par(mfrow=c(3,2))

plot(fit.lasso2,xvar='lambda')
plot(fit10,main='LASSO')

plot(fit.ridge2,xvar='lambda')
plot(fit0,main='RIDGE')

plot(fit.elnet2,xvar='lambda')
plot(fit5,main='Elastic Net')

yhat0.2<-predict(fit0,s=fit0$lambda.1se,newx=x.test2)
yhat1.2<-predict(fit1,s=fit1$lambda.1se,newx=x.test2)
yhat2.2<-predict(fit2,s=fit2$lambda.1se,newx=x.test2)
yhat3.2<-predict(fit3,s=fit3$lambda.1se,newx=x.test2)
yhat4.2<-predict(fit4,s=fit4$lambda.1se,newx=x.test2)
yhat5.2<-predict(fit5,s=fit5$lambda.1se,newx=x.test2)
yhat6.2<-predict(fit6,s=fit6$lambda.1se,newx=x.test2)
yhat7.2<-predict(fit7,s=fit7$lambda.1se,newx=x.test2)
yhat8.2<-predict(fit8,s=fit8$lambda.1se,newx=x.test2)
yhat9.2<-predict(fit9,s=fit9$lambda.1se,newx=x.test2)
yhat10.2<-predict(fit10,s=fit10$lambda.1se,newx=x.test2)

(mse0.2<-mean((y.test2-yhat0.2)^2))
(mse1.2<-mean((y.test2-yhat1.2)^2))
(mse2.2<-mean((y.test2-yhat2.2)^2))
(mse3.2<-mean((y.test2-yhat3.2)^2))
(mse4.2<-mean((y.test2-yhat4.2)^2))
(mse5.2<-mean((y.test2-yhat5.2)^2))
(mse6.2<-mean((y.test2-yhat6.2)^2))
(mse7.2<-mean((y.test2-yhat7.2)^2))
(mse8.2<-mean((y.test2-yhat8.2)^2))
(mse9.2<-mean((y.test2-yhat9.2)^2))
(mse10.2<-mean((y.test2-yhat10.2)^2))


install.packages("AUC")
library(AUC)

roc.res0=roc(yhat0.2,factor(y.test2))
plot(roc.res0, main='ROC for RIDGE')
auc(roc.res0)

roc.res5=roc(yhat5.2,factor(y.test2))
plot(roc.res5, main='ROC for ELASTIC NET')
auc(roc.res5)

roc.res10=roc(yhat10.2,factor(y.test2))
plot(roc.res10, main='ROC for LASSO')
auc(roc.res10)

roc.res0=roc(yhat0.2,factor(y.test2))   #Ridge
auc(roc.res0)
roc.res5=roc(yhat5.2,factor(y.test2))   #Elnet
auc(roc.res5)
roc.res10=roc(yhat10.2,factor(y.test2)) #LASSO
auc(roc.res10)

coef(fit10)

yhat10.2<-predict(fit10,s=fit10$lambda.1se,newx=x.test2)







