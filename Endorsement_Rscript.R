library(MASS)
source("C:/Users/sony/Desktop/2016fall/STAT5330/Rnotes/ROC.R")
endorse <- read.csv("C:/Users/sony/Desktop/2016fall/STAT5330/Project1/endorsements.csv", header = TRUE)
summary(endorse)
set.seed(37)
t <- sample(nrow(endorse),30)
train<-endorse[t,]

# Logistic regression:
train.glm<-glm(won_primary ~ endorsement_points + percentage_endorsement_points + money_raised + percentage_of_money,data = endorse,family = binomial)
summary(train.glm)
train.glm2<-glm(won_primary ~ percentage_endorsement_points+percentage_of_money,data = endorse,family = binomial)
summary(train.glm2)

# Prediction£ºthrehold value .5
contrasts(endorse$won_primary)   #  no=0 yes=1
glm.prob<-predict(train.glm2,endorse,type="response")
glm.pred<-rep("No",109)
glm.pred[glm.prob > 0.5]="Yes"
table(glm.pred,endorse$won_primary)
# Accurary  figure out how many were correctly classified
AC_logit<- mean(endorse$won_primary == glm.pred)
AC_logit     

#  LDA:
train.lda<-lda(won_primary ~ percentage_endorsement_points + percentage_of_money, data = train)
train.lda
# prediction
lda.pred<-predict(train.lda,endorse)
table(lda.pred$class,endorse$won_primary)
# Accurary
AC_lda<-mean(endorse$won_primary == lda.pred$class)
AC_lda       

# QDA: 
train.qda<-qda(won_primary ~ percentage_endorsement_points + percentage_of_money, data = train)
train.qda
# prediction
qda.pred <- predict(train.qda,endorse)  
table(qda.pred$class,endorse$won_primary)
# Accurary
AC_qda<-mean(endorse$won_primary == qda.pred$class)
AC_qda  


#ROC curve: logistic,lad and qda
prediction<-lda.pred$posterior  
plot.roc(prediction[,2],endorse$won_primary,col="blue")
prediction2<-qda.pred$posterior 
lines.roc(prediction2[,2],endorse$won_primary,col = "green")
prediction3<-glm.prob
lines.roc(prediction3,endorse$won_primary,col="red")
legend(x=c("right"),y=c("center"), c("ROC from Logistic","ROC from LDA","ROC from QDA"),lty = c(1,1,1),lwd=c(2,2,2),col = c("red","blue","green"))


#five-fold cross validation
#calculate MSE = (yi-yhat)^2
x<-endorse$won_primary=='Yes'
x <- sapply(x, as.numeric)
endorse[,"x"]<-x  # convert yes no into 0,1
data<-endorse[,c(5,7,10)]
n <-nrow(endorse)
m<-5
len<-floor(n/m)   
#5-Fold Cross Validation lda
prd.err<-matrix(0,1,m)
prd.err2<-matrix(0,1,m)
prd.err3<-matrix(0,1,m)

for(k in 1:m){
  ind <- (len*k-len +1) : (len*k)     
  training<- data[-ind,]
  testing<-data[ind, ]
  
  fit<-lda(x~.,training)
  yhat<-predict(fit,testing)
  prd.err[k]<-mean((testing$x-yhat$posterior)^2)    # want to store the err value in the matrix created
  fit2<-qda(x~.,training)
  yhat2<-predict(fit2,testing)
  prd.err2[k]<-mean((testing$x-yhat2$posterior)^2) 
  
  fit3<-glm(x~.,training,family = "binomial")
  yhat3<-predict(fit3,testing,type="response")
  prd.err3[k]<-mean((testing$x-yhat3)^2)   
}
lda <- mean(prd.err)
qda <-mean(prd.err2)
log <- mean(prd.err3)



# classification tree
# Using two different impurity function
library(tree)
endorse.tree<-tree(won_primary~percentage_endorsement_points+percentage_of_money,endorse)
plot(endorse.tree)
text(endorse.tree)
summary(endorse.tree)

endorse.tree2<-tree(won_primary ~ percentage_endorsement_points+percentage_of_money ,endorse,split="gini")
plot(endorse.tree2)
text(endorse.tree2)
summary(endorse.tree2)

# nicer plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)
endorse.tree3<- rpart(won_primary~percentage_endorsement_points+percentage_of_money,endorse,method = "class")
endorse.tree3
fancyRpartPlot(endorse.tree3)  #using package rattle
printcp(endorse.tree3)   # error is the cross validation error, want it to be small to decide the max tree


# bagging, random forests and boosting
library(ipred)
p<-bagging(won_primary~percentage_endorsement_points+percentage_of_money ,endorse,coob=T)

library(randomForest)
p2<-randomForest(won_primary ~ percentage_endorsement_points+percentage_of_money,endorse, importance=T)

mypredict.rpart<-function(object,newdata){
  predict(object,newdata=newdata,type="class")
}
c(Tree = errorest(won_primary ~ percentage_endorsement_points+percentage_of_money,endorse,model = rpart, predict = mypredict.rpart)$error,
  Bagging = errorest(won_primary ~ percentage_endorsement_points+percentage_of_money,endorse,model = bagging)$error,
  Forest = errorest(won_primary ~ percentage_endorsement_points+percentage_of_money,endorse,model = randomForest)$error,
  Boosting = errorest(won_primary ~ percentage_endorsement_points+percentage_of_money,endorse,model = ada)$error)











