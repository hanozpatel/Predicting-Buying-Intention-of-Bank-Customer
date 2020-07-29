#Loading Data
BankData<- read.csv('bank-full.csv', stringsAsFactors = F, header = T)
ExpData<- read.csv('bank-full.csv', stringsAsFactors = T, header = T)

#Checking the dimension
dim(BankData)

#Checking Structure of the data
str(BankData)
str(ExpData)

#summary of the dataset
summary(BankData)

#Checking the Categories
levels(ExpData$job)
levels(ExpData$marital)
levels(ExpData$education)
levels(ExpData$default)
levels(ExpData$housing)
levels(ExpData$loan)
levels(ExpData$contact)
levels(ExpData$y)

#Visualizing the data
#1)Exploring the output the Marketing Campaign
library(ggplot2)
ggplot(data=ExpData, aes(x=y, fill="Navi blue"))+
  geom_bar()+
  xlab("Outcome Y")+
  ylab("Count")+
  ggtitle("Exploring the Output of the Campaign")

#2)Boxplot of Jobs categories and Bank balance 
ggplot(ExpData, aes(x=job,y=balance)) + 
  geom_boxplot(size=1.2)+
  ylim(-1000,10000)+
  xlab("Job")+
  ylab("Balance")+
  ggtitle("Boxplot of Variours Job Categories and Balance in Account")

#3)Subscription based on Education categories
ggplot(ExpData, aes(x=education,fill=y)) + 
  geom_bar(position = "dodge", colour="black")+
  xlab("EDUCATION")+
  ylab("COUNT")+ggtitle("Subscription Based on Eduction Categories")

#4)Duration of Call vs The Age of Teh Customer
#age vs Duration 
ggplot(data=ExpData, aes(x=age,y=duration,colour=duration))+
  geom_point()+
  xlab("AGE")+
  ylab("DURATION (in sec)")+ggtitle(" Duration of Call vs The Age of The Customer")



#5)Distribustion of balances by education
ggplot(ExpData, aes(x=education,y=balance, colour=y)) + 
  geom_boxplot(size=1.2)+
  ylim(-1000,10000)+
  xlab("Education")+
  ylab("Balance")+
  ggtitle("Distribution of Bank Balances By Eduaction")

#6) Scatter plt matrix
#install.packages("GGally")
library(GGally)
ggpairs(ExpData[, c(1,6,12,13,14,15)])


##### Data Preparation and Preprocessing

#Checking for missing values
sum(is.na(BankData))

#To check the collinearity among predictor variables
corrmat<-cor(ExpData)
#install.packages("corrplot")
library(corrplot)
corrplot(corrmat)
corrplot

# This code of chunk make additional columns for unknownn values 
#and tranformsthe character data into numeic format

BankData$job_unk <- ifelse(BankData$job == "unknown", 1, 0)
BankData$job <- as.numeric(as.factor(BankData$job))
BankData$marital <- as.numeric(as.factor(BankData$marital))
BankData$edu_unk <- ifelse(BankData$education == "unknown", 1, 0)
BankData$education <- as.numeric(as.factor(BankData$education))
BankData$default<- ifelse(BankData$default == "yes", 1, 0)
BankData$housing <- ifelse(BankData$housing== "yes", 1, 0)
BankData$loan<- ifelse(BankData$loan== "yes", 1, 0)
BankData$month <- as.numeric(as.factor(BankData$month))
BankData$cont_unk <- ifelse(BankData$contact == "unknown", 1, 0)
BankData$contact <- as.numeric(as.factor(BankData$contact))
BankData$pout_unk <- ifelse(BankData$poutcome == "unknown", 1, 0)
BankData$poutcome <- as.numeric(as.factor(BankData$poutcome))
BankData$y <- ifelse(BankData$y== "yes", 1, 0)

str(BankData)

#splitting the data into traing and test data 
library(caTools)
set.seed(100)
split=sample.split(BankData$y,SplitRatio = 0.75)
BankData_training = subset(BankData, split==T)
BankData_test = subset(BankData, split==F)


#feature scaling
BankData_training[,c(1,6,10,12,13:16)] = scale(BankData_training[,c(1,6,10,12,13:16)])
BankData_test[,c(1,6,10,12,13:16)] = scale(BankData_test[,c(1,6,10,12,13:16)] )
head(BankData_test)

#Data Mining Techniques and Implementation
#1)Logistic Regression

classifier = glm(formula = y~.,
                 family =binomial,
                 data= BankData_training)
summary(classifier)
#predicting the Test set results
Prob_pred = predict(classifier, type= 'response',
                    newdata= BankData_test[-17])
#head(Prob_pred)
y_pred= ifelse(Prob_pred>0.5,1,0)

#Creating Confusion  the  matrix
ConMatLog = table(BankData_test[,17],y_pred)
ConMatLog
 
#Accuracy
Acc_Log1<-sum(diag(ConMatLog))/sum(ConMatLog)
Acc_Log1

library(gmodels)
CrossTable(y_pred,BankData_test[,17],prop.chisq = F)

library(ROCR)
ROCRpredLogi <- prediction(Prob_pred,BankData_test$y)
ROCRperfLogi <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of Logistic Regression")

#2) KNN
library(class)
#Fitting K-NN to the training dataset and predicting  the Test set results
#k=1
y_pred_kNN1 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 1)
ConMatKNN1 = table(BankData_test[,17],y_pred_kNN1)
#Accuracy
Acc_KNN1<-sum(diag(ConMatKNN1))/sum(ConMatKNN1)
Acc_KNN1

#k=2
y_pred_kNN2 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 2)
ConMatKNN2 = table(BankData_test[,17],y_pred_kNN2)
#Accuracy
Acc_KNN2<-sum(diag(ConMatKNN2))/sum(ConMatKNN2)
Acc_KNN2

#k=3
y_pred_kNN3 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 3)
ConMatKNN3 = table(BankData_test[,17],y_pred_kNN3)
#Accuracy
Acc_KNN3<-sum(diag(ConMatKNN3))/sum(ConMatKNN3)
Acc_KNN3

#k=4
y_pred_kNN4 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 4)
ConMatKNN4 = table(BankData_test[,17],y_pred_kNN4)
ConMatKNN4
#Accuracy
Acc_KNN4<-sum(diag(ConMatKNN4))/sum(ConMatKNN4)
Acc_KNN4

#k=5
y_pred_kNN5 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 5)
ConMatKNN5 = table(BankData_test[,17],y_pred_kNN5)
ConMatKNN5
#Accuracy
#Accuracy
Acc_KNN5<-sum(diag(ConMatKNN5))/sum(ConMatKNN5)
Acc_KNN5

#k=6
y_pred_kNN6 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 6)
ConMatKNN6 = table(BankData_test[,17],y_pred_kNN6)
#Accuracy
Acc_KNN6<-sum(diag(ConMatKNN6))/sum(ConMatKNN6)
Acc_KNN6

#k=7
y_pred_kNN7 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 7)
ConMatKNN7 = table(BankData_test[,17],y_pred_kNN7)
#Accuracy
Acc_KNN7<-sum(diag(ConMatKNN7))/sum(ConMatKNN7)
Acc_KNN7

#k=8
y_pred_kNN8 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 8)
ConMatKNN8 = table(BankData_test[,17],y_pred_kNN8)
#Accuracy
Acc_KNN8<-sum(diag(ConMatKNN8))/sum(ConMatKNN8)
Acc_KNN8

#k=9
y_pred_kNN9 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 9)
ConMatKNN9 = table(BankData_test[,17],y_pred_kNN9)
#Accuracy
Acc_KNN9<-sum(diag(ConMatKNN9))/sum(ConMatKNN9)
Acc_KNN9

#k=10
y_pred_kNN10 = knn(train = BankData_training[,-17],
                  test= BankData_test[,-17],
                  cl=BankData_training[,17],
                  k= 10)
ConMatKNN10 = table(BankData_test[,17],y_pred_kNN10)
#Accuracy
Acc_KNN10<-sum(diag(ConMatKNN10))/sum(ConMatKNN10)
Acc_KNN10

ConMatKNN5
CrossTable(y_pred_kNN5,BankData_test[,17],prop.chisq = F)

#ROC knn
ROCRpredknn <- prediction(as.numeric(y_pred_kNN10),BankData_test$y)
ROCRperfknn <- performance(ROCRpredknn, 'tpr','fpr')
plot(ROCRperf, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of k-NN Algorithm")

#3)Decision tree###############################

library(caTools)
set.seed(100)
split=sample.split(ExpData$y,SplitRatio = 0.75)
CTdata2_training = subset(ExpData, split==T)
CTdata2_test = subset(ExpData, split==F)

library(rpart )
library(rpart.plot)
CT_model2<- rpart(formula = y~.,
                  data =CTdata2_training, method='class',maxdepth=5)
print(CT_model2)
rpart.plot(CT_model2,type = 1, extra = 1, split.font = 1, varlen = -10)
summary(CT_model2)
y_CT2<-predict(CT_model2,newdata= CTdata2_test[,-17], type ='class')

CMCT2= table(CTdata2_test[,17],y_CT2)
CMCT2

Acc_CT2<-sum(diag(CMCT2))/sum(CMCT2)
Acc_CT2
library(gmodels)
CrossTable(y_CT2,CTdata2_test[,17],prop.chisq = F)

#ROC--CT
ROCRpredCT1 <- prediction(as.numeric(y_CT2),BankData_test$y)
ROCRperfCT1<- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperfCT1, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of Decision tree Algorithm")

#4)SVM###################################
library(e1071)
SVM_Model1 = svm(formula = y~.,
                 data= BankData_training,
                 type='C-classification',
                 kernel='linear')
summary(SVM_Model1)
#predicting the Test set results
y_SVM1 = predict(SVM_Model1,newdata= BankData_test[-17])

#making the matrix
ConMatSVM1 = table(BankData_test[,17],y_SVM1)
ConMatSVM1
#Accuracy
Acc_SVM<-sum(diag(ConMatSVM1))/sum(ConMatSVM1)
Acc_SVM

#ROC SVM
ROCRpredSVM <- prediction(as.numeric(y_SVM1),BankData_test$y)
ROCRperfSVM <- performance(ROCRpredSVM, 'tpr','fpr')
plot(ROCRperfSVM, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of SVM Algorithm")

#5)RandomForest
#install.packages("randomForest")

library(caTools)
set.seed(100)
split=sample.split(ExpData$y,SplitRatio = 0.75)
RT_training = subset(ExpData, split==T)
RT_test = subset(ExpData, split==F)

library(randomForest)
RF <- randomForest(y ~ ., data = RT_training, ntree = 500,
                         mtry = 4, nodesize = 5, importance = TRUE)
RF
 
rf.pred <- predict(RF, RT_test[,-17])

#confusion Matrix
CMRT= table(RT_test[,17],rf.pred)
CMRT

Acc_RT<-sum(diag(CMRT))/sum(CMRT)
Acc_RT

ROCRpredRF <- prediction(as.numeric(rf.pred),RT_test[,17] )
ROCRperfRF <- performance(ROCRpredRF, 'tpr','fpr')
plot(ROCRperfRF, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of Random Forest Algorithm")


#Naive Bayes 

library(e1071)
#importing data
NB_data<-read.csv("bank-full.csv")

#encoding the target feature
NB_data$y<- factor(BankData$y, levels=c(0,1))

#Spliting data
library(caTools)
set.seed(100)
split=sample.split(NB_data$y,SplitRatio = 0.75)
NB_training = subset(NB_data, split==T)
NB_test = subset(NB_data, split==F)

NB1 =naiveBayes(x=NB_training[-17],
                y= NB_training[,17])
NB1

#summary(NB1)
#predicting the Test set results
NB_Pred_Training= predict(NB1,newdata= NB_training[,-17])
NB_Pred_Training

#predicting the Test set results
NB_Pred = predict(NB1,newdata= NB_test[,-17])
NB_Pred

#Creating Confusion  the  training matrix
ConMatNB_train = table(NB_training[,17],NB_Pred_Training)
ConMatNB_train

ROCRpredRF <- prediction(as.numeric(rf.pred),RT_test[,17] )
ROCRperfRF <- performance(ROCRpredRF, 'tpr','fpr')
plot(ROCRperfRF, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of Random Forest Algorithm")
#Creating Confusion  the  test matrix
ConMatNB = table(NB_test[,17],NB_Pred)
ConMatNB

#Accuracy test
Acc_NB<-sum(diag(ConMatNB))/sum(ConMatNB)
Acc_NB

#Accuracy train
Acc_NB_Train<-sum(diag(ConMatNB_train))/sum(ConMatNB_train)
Acc_NB_Train

#ROC for NB
ROCRpredNB <- prediction(as.numeric(NB_Pred),NB_test[,17] )
ROCRperfNB <- performance(ROCRpredNB, 'tpr','fpr')
plot(ROCRperfNB, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of Naive Bayes Algorithm")

#Nural nets
install.packages("neuralnet")
library(neuralnet)
nn<- neuralnet(y~.,
               data = BankData_training,
               linear.output = F, 
               hidden = 3)
nn
plot(nn, rep="best")
predictnn <- neuralnet::compute(nn,BankData_test[,-17])
pre_nn <- predict(nn,newdata= BankData_test[,-17])

y_pred_NN= ifelse(pre_nn>0.5,1,0)
#confusion Matrix
ConMAtNN = table(BankData_test[,17],y_pred_NN)
ConMAtNN

#Accuracy 
Acc_NN<-sum(diag(ConMAtNN))/sum(ConMAtNN)
Acc_NN
#ROC NN

ROCRpredNN <- prediction(as.numeric(y_pred_NN),BankData_test[,17] )
ROCRperfNN <- performance(ROCRpredNN, 'tpr','fpr')
plot(ROCRperfNN, colorize = F, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
title(main="ROC Curve Of Nural Nets ")