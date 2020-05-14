install.packages("dplyr")
install.packages (' arules' )
install.packages ('arulesViz' )
install.packages("rpart.plot")# install package rpart.plot
install.packages("caret")
install.packages("lattice")
install.packages("ROCR")
install.packages("ggplot2")
library("rpart") # load libraries
library("rpart.plot")
library('arules')
library('arulesViz')
library(hexbin)
library(ggplot2)
library(dplyr)
libary(DT)
library(data.table)
install.packages("el071") # install pac~age e1071
library(el071)
library(caTools)
library(caret)
library(ggplot2)
library(lattice)
library(ROCR)
#import data set
setwd("E:/TEJA/niuFALL19_SEM3/665/group_project")
loadData<-read.csv("telecomdata.csv")
#divide the data 80-20 into train and test 
trainData<-as.data.frame(loadData[1:6000,])
testData<-as.data.frame(loadData[6001:7043,])
#examine whether dataset loaded properly
head(loadData)
summary(loadData) #provides descriptive statistics
is.data.frame(loadData) #indicates if data frame was created by read.csv
# Amongst the variable stored in dataframe, 
# each variable is imported as a vector in dataframe except 
# the categorical variables imported as factor
#contingency_table<-table(loadData$gender,loadData$Churn)
#contingency_table
#head(as.data.frame(contingency_table))

ggplot(loadData)+geom_bar(aes(x = Churn,fill=Churn))+facet_wrap(~gender)

#cols<-c(CustomerChurn = "Yes", CustomerRetain = "No")
#plot1<-ggplot(loadData)+geom_col(aes(x=Churn,y=contingency_table["Yes"],fill=Churn))+scale_fill_manual(values = cols)+facet_wrap(~gender)
#exploratory= scatter plot between tenure and Monthly charges
plot(loadData$tenure, loadData$MonthlyCharges)
#exploratory = hexbin plot b/n tenure and monthly charges
d <- ggplot(loadData, aes(tenure, MonthlyCharges))
d + geom_hex()
# couldn't understand the relationship b/n the variables
#check the correlation factor
x<-cor(loadData$tenure,loadData$MonthlyCharges)
x


logistic_Regression<-glm(Churn~tenure+as.factor(Dependents)+MonthlyCharges+as.factor(PhoneService)+as.factor(MultipleLines)+as.factor(InternetService)+as.factor(OnlineSecurity)+as.factor(OnlineBackup)+as.factor(DeviceProtection)+as.factor(TechSupport)+as.factor(StreamingTV)+as.factor(StreamingMovies)+as.factor(Contract),data=loadData,family=binomial(link = "logit"))
summary(logistic_Regression)
head(loadData)
#fit linear regression model for gender, monthly charges, total charges, dependents
#check significance

head(trainData)
head(testData)
linearRegression <- lm(tenure~gender + MonthlyCharges
               + TotalCharges
               + Dependents,traindata)
summary(linearRegression)
confint(linearRegression, level = .95)
with(linearRegression , {
  plot(fitted.values, residuals,ylim=c(-60,40) )
  points(c(min(fitted.values) ,max(fitted.values)),
         c(O,O) , type = "1") })
par(mfrow=c(2,2)) 
plot(linearRegression)
#decision tree
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

decision_tree <- rpart(Churn~ SeniorCitizen+ Dependents +tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+
             Contract+StreamingTV, method="class",data=trainData,control=rpart.control(minsplit=1),parms=list(split='information'))
summary(decision_tree)
rpart.plot(decision_tree, type=4, extra=1)
rpart.plot(decision_tree, type=4, extra=2, clip.right.labs=FALSE,
           varlen=0)
newData<-testData[,c(1:20)]
View(newData)
predict_class<-predict(decision_tree,newdata=newData,type="class")
#roc for decision tree
t<-table(predictions=predict_class,actual=testData$Churn)
t
ratio<-sum(diag(t))/sum(t)
ratio
install.packages("pROC")
library(pROC)
predict_prob<-predict(decision_tree,testData, type="prob")
predict_prob
auc<-auc(testData$Churn,predict_prob[,2])
auc
plot(roc(testData$Churn,predict_prob[,2]))

#Naive
loadData <-read.csv("Churn.CSV")
traindata<-as.data.frame(loadData[1:6000,])
testData<-as.data.frame(loadData[6000:nrow(loadData),])
testData1<-as.data.frame(loadData[6001,])
#Naive Bayesian Method1
#Class Probability
tprior <- table(traindata$Churn)
tprior <- tprior/sum(tprior)
tprior
#Conditional Probablities
seniorCitizen <- table(traindata [, c ("Churn", "SeniorCitizen")])
seniorCitizen <- seniorCitizen/rowSums(seniorCitizen)
seniorCitizen
partner <- table(traindata [, c ("Churn", "Partner")])
partner <- partner/rowSums(partner)
partner
dependents <- table(traindata [, c ("Churn", "Dependents")])
dependents <- dependents/rowSums(dependents)
dependents
tenure <- table(traindata [, c ("Churn", "tenure")])
tenure <- tenure/rowSums(tenure)
tenure
phoneService <- table(traindata [, c ("Churn", "PhoneService")])
phoneService <- phoneService/rowSums(phoneService)
phoneService
multipleLines <- table(traindata [, c ("Churn", "MultipleLines")])
multipleLines <- multipleLines/rowSums(multipleLines)
multipleLines
internetService <- table(traindata [, c ("Churn", "InternetService")])
internetService <- internetService/rowSums(internetService)
internetService
onlineSecurity <- table(traindata [, c ("Churn", "OnlineSecurity")])
onlineSecurity <- onlineSecurity/rowSums(onlineSecurity)
onlineSecurity
onlineBackup <- table(traindata [, c ("Churn", "OnlineBackup")])
onlineBackup <- onlineBackup/rowSums(onlineBackup)
onlineBackup
deviceProtection <- table(traindata [, c ("Churn", "DeviceProtection")])
deviceProtection <- deviceProtection/rowSums(deviceProtection)
deviceProtection
techSupport <- table(traindata [, c ("Churn", "TechSupport")])
techSupport <- techSupport/rowSums(techSupport)
techSupport
streamingTV <- table(traindata [, c ("Churn", "StreamingTV")])
streamingTV <- streamingTV/rowSums(streamingTV)
streamingTV
streamingMovies <- table(traindata [, c ("Churn", "StreamingMovies")])
streamingMovies <- streamingMovies/rowSums(streamingMovies)
streamingMovies
contract <- table(traindata [, c ("Churn", "Contract")])
contract <- contract/rowSums(contract)
contract
paperlessBilling <- table(traindata [, c ("Churn", "PaperlessBilling")])
paperlessBilling <- paperlessBilling/rowSums(paperlessBilling)
paperlessBilling
paymentMethod <- table(traindata [, c ("Churn", "PaymentMethod")])
paymentMethod <- paymentMethod/rowSums(paymentMethod)
paymentMethod
monthlyCharges <- table(traindata [, c ("Churn", "MonthlyCharges")])
monthlyCharges <- monthlyCharges/rowSums(monthlyCharges)
monthlyCharges
#Posterior Probability
pyes<-
  seniorCitizen["Yes",testData[,c ("SeniorCitizen")]]
partner["Yes",testData[,c ("Partner")]]*
  dependents["Yes",testData[,c ("Dependents")]]*
  tenure["Yes",testData[,c ("tenure")]]*
  phoneService["Yes",testData[,c ("PhoneService")]]*
  multipleLines["Yes",testData[,c ("MultipleLines")]]*
  internetService["Yes",testData[,c ("InternetService")]]*
  onlineSecurity["Yes",testData[,c ("OnlineSecurity")]]*
  onlineBackup["Yes",testData[,c ("OnlineBackup")]]*
  deviceProtection["Yes",testData[,c ("DeviceProtection")]]*
  techSupport["Yes",testData[,c ("TechSupport")]]*
  streamingTV["Yes",testData[,c ("StreamingTV")]]*
  streamingMovies["Yes",testData[,c ("StreamingMovies")]]*
  contract["Yes",testData[,c ("Contract")]]*
  paperlessBilling["Yes",testData[,c ("PaperlessBilling")]]*
  paymentMethod["Yes",testData[,c ("PaymentMethod")]]*
  monthlyCharges["Yes",testData[,c ("MonthlyCharges")]]*
  tprior["Yes"]
pyes
pno<-
  seniorCitizen["No",testData[,c ("SeniorCitizen")]]*
  partner["No",testData[,c ("Partner")]]*
  dependents["No",testData[,c ("Dependents")]]*
  tenure["No",testData[,c ("tenure")]]*
  phoneService["No",testData[,c ("PhoneService")]]*
  multipleLines["No",testData[,c ("MultipleLines")]]*
  internetService["No",testData[,c ("InternetService")]]*
  onlineSecurity["No",testData[,c ("OnlineSecurity")]]*
  onlineBackup["No",testData[,c ("OnlineBackup")]]*
  deviceProtection["No",testData[,c ("DeviceProtection")]]*
  techSupport["No",testData[,c ("TechSupport")]]*
  streamingTV["No",testData[,c ("StreamingTV")]]*
  streamingMovies["No",testData[,c ("StreamingMovies")]]*
  contract["No",testData[,c ("Contract")]]*
  paperlessBilling["No",testData[,c ("PaperlessBilling")]]*
  paymentMethod["No",testData[,c ("PaymentMethod")]]*
  monthlyCharges["No",testData[,c ("MonthlyCharges")]]*
  tprior["No"]
pno
max(pyes,pno)
#Naive Bayesian Method2
model <- naiveBayes(Churn ~ SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingMovies+
                      Contract+StreamingTV+PaperlessBilling+PaymentMethod+MonthlyCharges,traindata)
model
results <- predict (model,testData)
results
#Validation
resultsCheck <- predict (model,testData1)
resultsCheck
head(testData1)
#AUC
nb_model <- naiveBayes(Churn~.,data=traindata)
nb_prediction <- predict(nb_model,testData[,-ncol(testData)],type='raw')
nb_prediction
score<- nb_prediction[, c("Yes")]
score
actual_class <- testData$Churn == 'Yes'
actual_class
pred <- prediction(score, actual_class)
perf <- performance (pred, "tpr" , "fpr" )
#auc < - performance (pred, "auc" )
auc <- performance(pred, measure = "auc") ######
auc <- unlist (slot (auc, "y.values" )) 
auc
plot(perf, lwd=2, xlab="False Positive Rate (FPR) ",
     ylab="True Positive Rate (TPR) ",main=paste("Area under the curve:", auc) )

abline (a=0, b=1, col="gray50" , lty=3 )
#Confusion matrix
Churnmodel.nb <- naiveBayes(Churn ~ ., data = traindata)

nb_train_predict <- predict(Churnmodel.nb, testData[ , names(testData) != "Churn"])

cfm <- confusionMatrix(nb_train_predict, testData$Churn)
cfm


 #association
association<-loadData[,c(7:16,21)]
View(association)

dtassociation <- data.table(association)

associationTrans<-as(dtassociation,'transactions')
rules <- apriori(associationTrans, parameter = list(support = 0.05, confidence = 0.7, minlen = 2, maxlen = 5),
appearance = list( rhs = c(Churn='No', Churn='Yes'), default = 'lhs' ),
control = list(verbose = FALSE))
                 #Myassociation.matrix<-as.matrix(association)
#transactions<-as(Myassociation.matrix,"transactions")
#read.transaction
summary( itemFrequency(associationTrans))
inspect(association)

#logistic regression removing correlated variables-1
logistic_Regression2<-glm(Churn~tenure+as.factor(Dependents)+MonthlyCharges+as.factor(PhoneService)+as.factor(InternetService)+as.factor(Contract),data=trainData,family=binomial(link = "logit"))
summary(logistic_Regression2)
#2nd way
logistic_Regression3<-glm(Churn~tenure+as.factor(Dependents)+MonthlyCharges+as.factor(MultipleLines)+as.factor(OnlineSecurity)+as.factor(OnlineBackup)+as.factor(DeviceProtection)+as.factor(TechSupport)+as.factor(StreamingTV)+as.factor(StreamingMovies)+as.factor(Contract),data=loadData,family=binomial(link = "logit"))
summary(logistic_Regression3)
#ROC for logistic regression
install.packages("gplots")
library(gplots)
install.packages("bitops")
library(bitops)
install.packages("caTools")
library(caTools)
install.packages("ROCR")
library(ROCR)

pred = predict(logistic_Regression2, type="response")
predObj = prediction(pred, testData$Churn)
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc") 
auc = aucObj@y.values[[1]]
auc
plot(rocObj, main = paste("Area under the curve:", auc))

