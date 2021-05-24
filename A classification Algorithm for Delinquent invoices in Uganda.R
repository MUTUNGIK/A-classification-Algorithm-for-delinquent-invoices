getwd()
#importing data with only variaables to use in the dataset
library(caret)
library(RWeka)
library(rpart)
library(FSelector)
library(dplyr)
library(e1071)

Fdata = read.csv("RFINAL.csv",header = TRUE,stringsAsFactors = TRUE )
invoicedata=Fdata
deal =read.csv("trial.csv",header =TRUE,stringsAsFactors = TRUE)
invtrain = invoicedata[1:1521,]
invtest =invoicedata[1522:2028,]
#Exploring the data structure
names(invoicedata)
str(invoicedata)

# Generating summaries and descriptive statistics

table(invoicedata$Industry)
table(invoicedata$Sector)
table(invoicedata$Product)
table(invoicedata$CustomerLocation)
table(invoicedata$paymenstatus)

summary(invoicedata$Amount)
summary(invoicedata$CPI)
summary(invoicedata$CBR)
summary(invoicedata$Exchangerate)
summary(invoicedata$proportionofpreviouslydelayedinvoices)
summary(invoicedata$Paymentterm)

# Generating Gainratios for attributes and ranking the top 75% to rank attribute importance

Gainratios= gain.ratio(paymenstatus~CustomerLocation+Industry+Paymentterm+Product+CBR+CPI+Sector+Amount+Exchangerate+proportionofpreviouslydelayedinvoices,data=invoicedata,unit="log2")
print(Gainratios)
subset = cutoff.k.percent(Gainratios,0.75)
print(subset)

# Generating a decision tree model

dectree = J48(paymenstatus~CustomerLocation+Industry+Product+CPI+CBR+proportionofpreviouslydelayedinvoices+Sector+Paymentterm+Amount+Exchangerate,data=invoicedata)
print(dectree)
summary(dectree)
tt =predict(dectree,invoicedata)
table(tt)
# optimising the decision tree model using k-cross validation method.

dectree1=train(paymenstatus~CustomerLocation+Industry+Product+proportionofpreviouslydelayedinvoices+Sector+Paymentterm+Amount+CPI+CBR+Exchangerate,data=invoicedata,method = "J48",na.action =na.pass)

print(dectree1)

#setting the controlls to use in the decision tree model evaaluation so as to obtain optimal performing model

# cross validation using 10 folds for cross validation
# instructing function to select based model 


controls=trainControl(method="cv",number=10)


#training and re-evaluating the model performance

finalmodel = train(paymenstatus~CustomerLocation+Industry+Product+proportionofpreviouslydelayedinvoices+Sector+Paymentterm+Amount+CPI+CBR+Exchangerate,data =invoicedata,na.action =na.pass,method = "J48", metric="Accuracy",trControl =controls)

print(finalmodel)

p= predict(finalmodel,deal,na.action =na.pass)

table(p)

confusionMatrix(p,invoicedata$paymenstatus,positive = "ontime")

kk =data.frame(invoicedata,p)





is.na(invoicedata$CustomerLocation)



