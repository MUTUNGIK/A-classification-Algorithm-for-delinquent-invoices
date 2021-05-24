getwd()
#importing data with only variaables to use in the dataset


Fdata = read.csv("RFINAL.csv",header = TRUE,stringsAsFactors = TRUE )

invoicedata=Fdata

#installing packages

library(caret)
library(RWeka)
library(rpart)
library(FSelector)
library(dplyr)
library(e1071)
library(klaR)
library(caTools)
library(ranger)
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



# GENERATING DECISION TREE MODEL



#setting the controlls to use in the decision tree model evaluation so as to obtain optimal performing model using the traincontrol function.

# setting bootstrapping as a means of model evaluation,resampling to be done 25 times



controls=trainControl(method="boot",number=25,selectionFunction = "oneSE",classProbs = TRUE)

decgrid=expand.grid(.C=c(0.1,0.25,0.5),.M=c(10:20))


#Using train to develop different decision tree models from the data and select the best performing model

dectreemodel = train(paymenstatus~CustomerLocation+Industry+Paymentterm+Product+CBR+CPI+Sector+Amount+Exchangerate+proportionofpreviouslydelayedinvoices,data =invoicedata,na.action =na.pass,method = "J48",metric="Accuracy",trControl = controls,tuneGrid = decgrid)

print(dectreemodel)

plot (dectreemodel)

summary(dectreemodel$finalModel)

dectreemodel$finalModel

# Setting control parameters for the random forest model

forestctrl = trainControl(method = "boot632",number = 25,selectionFunction = "oneSE")
fgrid = expand.grid(.mtry=c(1:10))

# Training the random forest model and estimating its performance by bootstrapping

forest = train(paymenstatus~CustomerLocation+Industry+Paymentterm+Product+CBR+CPI+Sector+Amount+Exchangerate+proportionofpreviouslydelayedinvoices,data =invoicedata,method="rf",na.action = na.omit,trControl=forestctrl,tuneGrid =fgrid)
plot(forest)
forest$finalModel
print(forest)





#Final selected random forset model was selected based on oneSE rule.

























