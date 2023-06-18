setwd("E:/VIT/DS-LAB/COURSE PROJECT")
library(stats)
library(dplyr)
library(randomForest)
library(anytime)
library(xts)
library(Metrics)
library(e1071)
library(caret)

f = read.csv("Bitcoin.csv")
#print(summary(f))
str(f)

f$Date = as.Date(anytime(f$Date)) // CONVERTING THE DATES INTO DATE OBJECTS
f$Volume = gsub(',','',f$Volume) // REPLACING THE COMMAS WITH EMPTY STRING I.E REVOMING COMMAS
f$Market.Cap = gsub(',','',f$Market.Cap)
f$Volume <- NULL                 // DROPPING VOLUME COLUMN
f$Market.Cap = as.numeric(f$Market.Cap) // CONVERTING MARKET CAP INTO NUMERIC DATATYPE
head(f)

Train = xts(f[, -1], order.by=as.POSIXct(f$Date)) // EXCLUDING FIRST COLUMN AND ORDERING ALL DATASET ACCORDING TO SORTED DATES
head(Train)

set.seed(123) // USED FOR RANDOM SPLITTING
index = sample(2, nrow(Train),replace = TRUE,prob=c(0.7,0.3)) // SPLITTING DATA INTO TRAINING AND TESTING

Training = Train[index==1,]
Testing = Train[index==2,]

rfm = randomForest(Close~Open+High+Low+Market.Cap,data = Training)
l1 = lm(Close~Open+High+Low+Market.Cap,data=Training)
model = svm(Close~Open+High+Low+Market.Cap,data=Training,kernel="linear")

pred_rfm = predict(rfm, Testing)
Testing$Predicted_Value_rfm = pred_rfm

pred_lm = predict(l1, Testing)
Testing$Predicted_Value_lm = pred_lm

pred_svr = predict(model, Testing)
Testing$Predicted_Value_svr = pred_svr

View(Testing)


