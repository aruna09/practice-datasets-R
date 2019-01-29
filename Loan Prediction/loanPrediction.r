# appending the "" values with NaN
train.df<-read.csv("train.csv", na.strings = c("","NaN"," "))
test.df<-read.csv("test.csv", na.strings = c("","NaN"," "))

summary(train.df)
summary(test.df)

# creating copy for train and test set
features_train<-train.df[,]# copy of train set
features_test<-test.df[,]# copy of test set

#making loanID null as it might be redundant
features_train$Loan_ID=NULL
names(features_train)
countNATrain<-sapply(features_train, function(x) sum(is.na(x)))
countNATest<-sapply(features_test, function(x) sum(is.na(x)))

# displays the total NaN values in train and test set
countNATrain
countNATest


# most of these have outlier so imputing with median instead of mean is a better idea.
# check from the plots below
hist(train.df$Loan_Amount_Term)
hist(train.df$Loan_Amount)
hist(train.df$ApplicantIncome)
hist(train.df$CoapplicantIncome)
hist(train.df$Credit_History) 

# imputing the training set
features_train$Loan_Amount_Term[is.na(features_train$Loan_Amount_Term)]<-360
features_train$LoanAmount[is.na(features_train$LoanAmount)]<-180
features_train$Credit_History[is.na(features_train$Credit_History)]<-1
features_train$Gender[is.na(features_train$Gender)]<-"Male"
features_train$Dependents[is.na(features_train$Dependents)]<-0
features_train$Self_Employed[is.na(features_train$Self_Employed)]<-"Yes"
features_train$Married[is.na(features_train$Married)]<-"Yes"
# check whether all the values have been correcctly imputed
summary(features_train)

# imputing in test set
mean(features_test$Loan_Amount_Term)
features_test$Loan_Amount_Term[is.na(features_test$Loan_Amount_Term)]<-m


LA<-features_test$LoanAmount
c<-0
sum<-0
for(i in LA){
  if(is.na(i)==FALSE){
    i
    sum<-sum+i
    c<-c+1
  }
}
mean_calculated<-sum/c
mean_calculated
features_test$LoanAmount[is.na(features_test$LoanAmount)]<-mean_calculated

CH<-features_test$Credit_History
c<-0
sum<-0
for(i in CH){
  if(is.na(i)==FALSE){
    i
    sum<-sum+i
    c<-c+1
  }
}
mean_calculated<-sum/c
mean_calculated
features_test$Credit_History[is.na(features_test$Credit_History)]<-mean_calculated

sum(is.na(features_train))
names(features_train)

## tried to fix the NA values
# DO NOT RUN THIS
features_train$TotalIncome <- log(clean_trainset$ApplicantIncome + clean_trainset$CoapplicantIncome)
clean_trainset$TotalIncomeLoanRatio = log(((clean_trainset$ApplicantIncome + clean_trainset$CoapplicantIncome)/clean_trainset$LoanAmount)*(as.numeric(
clean_trainset$Loan_Amount_Term)/360))
clean_trainset$LoanAmount <- log(clean_trainset$LoanAmount)
clean_trainset <- clean_trainset[,!(names(clean_trainset)) %in% c("ApplicantIncome","CoapplicantIncome")]

# final model making, training and prediction
first_model <- glm(features_train$Loan_Status~.,family = binomial,data = features_train)
predict<- predict(first_model, newdata=features_test, type="response")

# check whether the final prediction consists of null values, before making the CSV file 
sum(is.na(predict))

predict <- ifelse(predict > 0.5,"Y","N")
submit <- data.frame(Loan_ID = features_test$Loan_ID, Loan_Status = predict)
write.csv(submit,"result.csv",row.names = FALSE)