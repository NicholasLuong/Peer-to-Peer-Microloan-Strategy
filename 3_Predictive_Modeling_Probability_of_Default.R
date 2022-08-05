### Week 3 - Predicting the Default Probability

# This script carries out the following steps
# 1. Read the orignal data including all the fields added by Lending Club
#   to predict default probability
# 2. Redo the prediction using only the fields available in our saved file
# 3. Start with predicting default using only signals such as the grade or
#   interest assigned to a loan by Lending Club
# 4. Redo the analysis only with variables available at the time of loan
#   application


## Step 1 - Load the raw data again and predict default from all available fields

# Skip the first line of the file (the header is contained in the second line)
loans.full <- read.csv("LoanStats3a_securev1.csv", skip=1L)


columns_to_pick <- c('id','loan_amnt','funded_amnt','funded_amnt_inv','term','int_rate',
                     'installment','grade','sub_grade','emp_title','emp_length',
                     'home_ownership','annual_inc','verification_status','issue_d',
                     'loan_status','purpose','title','zip_code','addr_state','dti','total_pymnt',
                     'delinq_2yrs','earliest_cr_line','open_acc','pub_rec','last_pymnt_d',
                     'last_pymnt_amnt','fico_range_high','fico_range_low','last_fico_range_high',
                     'last_fico_range_low','application_type','revol_bal','revol_util','recoveries')

# Only keep a subset of the columns for analysis
loans.full <- loans.full[,columns_to_pick]
summary(loans.full)
nrow(loans.full)


drop_na_columns <- c('annual_inc','loan_status','issue_d','last_pymnt_d','loan_amnt',
                    'int_rate','earliest_cr_line','open_acc','pub_rec','delinq_2yrs','recoveries',
                    'grade','fico_range_high','fico_range_low','installment', 'last_fico_range_high',
                    'last_fico_range_low','funded_amnt','dti','funded_amnt_inv','revol_bal','revol_util')

# Identify the type of each of these columns
float_columns <- c('loan_amnt', 'funded_amnt', 'installment', 'annual_inc',
                   'dti', 'revol_bal', 'delinq_2yrs', 'open_acc', 'pub_rec',
                   'fico_range_high', 'fico_range_low','last_fico_range_low',
                   'last_fico_range_high','total_pymnt', 'recoveries')

categorical_columns <- c('term', 'grade', 'emp_length', 'home_ownership',
                         'verification_status', 'loan_status', 'purpose')

percentage_columns <- c('int_rate', 'revol_util')

date_columns <- c('issue_d', 'earliest_cr_line', 'last_pymnt_d')

# Re-clean the data a bit
loans.full[,float_columns] <- sapply(loans.full[,float_columns],as.numeric)

# Function for processing the percentage columns
clean_percentage <- function(x){
  if(is.na(x) | is.null(x) | is.nan(x)){
    return(NA)
  } else {
    return(as.numeric(substr(x,1,nchar(x)-1)))
  }
}

loans.full[, percentage_columns] <- sapply( loans.full[, percentage_columns], as.character)
for (i in percentage_columns) {
  loans.full[, i] <- sapply( loans.full[, i], clean_percentage)
}

# Clean the date columns
loans.full[, date_columns] <- sapply( loans.full[, date_columns], as.character)

clean_date <- function(x) {
  if(is.na(x) | is.null(x) | is.nan(x)) {
    return(NA)
  } else {
    # We assume the dates are formatted as month-year (w/ abbreviated month)
    # e.g. "Dec-2011" or "Feb-2007"
    # To handle this we append a 1 for the day and call as.Date
    return(as.Date(paste("1",x,sep="-"),format="%d-%b-%Y"))
  }
}


for (i in date_columns) {
  loans.full[,i] <- sapply(loans.full[,i], clean_date)
}

# add credit history feature
loans.full[,'cr_hist'] <- (loans.full$issue_d - loans.full$earliest_cr_line) /30
float_columns <- c(float_columns, 'cr_hist')

# Drop rows with NA values in specified columns
loans.full <- na.omit(loans.full,cols=drop_na_columns)
nrow(loans.full)
summary(loans.full)


# Engineer the features and generate the training/testing sets

# First create the label from the loan status column (needs to be converted from string to numeric data)

loans.full <- loans.full[which(loans.full$loan_status %in% c('Fully Paid','Charged Off','Default')),]
loans.full[,'default'] <- as.factor(as.integer(loans.full$loan_status %in% c('Charged Off','Default')))



## generate train/test sets

# Grab columns for eventual later use
loans.pred <- loans.full[, c('default','loan_amnt','funded_amnt','int_rate','installment',
                             'annual_inc','dti','delinq_2yrs','open_acc','pub_rec',
                             'fico_range_high','fico_range_low','cr_hist','revol_bal',
                             'recoveries','last_fico_range_high','last_fico_range_low',
                             'revol_util', 'total_pymnt')]

features <- c('loan_amnt','funded_amnt','int_rate','installment',
              'annual_inc','dti','delinq_2yrs','open_acc','pub_rec',
              'fico_range_high','fico_range_low','cr_hist','revol_bal',
              'recoveries','last_fico_range_high','last_fico_range_low',
              'revol_util', 'total_pymnt')

# If some of the analysis later takes too long, you can sub sample
# the dataset to reduce the number of observations by uncommenting next two lines
# s <- sample(nrow(loans.pred),10000)
# loans.pred <- loans.pred[s,]


# downscale the dataset so that neither class in default is over-represented
# The caret package provides a wrapper to do just that
if (!require(caret)) {
  install.packages('caret')
}
library('caret')

loans.pred <- downSample(loans.pred[,features],loans.pred$default,yname='default')

maxminscale <- function(x)
{
  return((x-min(x)) / (max(x)-min(x)))
}

for(i in float_columns) {
  loans.pred[,i] <- maxminscale(loans.pred[,i])
}
for(i in percentage_columns) {
  loans.pred[,i] <- maxminscale(loans.pred[,i])
}


# split into test/train
set.seed(2314513)
N <- nrow(loans.pred)
fraction <- 0.7                   # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction
loans.train <- loans.pred[train_idxs,]
loans.test <- loans.pred[test_idxs,]

if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
# decision trees for train dataset
ftree <- rpart(default ~ .,data=loans.train)
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ftree); text(ftree)  # simple graph
prp(ftree)                # tree graph
prp(ftree,extra=101)      # add the size and proportion of data in the node

# Plot ROC curve and Calculare AUC
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
fit.pr.tree <- predict(ftree,loans.test,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,loans.test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)
#Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

# logistic regression
lrfit <- glm(default ~ .,data=loans.train,family="binomial")
summary(lrfit)
# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=loans.test, type="response")
pred.lr = prediction(lr.default.prob, loans.test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))


## Both trees and logisitic regression give a nearly perfect fit!!
## There is probably some leakage here :(

## Step 2: 
## Redo analysis with subset of features

# We will drop the first column that read.csv adds as it isn't necessary
loans.final <- read.csv('loans_data.csv')[-1]
summary(loans.final)
loans.final[,'default'] <- as.factor(as.integer(loans.final$loan_status %in% c('Charged Off','Default')))

loans.pred <- loans.final[, c('default',
                             'home_ownership',
                             'term',
                             'purpose',
                             'verification_status',
                             'grade',
                             'emp_length',
                             'loan_amnt',
                             'funded_amnt',
                             'installment',
                             'annual_inc',
                             'dti',
                             'revol_bal',
                             'delinq_2yrs',
                             'open_acc',
                             'pub_rec',
                             'fico_range_high',
                             'fico_range_low',
                             'int_rate',
                             'revol_util')]
cont_features <- c('loan_amnt',
                   'funded_amnt',
                   'installment',
                   'annual_inc',
                   'dti',
                   'revol_bal',
                   'delinq_2yrs',
                   'open_acc',
                   'pub_rec',
                   'fico_range_high',
                   'fico_range_low',
                   'int_rate',
                   'revol_util')
disc_features <- c('home_ownership',
                   'term',
                   'purpose',
                   'verification_status',
                   'grade',
                   'emp_length')

features <- c(cont_features,disc_features)

# downscale the dataset so that neither class in default is over-represented
if (!require(caret)) {
  install.packages('caret')
}
library('caret')

loans.pred <- downSample(loans.pred[,features],loans.pred$default,yname='default')

maxminscale <- function(x)
{
  return((x-min(x)) / (max(x)-min(x)))
}

for(i in cont_features) {
  loans.pred[,i] <- maxminscale(loans.pred[,i])
}


# split into test/train
set.seed(2314513)
N <- nrow(loans.pred)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction
loans.train <- loans.pred[train_idxs,]
loans.test <- loans.pred[test_idxs,]

if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
# decision trees
ftree <- rpart(default~.,data=loans.train)
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ftree); text(ftree)  # simple graph
prp(ftree)                # tree graph
prp(ftree,extra=101)      # add the size and proportion of data in the node

# Plot ROC curve and Calculate AUC
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
fit.pr.tree <- predict(ftree,loans.test,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,loans.test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)
#Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

# logistic regression
lrfit <- glm(default ~ .,data=loans.train,family="binomial")
summary(lrfit)
# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=loans.test, type="response")
pred.lr = prediction(lr.default.prob, loans.test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

###ADDED: Determining significant variables in the ROC 
library(psych)
SD(loans.pred)
standdev <- SD(loans.pred)
coeffic <- summary(lrfit)$coeff[,1]

#Remove first column of coefficient, as this is just an intercept
coeffic <- coeffic[-1]
#Retrieve 13 columns of standard deviation and coefficients to cross multiply
standdev <- standdev[1:13]
coeffic <- coeffic[1:13]
abs(standdev * coeffic)

## Step 3: Redo the analysis using only each of the synthetic variables calculated by LC
#    namely the grade only or the interest rate only
#    Let's use only logisitc regression for this.
# logistic regression
lrfit <- glm(default ~ grade,data=loans.train,family="binomial")
summary(lrfit)
# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=loans.test, type="response")
pred.lr = prediction(lr.default.prob, loans.test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

#... now with the interest rate only
lrfit <- glm(default ~ int_rate,data=loans.train,family="binomial")
summary(lrfit)

# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=loans.test, type="response")
pred.lr = prediction(lr.default.prob, loans.test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

##############################################################################
## Step 4: Finally we will do both models dropping these two variables.
features
# Entries 12 and 18 are these two that we want to drop
(features = features[-c(12,18)])

# downscale the dataset so that neither class in default is over-represented
if (!require(caret)) {
  install.packages('caret')
}
library('caret')

loans.pred <- downSample(loans.pred[,features],loans.pred$default,yname='default')

cont_features
#Remove int_rate
cont_features = cont_features[-12]
for(i in cont_features) {
  loans.pred[,i] <- maxminscale(loans.pred[,i])
}


# split into test/train
set.seed(2314513)
N <- nrow(loans.pred)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction
loans.train <- loans.pred[train_idxs,]
loans.test <- loans.pred[test_idxs,]

if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
# decision trees
ftree <- rpart(default ~ .,data=loans.train)
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ftree); text(ftree)  # simple graph
prp(ftree)                # tree graph
prp(ftree,extra=101)      # add the size and proportion of data in the node

if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # show model response
plotmo(ftree)   # evaluates selected input but holds other values at median


# Plot ROC curve and Calculate AUC
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
fit.pr.tree <- predict(ftree,loans.test,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,loans.test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)
#Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

# logistic regression
####################################################
# Stepwise Regression
# run a step-wise regression 
# first estimate the null model (this just has an intercept)
null = glm(default~1,data=loans.train,family="binomial")
# second estimate a complete model (with all variables that you are interested in)
full = glm(default~.,data=loans.train,family="binomial") 
# finally estimate the step wise regression starting with the null model
swlrmdl = step(null, scope=formula(full),steps=15,dir="forward")  # !! can increase beyond 15 steps, just takes more time
summary(swlrmdl)

# Use the stepwise model for counterfactuals
lrfit = swlrmdl
summary(lrfit)
plotmo(lrfit)             # evaluates selected input but holds other values at median
# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)


####################################################
# Visualization of results against variables
if (!require(visreg)) {install.packages("visreg"); library(visreg)}  # visualize regression
# plot the log of the odds ratio of Default as function of loan amount
visreg(lrfit,"loan_amnt",ylab="Log(OddsRatio of Default)")
# plot the prob of default as a function of the fico_range_high value
visreg(lrfit,"fico_range_high",scale="response",ylab="Pr(Default)")
# create a contour plot to visualize two effects at the same time (fico_range_high and annual_inc)
visreg2d(lrfit,"fico_range_high","annual_inc",plot.type="image",main="Log(OddsRatio of Default)")
visreg2d(lrfit,"fico_range_high","annual_inc",scale="response",plot.type="image",main="Pr(Default)")
#########################
# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=loans.test, type="response")
pred.lr = prediction(lr.default.prob, loans.test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

###ADDED: Determining significant variables in the ROC 
library(psych)
SD(loans.test)
standdev1 <- SD(loans.test)
coeffic1 <- summary(lrfit)$coeff[,1]

#Remove first column of coefficient, as this is just an intercept
coeffic1 <- coeffic1[-1]
#Retrieve 13 columns of standard deviation and coefficients to cross multiply
standdev1 <- standdev1[1:13]
coeffic1 <- coeffic1[1:13]
abs(standdev1 * coeffic1)

###############################################################################
### compare models using ROC plot
###############################################################################

# plot all ROC curves together
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(fit.perf.tree,add=TRUE,col="blue")
legend("bottomright",c("LogRegr","Tree"),pch=15,col=c("red","blue"),bty="n")



################################################################
## Spreadsheet output of regression model for further analysis:
################################################################
# Export data for a simulator spreadsheet to "loans_lrmodeldata.csv"
### uses the models that were created above, so you must have trained your models
###
### the CSV file contains the:
###  a) the model parameters from our logistic regression, and
###  b) average and standard deviation of the original data

# a) retrieve coefficients from your model
coeflist=summary(lrfit)$coefficients  # extract coefficients estimates and std errors and z values
coefdata=data.frame(rn=rownames(coeflist),coeflist,row.names=NULL)  # change to dataframe
colnames(coefdata)=c("rn",colnames(coeflist))
print(coefdata)   # print out the coefficients

# b) retrieve averages and std dev across all users
modelall=model.matrix(lrfit,data=loans.train)  # get a matrix of all data used in the model (just training sample)
meandata=apply(modelall,2,mean) # compute the average for the selected variables (the "2" means compute by column)
sddata=apply(modelall,2,sd)  # compute the standard deviation for selected variables
descdata=data.frame(rn=names(meandata),meandata,sddata,row.names=NULL)  # merge the vectors with the mean and stddev into a single dataframe
print(descdata)   # print out the descriptive values

# data manipulation package
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
# combine the data together to make it easier to dump out to a single spreadsheet
mdata=join(coefdata,descdata,type='full',by='rn')  # merge the coefficients and descriptive stats
print(mdata)    # print out the combined data

# write the data to a spreadsheet
write.csv(mdata,file="loans_lrmodeldata.csv")   
###############################################################################################
#  ADVANCED Models  #
# Random Forest  ####
#####################
#install.packages("randomForest")
library(randomForest)
loans.rf <- randomForest(default ~ ., data=loans.train, ntree=200, nodesize=15)
rf.loans.predict <- predict(loans.rf, newdata=loans.train)
table(loans.train$default, rf.loans.predict)

rf.default.prob = predict(loans.rf, newdata=loans.test, type="prob")
pred.rf = prediction(rf.default.prob[,"1"], loans.test$default)
perf.rf = performance(pred.rf, measure = "tpr", x.measure = "fpr") 
importance(rf.loans)
varImpPlot(rf.loans)

###############################################################################
### compare models using ROC plot
###############################################################################

# plot all ROC curves together
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(fit.perf.tree,add=TRUE,col="blue")
plot(perf.rf,add=TRUE,col="green")
legend("bottomright",c("LogRegr","Tree","RandomForest"),pch=15,col=c("red","blue","green"),bty="n")


################ Output predictions from all models for further analysis and optimization ######
# Output a final csv file that adds three columns with the default probability predicted by your model 
#     as the last three columns
tree.default.prob = predict(ftree, newdata=loans.test, type="prob")
lr.default.prob = predict(lrfit, newdata=loans.test, type="response")
rf.default.prob = predict(loans.rf, newdata=loans.test, type="prob")
write.csv(cbind(loans.test,tree.default.prob[,2],lr.default.prob,rf.default.prob[,2]),"loans_imputed_all.csv")


# We will drop the first column that read.csv adds as it isn't necessary
loans.final <- read.csv('loans_data.csv')[-1]
continuous_features <- as.vector(read.csv('cont_features.csv')[-1])
discrete_features <- as.vector(read.csv('disc_features.csv')[-1])
summary(loans.final)





