#Script for HW 4

### HW4 - Picking Loans using Regression of Loan Returns

# Steps to implement:
#
# 1. Load saved data
# 2. Rerun models from week 3 and test stability over time
#     a. need to implement proper train-test split for this
# 3. Implement regression to predict returns (using the different methods)
#     a. linear regression
#     b. l1-regularized linear regression
#     c. l2-regularized linear regression
# 4. Implement heuristics to choose loans to invest in
#     a. Random
#     b. Ranking - Score by probability of default (from Week 3 models)
#     c. Regression - Score by predicted return (from regression)
#     d. Greedy/Knapsack - score by return to risk ratio (using both classification and regression results)
#     e. Rank by return but only accept if P(default) is not too high
#     (f. If you want to go further, you can try to implement a two-stage model - 
#          Estimate expected return in two stages: stage 1 - default or not default, 
#          stage 2 - resulting return given stage 1 state, and combine them using 
#         Pr(default) predicted from another classification model)
# 

# utility function for scaling datasets before training model
maxminscale <- function(x)
{
  return((x-min(x)) / (max(x)-min(x)))
}

# final set of features to use for default probability and predicting returns
features <- c('home_ownership','term','purpose','verification_status','emp_length',
              'loan_amnt','funded_amnt','annual_inc','dti','revol_bal','delinq_2yrs',
              'open_acc','pub_rec','fico_range_high','fico_range_low','revol_util', 'cr_hist')


####Part 1: Testing the stability over time of our default probability models####

# load the data for 2007-2011
loans07_11 <- read.csv(unz("2007-2011-LoanStats3a_securev1.csv.zip","LoanStats3a_securev1.csv"), skip=1L)

# load the data for 2012-2013
loans12_13 <- read.csv(unz("2012-2013-LoanStats3b_securev1.csv.zip","LoanStats3b_securev1.csv"), skip=1L)

set.seed(7777777)
# randomly sample rows from loans12_13 so its not too much bigger
loans12_13 <- loans12_13[sample(nrow(loans07_11)),]


columns_to_pick <- c('id','loan_amnt','funded_amnt','funded_amnt_inv','term','int_rate',
                     'installment','grade','sub_grade','emp_title','emp_length',
                     'home_ownership','annual_inc','verification_status','issue_d',
                     'loan_status','purpose','title','zip_code','addr_state','dti','total_pymnt',
                     'delinq_2yrs','earliest_cr_line','open_acc','pub_rec','last_pymnt_d',
                     'last_pymnt_amnt','fico_range_high','fico_range_low','last_fico_range_high',
                     'last_fico_range_low','application_type','revol_bal','revol_util','recoveries')

loans07_11 <- loans07_11[,columns_to_pick]
loans12_13 <- loans12_13[,columns_to_pick]

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
loans07_11[,float_columns] <- sapply(loans07_11[,float_columns],as.numeric)
loans12_13[,float_columns] <- sapply(loans12_13[,float_columns],as.numeric)

# Function for processing the percentage columns
clean_percentage <- function(x){
  if(is.na(x) | is.null(x) | is.nan(x)){
    return(NA)
  } else {
    return(as.numeric(substr(x,1,nchar(x)-1)))
  }
}

loans07_11[, percentage_columns] <- sapply( loans07_11[, percentage_columns], as.character)
loans12_13[, percentage_columns] <- sapply( loans12_13[, percentage_columns], as.character)
for (i in percentage_columns) {
  loans07_11[, i] <- sapply( loans07_11[, i], clean_percentage)
  loans12_13[, i] <- sapply( loans12_13[, i], clean_percentage)
}

# Clean the date columns
loans07_11[, date_columns] <- sapply( loans07_11[, date_columns], as.character)
loans12_13[, date_columns] <- sapply( loans12_13[, date_columns], as.character)

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
  loans07_11[,i] <- sapply(loans07_11[,i], clean_date)
  loans12_13[,i] <- sapply(loans12_13[,i], clean_date)
}

# add credit history feature
loans07_11[,'cr_hist'] <- (loans07_11$issue_d - loans07_11$earliest_cr_line) /30
loans12_13[,'cr_hist'] <- (loans12_13$issue_d - loans12_13$earliest_cr_line) /30
float_columns <- c(float_columns, 'cr_hist')

# scale the continuous features
#for(i in c(float_columns,percentage_columns)) {
#  loans07_11[,i] <- maxminscale(loans07_11[,i])
#  loans12_13[,i] <- maxminscale(loans12_13[,i])
#}


# Thomas: I'm removing this step for now since I will just delete rows with NA's
# from the dataset only containing the features and the response (lines 154,155)
# Drop rows with NA values in specified columns
#loans07_11 <- na.omit(loans07_11,cols=drop_na_columns)
#loans12_13 <- na.omit(loans12_13,cols=drop_na_columns)


# create the label from the loan status column (needs to be converted from string to numeric data)

loans07_11 <- loans07_11[which(loans07_11$loan_status %in% c('Fully Paid','Charged Off','Default')),]
loans07_11[,'default'] <- as.factor(as.integer(loans07_11$loan_status %in% c('Charged Off','Default')))

loans12_13 <- loans12_13[which(loans12_13$loan_status %in% c('Fully Paid','Charged Off','Default')),]
loans12_13[,'default'] <- as.factor(as.integer(loans12_13$loan_status %in% c('Charged Off','Default')))

# remove everything except the response variables and the final set of features
loans07_11 <- na.omit(loans07_11[,c('default',features)])
loans12_13 <- na.omit(loans12_13[,c('default',features)])


# downscale the dataset so that neither class in default is over-represented
if (!require(caret)) { install.packages('caret') };  library('caret')


# downsample so that default and not-default are equally represented
loans07_11 <- downSample(loans07_11[,features],loans07_11$default,yname='default')
loans12_13 <- downSample(loans12_13[,features],loans12_13$default,yname='default')


# Train and test on each collection of years to see changes
# interested in changes to:
# 1. performance
# 2. important variables

# split into test and training sets
fraction <- 0.7

N <- nrow(loans07_11)
rand_values <- runif(N)
train07_11 <- loans07_11[rand_values <= fraction,]
test07_11 <- loans07_11[rand_values > fraction,]

N <- nrow(loans12_13)
rand_values <- runif(N)
train12_13 <- loans12_13[rand_values <= fraction,]
test12_13 <- loans12_13[rand_values > fraction,]


# decision tree

if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}

ftree07_11 <- rpart(default ~ ., data=loans07_11)
#par(mfrow=c(1,1))         # reset one graphic per panel
#plot(ftree07_11); text(ftree07_11)  # simple graph
#prp(ftree07_11)                # tree graph
prp(ftree07_11,extra=101)      # add the size and proportion of data in the node

ftree12_13 <- rpart(default ~ ., data=loans12_13)
#par(mfrow=c(1,1))         # reset one graphic per panel
#plot(ftree12_13); text(ftree12_13)  # simple graph
#prp(ftree12_13)                # tree graph
prp(ftree12_13,extra=101)      # add the size and proportion of data in the node

# Plot ROC curve and Calculate AUC
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}

fit.pr.tree07_11 <- predict(ftree07_11,test07_11,type="prob")[,2]
fit.pr.tree12_13 <- predict(ftree12_13,test12_13,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree07_11 <- prediction(fit.pr.tree07_11,test07_11$default)
fit.pred.tree12_13 <- prediction(fit.pr.tree12_13,test12_13$default)

fit.perf.tree07_11 <- performance(fit.pred.tree07_11,"tpr","fpr")
fit.perf.tree12_13 <- performance(fit.pred.tree12_13,"tpr","fpr")
plot(fit.perf.tree07_11,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
plot(fit.perf.tree12_13,lwd=2,col="red",add=TRUE)
abline(a=0,b=1)
#Commands for calculating AUROC using ROCR
auc.tmp07_11 <- performance(fit.pred.tree07_11,"auc")
(auc.tree07_11 <- as.numeric(auc.tmp07_11@y.values))
auc.tmp12_13 <- performance(fit.pred.tree12_13,"auc")
(auc.tree12_13 <- as.numeric(auc.tmp12_13@y.values))
#Correlation Plot
corrplot()

# logistic regression

lrfit07_11 <- glm(default ~ .,data=loans07_11,family="binomial")
summary(lrfit07_11)
# index of Significant predictors
id07_11 = which(summary(lrfit07_11)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig07_11 = summary(lrfit07_11)$coeff[,1][id07_11]
names(coeff.sig07_11)

lrfit12_13 <- glm(default ~ .,data=loans12_13,family="binomial")
summary(lrfit12_13)
# index of Significant predictors
id12_13 = which(summary(lrfit12_13)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig12_13 = summary(lrfit12_13)$coeff[,1][id07_11]
names(coeff.sig12_13)

# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob07_11 = predict(lrfit07_11, newdata=test07_11, type="response")
lr.default.prob12_13 = predict(lrfit12_13, newdata=test12_13, type="response")

pred.lr07_11 = prediction(lr.default.prob07_11, test07_11$default)
pred.lr12_13 = prediction(lr.default.prob12_13, test12_13$default)

perf.lr07_11 = performance(pred.lr07_11, measure = "tpr", x.measure = "fpr") 
perf.lr12_13 = performance(pred.lr12_13, measure = "tpr", x.measure = "fpr") 

plot(perf.lr07_11, col='blue')
plot(perf.lr12_13, col='red',add=TRUE)
abline(a=0,b=1)

#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr07_11,"auc")
(auc.lr07_11 = as.numeric(auc.tmp@y.values))
auc.tmp = performance(pred.lr12_13,"auc")
(auc.lr12_13 = as.numeric(auc.tmp@y.values))


# random forests
library(randomForest)

rf07_11 <- randomForest(default ~ ., data=train07_11, ntree=200, nodesize=15)
rf12_13 <- randomForest(default ~ ., data=train12_13, ntree=200, nodesize=15)

rf.loans.predict07_11 <- predict(rf07_11, newdata=train07_11)
table(train07_11$default, rf.loans.predict07_11)
rf.loans.predict12_13 <- predict(rf12_13, newdata=train12_13)
table(train12_13$default, rf.loans.predict12_13)

rf.default.prob07_11 = predict(rf07_11, newdata=test07_11, type="prob")
rf.default.prob12_13 = predict(rf12_13, newdata=test12_13, type="prob")
pred.rf07_11 = prediction(rf.default.prob07_11[,"1"], test07_11$default)
pred.rf12_13 = prediction(rf.default.prob12_13[,"1"], test12_13$default)
perf.rf07_11 = performance(pred.rf07_11, measure = "tpr", x.measure = "fpr") 
perf.rf12_13 = performance(pred.rf12_13, measure = "tpr", x.measure = "fpr") 

plot(perf.rf07_11, col='blue')
plot(perf.rf12_13, col='red',add=TRUE)
abline(a=0,b=1)

#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.rf07_11,"auc")
(auc.rf07_11 = as.numeric(auc.tmp@y.values))
auc.tmp = performance(pred.rf12_13,"auc")
(auc.rf12_13 = as.numeric(auc.tmp@y.values))

importance(rf07_11)
varImpPlot(rf07_11)
importance(rf12_13)
varImpPlot(rf12_13)

####Train on all of 2007-2011 and test on 2012-2013############################

# decision tree

ftree <- rpart(default ~ ., data=loans07_11)

# Plot ROC curve and Calculate AUC

fit.pr.tree <- predict(ftree,loans12_13,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,loans12_13$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
#plot(fit.perf.tree,lwd=2,col="blue",
#     main="ROC:  Classification Trees on Loan Default Dataset")
#abline(a=0,b=1)

#Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))


# logistic regression

lrfit <- glm(default ~ .,data=loans07_11,family="binomial")

# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=loans12_13, type="response")
pred.lr = prediction(lr.default.prob, loans12_13$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
#plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

# random forest

loans.rf <- randomForest(default ~ ., data=loans07_11, ntree=200, nodesize=15)
rf.loans.predict <- predict(loans.rf, newdata=loans07_11)
table(loans07_11$default, rf.loans.predict)

rf.default.prob = predict(loans.rf, newdata=loans12_13, type="prob")
pred.rf = prediction(rf.default.prob[,"1"], loans12_13$default)
perf.rf = performance(pred.rf, measure = "tpr", x.measure = "fpr")
#plot(perf.rf, col="green")

#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.rf,"auc")
(auc.rf = as.numeric(auc.tmp@y.values))


# plot all ROC curves together
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(fit.perf.tree,add=TRUE,col="blue")
plot(perf.rf,add=TRUE,col="green")
legend("bottomright",c("LogRegr","Tree","RandomForest"),pch=15,col=c("red","blue","green"),bty="n")





#####Part 2: Predicting returns and picking loans using heuristics############
### 
## Load saved data
# We will drop the first column that read.csv adds as it isn't necessary
loans.final <- read.csv('loans_data.csv')[-1]
continuous_features <- as.vector(read.csv('cont_features.csv')[[-1]])
discrete_features <- as.vector(read.csv('disc_features.csv')[[-1]])

summary(loans.final)

# Add credit history and default columns

loans.final[,'cr_hist'] <- (loans.final$issue_d - loans.final$earliest_cr_line) /30
continuous_features <- c(continuous_features,'cr_hist')


#loans.final <- loans.final[which(loans.final$loan_status %in% c('Fully Paid','Charged Off','Default')),]
loans.final[,'default'] <- as.factor(as.integer(loans.final$loan_status %in% c('Charged Off','Default')))

loans.final[,continuous_features] <- scale(loans.final[,continuous_features])


maxminscale <- function(x)
{
  return((x-min(x)) / (max(x)-min(x)))
}

for (i in continuous_features) {
  loans.final[,i] <- maxminscale(loans.final[,i])
}

# drop columns with na values
loans.final <- na.omit(loans.final)


## Train regression models to predict return


# final set of features to use for default probability and predicting returns
features <- c('home_ownership','term','purpose','verification_status','emp_length',
              'loan_amnt','funded_amnt','annual_inc','dti','revol_bal','delinq_2yrs',
              'open_acc','pub_rec','fico_range_high','fico_range_low','revol_util', 'cr_hist')


# library for regularized linear regression
if (!require(glmnet)){ install.packages('glmnet') }; library("glmnet")
if (!require(glmnetUtils)){ install.packages("glmnetUtils")}; library("glmnetUtils")
library(randomForest)
library(rpart)
library(rpart.plot)


## prepare train/test sets
set.seed(2314513)
N <- nrow(loans.final)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction

# we can reuse the indexes above to generate train/test sets for each return method


# Pessimistic return method
ret_col <- "ret_PESS"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]


# baseline "guess the mean" regression
(mean( (regression.test$ret_PESS - mean(regression.test$ret_PESS))^2))


## Ordinary linear regression
linreg <- lm(ret_PESS ~ .,data=regression.train)
summary(linreg)
library(car)
vif(linreg)
# The error here says that there are perfectly correlated variables in the data!

#compute correlation matrix among numerical attributes in one large matrix
(correlations <- cor(regression.train[,-c(1:6)]))
#Install a package for plotting correlations and include it
if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}
#Generate a heat map of correlated predictors
#  (the hclust parameter orders the rows and columns according to a hierarchical clustering method)
corrplot(correlations, order="hclust")
# Remove one from each pair of highly correlated variables and repeat the regression

linreg <- lm(ret_PESS ~ . - fico_range_low -funded_amnt,data=regression.train)
summary(linreg)
library(car)
vif(linreg)
names(linreg)
coef(linreg)
confint(linreg)

# Compute Mean-Squared-Prediction-Error (MSPE) on test set
(linreg_mspe <- mean((regression.test$ret_PESS - predict.lm(linreg,regression.test)) ^ 2))


## L2-Regularized linear regression, i.e. LASSO

# use alpha = 1 to select LASSO in glmnet
lasso.mod <- glmnet(ret_PESS ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lasso.mod)
plot(lasso.mod)
# Generate a cv record for the various lambdas
lassoreg <- cv.glmnet(ret_PESS ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lassoreg)
plot(lassoreg)
coef(lassoreg,s="lambda.min")

# Compute Mean-Squared-Prediction-Error (MSPE) of best LASSO model in test set
(lassoreg_mspe <- mean((regression.test$ret_PESS - predict(lassoreg,regression.test,s="lambda.min")) ^ 2))


## L1-Regularized linear regression, i.e. Ridge

# use alpha = 0 to select Ridge in glmnet
ridge.mod <- glmnet(ret_PESS ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridge.mod)
plot(ridge.mod)
# Generate a cv record for the various lambdas
ridgereg <- cv.glmnet(ret_PESS ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridgereg)
plot(ridgereg)
coef(ridgereg,s="lambda.min")

# Compute Mean-Squared-Prediction-Error (MSPE) of best ridge model in test set
(ridgereg_mspe <- mean((regression.test$ret_PESS - predict(ridgereg,regression.test,s="lambda.min")) ^ 2))


## Regression trees
regtree <- rpart(ret_PESS ~ . - fico_range_low -funded_amnt,data=regression.train,method='anova')
prp(regtree,extra=101)
(regtree_mspe <- mean((regression.test$ret_PESS - predict(regtree,regression.test)) ^2))


## Random forest (for regression)
regforest <- randomForest(ret_PESS ~ . - fico_range_low -funded_amnt,data=regression.train,ntree=100, nodesize=10)
(regforest_mspe <- mean((regression.test$ret_PESS - predict(regforest,regression.test))^2 ))


###Repeat the above for the three other return methods#################

# Optimistic return method
ret_col <- "ret_OPT"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]

# baseline "guess the mean" regression
(mean( (regression.test$ret_OPT - mean(regression.test$ret_OPT))^2))


## Ordinary linear regression
linreg <- lm(ret_OPT ~ . -fico_range_low -funded_amnt,data=regression.train)
summary(linreg)

# Compute Mean-Squared-Prediction-Error (MSPE) on test set
(linreg_mspe <- mean((regression.test$ret_OPT - predict.lm(linreg,regression.test)) ^ 2))


## L2-Regularized linear regression, i.e. LASSO

# use alpha = 1 to select LASSO
lasso.mod <- glmnet(ret_OPT ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lasso.mod)
plot(lasso.mod)
# Generate a cv record for the various lambdas
lassoreg <- cv.glmnet(ret_OPT ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lassoreg)
plot(lassoreg)
coef(lassoreg,s="lambda.min")

(lassoreg_mspe <- mean((regression.test$ret_OPT - predict(lassoreg,regression.test,s="lambda.min")) ^ 2))


## L1-Regularized linear regression, i.e. Ridge

# use alpha = 0 to select Ridge
ridge.mod <- glmnet(ret_OPT ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridge.mod)
plot(ridge.mod)
# Generate a cv record for the various lambdas
ridgereg <- cv.glmnet(ret_OPT ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridgereg)
plot(ridgereg)
coef(ridgereg,s="lambda.min")


(ridgereg_mspe <- mean((regression.test$ret_OPT - predict(ridgereg,regression.test,s="lambda.min")) ^ 2))

## Regression trees
regtree <- rpart(ret_OPT ~ .-fico_range_low -funded_amnt,data=regression.train,method='anova')
prp(regtree,extra=101)
(regtree_mspe <- mean((regression.test$ret_OPT - predict(regtree,regression.test)) ^2))


## Random forest (for regression)
regforest <- randomForest(ret_OPT ~ . - fico_range_low -funded_amnt,data=regression.train,ntree=100, nodesize=10)
(regforest_mspe <- mean((regression.test$ret_OPT - predict(regforest,regression.test))^2 ))



####### Interest return method a############################
#
ret_col <- "ret_INTa"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]


# baseline "guess the mean" regression
(mean( (regression.test$ret_INTa - mean(regression.test$ret_INTa))^2))



## Ordinary linear regression
linreg <- lm(ret_INTa ~ .-fico_range_low -funded_amnt,data=regression.train)
summary(linreg)

# Compute Mean-Squared-Prediction-Error (MSPE) on test set
(linreg_mspe <- mean((regression.test$ret_INTa - predict.lm(linreg,regression.test)) ^ 2))


## L2-Regularized linear regression, i.e. LASSO

# use alpha = 1 to select LASSO
lasso.mod <- glmnet(ret_INTa ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lasso.mod)
plot(lasso.mod)
# Generate a cv record for the various lambdas
lassoreg <- cv.glmnet(ret_INTa ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lassoreg)
plot(lassoreg)
coef(lassoreg,s="lambda.min")

(lassoreg_mspe <- mean((regression.test$ret_INTa - predict(lassoreg,regression.test,s="lambda.min")) ^ 2))


## L1-Regularized linear regression, i.e. Ridge

# use alpha = 0 to select Ridge
ridge.mod <- glmnet(ret_INTa ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridge.mod)
plot(ridge.mod)
# Generate a cv record for the various lambdas
ridgereg <- cv.glmnet(ret_INTa ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridgereg)
plot(ridgereg)
coef(ridgereg,s="lambda.min")

(ridgereg_mspe <- mean((regression.test$ret_INTa - predict(ridgereg,regression.test,s="lambda.min")) ^ 2))


## Regression trees
regtree <- rpart(ret_INTa ~ . -fico_range_low -funded_amnt,data=regression.train,method='anova')
prp(regtree,extra=101)

(regtree_mspe <- mean((regression.test$ret_INTa - predict(regtree,regression.test)) ^2))


## Random forest (for regression)
regforest <- randomForest(ret_INTa ~ . - fico_range_low -funded_amnt,data=regression.train,ntree=100, nodesize=10)

(regforest_mspe <- mean((regression.test$ret_INTa - predict(regforest,regression.test))^2 ))


########Interest return method b################################################
# 
ret_col <- "ret_INTb"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]


# baseline "guess the mean" regression
(mean( (regression.test$ret_INTb - mean(regression.test$ret_INTb))^2))


## Ordinary linear regression
linreg <- lm(ret_INTb ~ .-fico_range_low -funded_amnt,data=regression.train)
summary(linreg)

# Compute Mean-Squared-Prediction-Error (MSPE) on test set
(linreg_mspe <- mean((regression.test$ret_INTb - predict.lm(linreg,regression.test)) ^ 2))


## L2-Regularized linear regression, i.e. LASSO

# use alpha = 1 to select LASSO
lasso.mod <- glmnet(ret_INTb ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lasso.mod)
plot(lasso.mod)
# Generate a cv record for the various lambdas
lassoreg <- cv.glmnet(ret_INTb ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lassoreg)
plot(lassoreg)
coef(lassoreg,s="lambda.min")

(lassoreg_mspe <- mean((regression.test$ret_INTb - predict(lassoreg,regression.test,s="lambda.min")) ^ 2))

## L1-Regularized linear regression, i.e. Ridge

# use alpha = 0 to select Ridge
ridge.mod <- glmnet(ret_INTb ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridge.mod)
plot(ridge.mod)
# Generate a cv record for the various lambdas
ridgereg <- cv.glmnet(ret_INTb ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=0)
summary(ridgereg)
plot(ridgereg)
coef(ridgereg,s="lambda.min")

(ridgereg_mspe <- mean((regression.test$ret_INTb - predict(ridgereg,regression.test,s="lambda.min")) ^ 2))


## Regression trees
regtree <- rpart(ret_INTb ~ .-fico_range_low -funded_amnt,data=regression.train,method='anova')
prp(regtree,extra=101)
(regtree_mspe <- mean((regression.test$ret_INTb - predict(regtree,regression.test)) ^2))

## Random forest (for regression)
regforest <- randomForest(ret_INTb ~ . - fico_range_low -funded_amnt,data=regression.train,ntree=100, nodesize=10)
(regforest_mspe <- mean((regression.test$ret_INTb - predict(regforest,regression.test))^2 ))



#######Heuristics for selecting loans###########################################
## 

# Random selection
RandomLoans <- function(data,num_loans) 
{
    selected_rows <- data[sample(nrow(data),num_loans),]
    return(selected_rows)
}

# Rank by probability of default using logistic regression model
RankByRisk <- function(data,num_loans,lr_model)
{
    default_prob <- predict(lr_model,newdata=data,type='response')
    ranking <- order(default_prob)
    selected_rows <- data[ranking[1:num_loans],]
    return(selected_rows)
}

# Rank by predicted return from regression model
RankByReturn <- function(data,num_loans,reg_model)
{
    pred_returns <- predict(reg_model,data)
    # use - to sort in descending order
    ranking <- order(-pred_returns)
    selected_rows <- data[ranking[1:num_loans],]
    return(selected_rows)  
}

# Rank by predicted return to risk ratio, i.e. return/default probability
RankByReturnRiskRatio <- function(data,num_loans,reg_model,lr_model)
{
    pred_returns <- predict(reg_model,data)
    default_prob <- predict(lr_model,newdata=data,type='response')
    ratio <- pred_returns / default_prob
    # use - to sort in descending order
    ranking <- order(-ratio)
    selected_rows <- data[ranking[1:num_loans],]
    return(selected_rows)
}

# Rank by return, but only accept the loan if it's risk is below a certain threshold
RankByReturnThreshold <- function(data,num_loans,reg_model,lr_model,thresh)
{
    pred_returns <- predict(reg_model,data)
    default_prob <- predict(lr_model,newdata=data,type='response')
    acceptable <- which(default_prob <= thresh)
    ranking <- order(-pred_returns[acceptable])
    selected_rows <- data[ranking[1:num_loans],]
    return(selected_rows)
}

# Rank by risk, but only accept the loan if it's return is above a certain threshold
RankByRiskThreshold <- function(data,num_loans,reg_model,lr_model,thresh)
{
  pred_returns1 <- predict(reg_model,data)
  default_prob2 <- predict(lr_model,newdata=data,type='response')
  acceptable1 <- which(pred_returns1 >= thresh)
  ranking1 <- order(default_prob2[acceptable1])
  selected_rows1 <- data[ranking1[1:num_loans],]
  return(selected_rows1)
}

# set up the data and parameters
# Uncomment below to choose whichever return method you want to use
#ret_col <- "ret_PESS"
#ret_col <- "ret_OPT"
#ret_col <- "ret_INTa"
ret_col <- "ret_INTb"
num_loans <- 2000
data <- loans.final[,c(ret_col,features)]

# set up the logistic regression model
lr_train <- loans.final[train_idxs,c('default',features)]
lr_model <- glm(default ~ .,data=lr_train,family="binomial")

# set up the regression model to predict returns
reg_train <- loans.final[train_idxs,c(ret_col,features)]

if(ret_col == "ret_PESS") {
  # reg_model <- lm(ret_PESS ~., data=reg_train)
  # alpha = 0 indicates ridge, alpha = 1 indicates lasso
  reg_model <- cv.glmnet(ret_PESS ~ .,data=reg_train,alpha=0)
} else if(ret_col == "ret_OPT") {
  # reg_model <- lm(ret_OPT ~., data=reg_train)
  # alpha = 0 indicates ridge, alpha = 1 indicates lasso
  reg_model <- cv.glmnet(ret_OPT ~ .,data=reg_train,alpha=0)
} else if(ret_col == "ret_INTa") {
  # reg_model <- lm(ret_INTa ~., data=reg_train)
  # alpha = 0 indicates ridge, alpha = 1 indicates lasso
  reg_model <- cv.glmnet(ret_INTa ~ .,data=reg_train,alpha=0)
} else{
  # reg_model <- lm(ret_INTb ~., data=reg_train)
  # alpha = 0 indicates ridge, alpha = 1 indicates lasso
  reg_model <- cv.glmnet(ret_INTb ~ .,data=reg_train,alpha=0)
}


# Now test each method
selected_rows <- RandomLoans(data,num_loans)
(avg_return <- mean(selected_rows[,ret_col]))

selected_rows <- RankByRisk(data,num_loans,lr_model)
(avg_return <- mean(selected_rows[,ret_col]))

selected_rows <- RankByReturn(data,num_loans,reg_model)
(avg_return <- mean(selected_rows[,ret_col]))

selected_rows <- RankByReturnRiskRatio(data,num_loans,reg_model,lr_model)
(avg_return <- mean(selected_rows[,ret_col]))

selected_rows <- RankByReturnThreshold(data,num_loans,reg_model,lr_model,0.05)
(avg_return <- mean(selected_rows[,ret_col]))

selected_rows <- RankByReturnThreshold(data,num_loans,reg_model,lr_model,0.25)
(avg_return <- mean(selected_rows[,ret_col]))

selected_rows <- RankByReturnThreshold(data,num_loans,reg_model,lr_model,0.5)
(avg_return <- mean(selected_rows[,ret_col]))

selected_rows <- RankByRiskThreshold(data,num_loans,reg_model,lr_model,0.1)
(avg_return <- mean(selected_rows[,ret_col]))
