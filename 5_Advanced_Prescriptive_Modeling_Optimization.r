# Week5.r 

# Prescriptive Analytics

# In this script we prepare data to be exported to Excel for use in
# optimization models.

# The main extra parameter we need from the data is an estimate of the
# standard deviation of the return of each loan, which we can use
# to quantify the risk of the loan.

# To do this we use k-means clustering to group loans into groups of similar
# loans, then assign the return standard deviation in each group as the
# standard deviation of each loan.

## Load saved data
# We will drop the first column that read.csv adds as it isn't necessary
loans.final <- read.csv('loans_data.csv')[-1]
continuous_features <- as.vector(read.csv('cont_features.csv')[[-1]])
discrete_features <- as.vector(read.csv('disc_features.csv')[[-1]])


# final set of features to use for default probability and predicting returns
features <- c('home_ownership','term','purpose','verification_status','emp_length',
              'loan_amnt','funded_amnt','annual_inc','dti','revol_bal','delinq_2yrs',
              'open_acc','pub_rec','fico_range_high','fico_range_low','revol_util', 'cr_hist')


summary(loans.final)



## add the employment length, credit history, and default features

# Use readr package to parse numbers from strings easily
if(!require(readr)) { install.packages("readr") }
library(readr)
# convert employment length to numeric values
loans.final$emp_length <- as.character(loans.final$emp_length)
emp_length <- parse_number(loans.final[,'emp_length'])

loans.final['emp_length'] <- emp_length

# Add credit history and default columns
loans.final[,'cr_hist'] <- (loans.final$issue_d - loans.final$earliest_cr_line) /30
continuous_features <- c(continuous_features,'cr_hist')


loans.final <- loans.final[which(loans.final$loan_status %in% c('Fully Paid','Charged Off','Default')),]
loans.final[,'default'] <- as.factor(as.integer(loans.final$loan_status %in% c('Charged Off','Default')))
# loans.final[,continuous_features] <- scale(loans.final[,continuous_features])

# remove NA rows
loans.final <- na.omit(loans.final)


## set up the data to carry out a k-means clustering to find std dev's for loan returns

# columns to use in the clustering
cluster_columns <- c('fico_range_high','fico_range_low','int_rate', 'annual_inc','loan_amnt','emp_length','funded_amnt','dti','pub_rec','cr_hist')

# let's save the mean and std dev so we can reverse the scaling
loans.mean=colMeans(loans.final[,cluster_columns])
loans.sd=apply(loans.final[,cluster_columns],2,sd)

# scale the data set before applying k-means
loans.final[,cluster_columns] <- scale(loans.final[,cluster_columns])

## prepare train/test sets
set.seed(2314513)
N <- nrow(loans.final)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction

# pick out the training set to make the k-means model
loans.train <- loans.final[train_idxs,]


# Choose a value of k for the k means clustering
# Now we explore clustering for different values of K
K_max <- 100
wcss <- numeric(K_max)
for(K in 1:K_max) {
  clustersK <- kmeans(loans.train[,cluster_columns],centers=K)
  wcss[K] <- clustersK$tot.withinss
}
plot(1:K_max,wcss,main="Within Cluster Sum of Squares vs K",xlab = 'K',ylab = 'WCSS')
lines(1:K_max,wcss)

# Trying k = 50 
K <- 50
clusters <- kmeans(loans.train[,cluster_columns],centers=K,nstart=50)



## Now predict cluster centers for the test set

# load a package for fast nearest neighbor look-ups
if(!require(FNN)) { install.packages("FNN") }
library(FNN)

# assign cluster centers
# this runs a nearest neighbor algorithm with the cluster centroids as input data
# and the loans as query points.  We extract only the index of the assigned centers.
loans.clusters <- get.knnx(clusters$centers,loans.final[,cluster_columns],1)$nn.index[,1]


## Use the clustering to assign a standard deviation to each loan

# names of return columns and corresponding columns for estimated std deviations
# important that these two have the same length for the "for" loop below
ret_cols <- c('ret_PESS','ret_OPT','ret_INTa','ret_INTb','ret_INTc')
std_dev_cols <- c('std_PESS','std_OPT','std_INTa','std_INTb','std_INTc')

# Initialize the values for these columns
for (col in std_dev_cols) {
  loans.final[,col] <- NA
}
# Calculate the standard deviation for each group and return type
st_dev_values <- as.data.frame(matrix(nrow=K,ncol=5))
colnames(st_dev_values) <- ret_cols

# Note that stdevn values are calculated only from the training data set
for (i in 1:K) {
  for (j in ret_cols) {
    st_dev_values[i,j] = sd(loans.train[clusters$cluster == i,j])
  }
}

# Fill the std dev values for each type for each data point
for (i in 1:nrow(loans.final)) {
  loans.final[i,"std_PESS"] <- st_dev_values[loans.clusters[i], "ret_PESS"]
  loans.final[i,"std_OPT"]  <- st_dev_values[loans.clusters[i], "ret_OPT"]
  loans.final[i,"std_INTa"] <- st_dev_values[loans.clusters[i], "ret_INTa"]
  loans.final[i,"std_INTb"] <- st_dev_values[loans.clusters[i], "ret_INTb"]
  loans.final[i,"std_INTc"] <- st_dev_values[loans.clusters[i], "ret_INTc"]
}


# Re-scale the continuous features
# translate the values back to the original scale
loans.final[,cluster_columns]=sweep(loans.final[,cluster_columns],MARGIN=2,loans.sd[cluster_columns],'*')       # step 1) scale: multiply the corresponding sd
loans.final[,cluster_columns]=sweep(loans.final[,cluster_columns],MARGIN=2,loans.mean[cluster_columns],'+')  # step 2) shift: add the original mean

# pick out the test set
loans.test <- loans.final[test_idxs,]


#### Run the LASSO model for predicting returns on the test data set########################################
# we can reuse the indexes above to generate train/test sets for each return method
# Pessimistic return method
ret_col <- "ret_PESS"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]

if (!require(glmnet)){ install.packages('glmnet') }; library("glmnet")
if (!require(glmnetUtils)){ install.packages("glmnetUtils")}; library("glmnetUtils")

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

test.predn <- data.frame(rownames(regression.test))

test.predn$lassoret_PESS <- predict(lassoreg,regression.test,s="lambda.min")

###Repeat the above for the three other return methods#################

# Optimistic return method
ret_col <- "ret_OPT"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]


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

test.predn$lassoret_OPT <- predict(lassoreg,regression.test,s="lambda.min")

####### Interest return method a############################
#
ret_col <- "ret_INTa"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]


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

test.predn$lassoret_INTa <- predict(lassoreg,regression.test,s="lambda.min")


########Interest return method b################################################
# 
ret_col <- "ret_INTb"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]


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

test.predn$lassoret_INTb <- predict(lassoreg,regression.test,s="lambda.min")


########Interest return method c################################################
# 
ret_col <- "ret_INTc"

regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]


## L2-Regularized linear regression, i.e. LASSO

# use alpha = 1 to select LASSO
lasso.mod <- glmnet(ret_INTc ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lasso.mod)
plot(lasso.mod)
# Generate a cv record for the various lambdas
lassoreg <- cv.glmnet(ret_INTc ~ . - fico_range_low -funded_amnt,data=regression.train,alpha=1)
summary(lassoreg)
plot(lassoreg)
coef(lassoreg,s="lambda.min")

(lassoreg_mspe <- mean((regression.test$ret_INTc - predict(lassoreg,regression.test,s="lambda.min")) ^ 2))

test.predn$lassoret_INTc <- predict(lassoreg,regression.test,s="lambda.min")



##############################################################################

loans.test.aug <- cbind(loans.test,test.predn)

# set of columns to output to excel - add any other columns you want to see
pred_ret_cols <- c('lassoret_PESS','lassoret_OPT','lassoret_INTa','lassoret_INTb','lassoret_INTc')
output_cols <- c(ret_cols,pred_ret_cols,std_dev_cols,'loan_amnt','grade', 'int_rate')

# selection of rows to output
sample_size <- 2000 # maybe no more than 10,000, or the size of the test set
set.seed(98237)
output_rows <- sample(nrow(loans.test),sample_size)


# create a single data file for all the returns
write.csv(loans.test.aug[output_rows,output_cols],'loans_pred__opti_data.csv')

###########################################################################################



