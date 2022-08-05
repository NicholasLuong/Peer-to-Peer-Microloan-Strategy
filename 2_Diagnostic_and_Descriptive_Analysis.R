### Week 2 - Diagnostic and Descriptive Analysis
# This notebook carries out the following steps
# 1. Read the saved data from Week 1
# 2. Add derived features of the returns on each loan
# 3. Visualize variables incl. returns
# 4. Explore returns by grade
# 5. Create multi-variable summaries
# 6. Run PCA and K-means on a relevant subset of the columns
# 7. Re-save data with new return columns

### *** LINES CHANGED: 28-40, 115-123, 182-188, 314-318 *** 
#===============================================================================

## Step 1: Read the data from Week 1

# We will drop the first column that read.csv adds as it isn't necessary
loans.final <- read.csv('LoanStats3a_securev1.csv')[-1]
continuous_features <- as.vector(read.csv('cont_features.csv')[[-1]])
discrete_features <- as.vector(read.csv('disc_features.csv')[[-1]])

summary(loans.final)

## Step 2: Incorporate previous variables to run multi-variable summary 

# Scatter plot between funded amount, interest rate, fico range low, total payment
# Total payment and funded amount are correlated
# Interest rate and FICO range low are correlated 
pairs(~funded_amnt+int_rate+fico_range_low+total_pymnt, data=loans.final)

# Boxplot between funded amount, loan status 
boxplot(loans.final$funded_amnt~loans.final$loan_status)

# Boxplot between funded amount, grade 
boxplot(loans.final$funded_amnt~loans.final$grade)

# Removed income outliers that were greater then 2,000,000 
boxplot(loans.final$annual_inc~loans.final$grade)

# Boxplot between funded amount, home ownership 
boxplot(loans.final$funded_amnt~loans.final$home_ownership)

#===============================================================================

## Step 3: Calculate returns for each loan and determine best fitting return method

# Define the names of the four returns we'll be calculating
return_columns <- c("ret_PESS", "ret_OPT", "ret_INTa", "ret_INTb", "ret_INTc")

# Let's do a little more pre-processing and remove rows for loans which were
# paid back on the day they were issued


loans.final['loan_length'] = (loans.final['last_pymnt_d'] - loans.final['issue_d']) /30
n <- nrow(loans.final)
loans.final <- loans.final[which(loans.final$loan_length != 0), ]
print(paste("Removed",n-nrow(loans.final),"rows"))


# Use readr package to parse numbers from strings easily
if(!require(readr)) { install.packages("readr") }
library(readr)

loans.final$term <- as.character(loans.final$term)

# Parse the numbers from the term strings
loans.final['term_num'] <- parse_number(loans.final[,'term'])

# The pessimistic return method (method 2)
# Calculate the return using a simple annualized profit margin
loans.final['ret_PESS'] <- ((loans.final['total_pymnt'] - loans.final['funded_amnt']) / loans.final['funded_amnt']) * (12 / loans.final['term_num'])


# The optimistic return method (method 1)
# Assuming that if a loan gives a positive return,
# we can immediately find a similar loan to invest in,
# otherwise we use method 2 to compute the return

loans.final['ret_OPT'] <- ((loans.final['total_pymnt'] - loans.final['funded_amnt']) / loans.final['funded_amnt']) * (12 / loans.final['loan_length'])
loans.final[which(loans.final$ret_OPT < 0),'ret_OPT'] <- loans.final[which(loans.final$ret_OPT < 0),'ret_PESS']


# The reinvestment return method (method 3)

# Given an investment time horizon (in months) and reinvestment
# interest rate, calculate the return of each loan
return_method_3 <- function(t,r) {
  
  # Assuming that the total amount paid back was paid at equal
  # intervals during the duration of the loan, calculate the
  # size of each of these installment
  actual_installment <- (loans.final['total_pymnt'] - loans.final['recoveries']) / loans.final['loan_length']
  
  # Assuming the amount is immediately re-invested at the prime
  # rate, find the total amount of money we'll have by the end
  # of the loan
  cash_by_end_of_loan <- actual_installment * (1 - (1+r) ^ loans.final['loan_length']) / (1 - (1+ r))
  cash_by_end_of_loan <- cash_by_end_of_loan + loans.final['recoveries']
  
  
  # Assuming that cash is then re-invested at the prime rate,
  # with monthly re-investment, until T months from the start
  # of the loan
  remaining_months <- t - loans.final['loan_length']
  final_return <- cash_by_end_of_loan * (1+r)^remaining_months
  
  return( (12/t) * (( final_return - loans.final['funded_amnt']) / loans.final['funded_amnt']) )
}

# Apply the above function with a time horizon of 5 years and three interest rates
loans.final['ret_INTa'] <- return_method_3(5*12, 0.001)
loans.final['ret_INTb'] <- return_method_3(5*12, 0.0025)
loans.final['ret_INTc'] <- return_method_3(5*12, 0.005)

# Attempt to incorporate actual loans' interest rate to method 3
loans.final['ret_INTreal'] <- return_method_3(5*12, (loans.final$int_rate/100))

# Compare the above three methods by creating boxplots 
boxplot(loans.final$ret_PESS)
boxplot(loans.final$ret_OPT)
boxplot(loans.final$ret_INTa)
boxplot(loans.final$ret_INTb)
boxplot(loans.final$ret_INTc)
boxplot(loans.final$ret_INTreal)

#===============================================================================

## Step 3: Visualize the Variables

# Identify the type of each of these columns
float_columns <- c('loan_amnt', 'funded_amnt', 'installment', 'annual_inc',
                   'dti', 'revol_bal', 'delinq_2yrs', 'open_acc', 'pub_rec',
                   'fico_range_high', 'fico_range_low', 'total_pymnt', 'recoveries')

categorical_columns <- c('term', 'grade', 'emp_length', 'home_ownership',
                         'verification_status', 'loan_status', 'purpose')

percentage_columns <- c('int_rate', 'revol_util')

date_columns <- c('issue_d', 'earliest_cr_line', 'last_pymnt_d')

visualize_columns <- function() {
  
  # visualize continuous columns using box plots
  for(i in c(float_columns, percentage_columns, return_columns)) {
    boxplot(loans.final[,i],horizontal = TRUE,xlab=i)
    
    # print the three highest values
    #highest_vals = sort(loans.final[,i],decreasing=TRUE)[1:3]
    #smallest_val = min(loans.final[,i])
  }
  
  # Understand categorical variables by counting distinct values
  for(i in categorical_columns) {
    print(i)
    dv <- length(unique(loans.final[,i]))
    print(paste(dv,"distinct values"))
    print(sort(table(loans.final[,i]),decreasing = TRUE))
    
    print("")
    print("")
    print("")
  }
  
  # Visualize date variables 
  for(i in date_columns) {
    plot(sort(table(loans.final[,i])),xlab=i,ylab="Count")
  }
}

visualize_columns()

#===============================================================================

## Step 4: Explore returns by loan grade

# Find the percentage of loans by grade
(percentage_by_grade <- table(loans.final[,'grade']) * (100 / nrow(loans.final)))

# Find the percentage of defaults by grade
(default_by_grade <- aggregate(loans.final[,'loan_status'], list(loans.final$grade), length))

sum(loans.final$grade == 'A' & loans.final$loan_status == 'Charged Off')/sum(loans.final$grade == 'A')
sum(loans.final$grade == 'B' & loans.final$loan_status == 'Charged Off')/sum(loans.final$grade == 'B')
sum(loans.final$grade == 'C' & loans.final$loan_status == 'Charged Off')/sum(loans.final$grade == 'C')
sum(loans.final$grade == 'D' & loans.final$loan_status == 'Charged Off')/sum(loans.final$grade == 'D')
sum(loans.final$grade == 'E' & loans.final$loan_status == 'Charged Off')/sum(loans.final$grade == 'E')
sum(loans.final$grade == 'F' & loans.final$loan_status == 'Charged Off')/sum(loans.final$grade == 'F')
sum(loans.final$grade == 'G' & loans.final$loan_status == 'Charged Off')/sum(loans.final$grade == 'G')

# Get the average return by grade for each method
(returns_by_grade <- aggregate(loans.final[,return_columns], list(loans.final$grade), mean))
(returns_by_grade[,return_columns] <- returns_by_grade[,return_columns] * 100)

# Get the average interest rate by grade
(int_rate_by_grade <- aggregate(loans.final[,'int_rate'],list(loans.final$grade), mean))

combined <- returns_by_grade
# Rename the furst column obrained from aggregation from Group.1 to Grade
names(combined)[names(combined)=="Group.1"] <- "Grade"
combined['percentage'] <- as.numeric(percentage_by_grade)
combined['default'] <- default_by_grade[,'x']
combined['int_rate'] <- int_rate_by_grade[,'x']

print(combined[, c('Grade','percentage', 'default','int_rate', return_columns)])

#===============================================================================

## Step 5: Create some multi-variable summaries

# Simple histograms of the default variable versus categorical variables
# barplot is called on the table command which automatically bins the variable given to it 
# This first one shows the histograms of the defaulters
barplot(table(loans.final$loan_status),
        main="Charged Off versus Fully Paid",
        xlab="Status",
        border="black", 
        col=c("red","green"))

barplot(table(loans.final$loan_status, loans.final$grade),
        main="Distribution of Fully paid vs Charged Off by Grade",
        xlab="Grade",
        col=c("red","green"))


# Box-and-whisker plots to see dispersion of continuous vars across default and not default
# boxplot uses the formula syntax, with LHS being the y-var (vertical) and RHS being the x-variables (horizontal)
boxplot(loans.final$int_rate ~ loans.final$loan_status,main="Interest Rates across Default")
boxplot(loans.final$installment ~ loans.final$loan_status,main="Installments across Default")
boxplot(loans.final$fico_range_high ~ loans.final$loan_status, main = "FICO across Default")

#compute correlation matrix among numerical attributes in one large matrix
(correlations <- cor(loans.final[,continuous_features]))
#Install a package for plotting correlations and include it
if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}
#Generate a heat map of correlated predictors
#  (the hclust parameter orders the rows and columns according to a hierarchical clustering method)
corrplot(correlations, order="hclust")

if(!require(psych)) { install.packages("psych") }
library(psych)
# Sample 1000 data points to create pairwise plots of correlations between continuous variables
pairs.panels(loans.final[sample(nrow(loans.final),size=1000),float_columns])

#===============================================================================

## Step 6: PCA and K-Means Analysis

# key columns for grade: fico high, fico low, int_rate, annual_inc, loan amount, and emp length

# convert employment length to numeric values
loans.final$emp_length <- as.character(loans.final$emp_length)
emp_length <- parse_number(loans.final[,'emp_length'])

loans.analysis <- loans.final[,c('fico_range_high','fico_range_low','int_rate', 'annual_inc','loan_amnt','grade')]
loans.analysis['emp_length'] <- emp_length

summary(loans.analysis)

# Remove rows where emp_length has an NA value
loans.analysis <- na.omit(loans.analysis)
summary(loans.analysis)
print(loans.analysis[1:5,])


# Standardize the data set with mean 0 and variance 1 scale
# Exclude col number 6 corresponding to the grade
sc.loans.analysis <- scale(loans.analysis[,-6])
summary(sc.loans.analysis)
print(sc.loans.analysis[1:5,])

# Perform K-Means analysis with K = 4, then compare this with the given grades

cluster_columns <- c('fico_range_high','fico_range_low','int_rate', 'annual_inc','loan_amnt','emp_length')
set.seed(1248765792)
clusters4 <- kmeans(sc.loans.analysis[,cluster_columns],centers=4)

confusion_df <- table(loans.analysis$grade, clusters4$cluster)
addmargins(confusion_df)
# Get row percentages
round(100*prop.table(confusion_df,1),digits=2)
# Get column percentages
round(100*prop.table(confusion_df,2),digits=2)

# Now we explore clustering for different values of K
K_max = 10
wcss = numeric(10)
for(K in 1:K_max) {
  clustersK <- kmeans(loans.analysis[,cluster_columns],centers=K)
  wcss[K] <- clustersK$tot.withinss
}
plot(1:K_max,wcss,main="Within Cluster Sum of Squares vs K",xlab = 'K',ylab = 'WCSS')
lines(1:K_max,wcss)

# Summarize the clusters by looking at the centroids and a parallel lines plot 
# !! set your cluster names !!,  update these after you have summarized the clusters below
K=4
knames=as.character(1:K)  # default is just name them 1, 2, ...
#knames=c("1","2","3"."4")    # edit the string, make sure you have one label for each kvalue
# summarize the centroids
round(clusters4$centers,2)   # print the centroid values for each cluster
# create a parallel plot to visualize the centroid values (scales cex changes the size of text font)
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
parallelplot(clusters4$centers,auto.key=list(text=knames,space="top",columns=1,lines=T),scales=list(cex=.5))

# a parallel plot with just a few variables, since the previous plot is quite dense; 
# +++ try different lists of variables +++
shortvarlist=c('fico_range_low', 'annual_inc','emp_length')   # short list of variables, note: add variables if you like
round(clusters4$centers[,shortvarlist],2)
parallelplot(clusters4$centers[,shortvarlist],varnames=shortvarlist,auto.key=list(text=knames,space="top",columns=3,lines=T))
# let's do a pairwise plot with the short list of variables
pairs(loans.analysis[shortvarlist],col=clusters4$cluster)

## short list of 4 variables 
shortvarlist=c('fico_range_low', 'annual_inc','int_rate','loan_amnt')   # short list of variables, note: add variables if you like
round(clusters4$centers[,shortvarlist],2)
parallelplot(clusters4$centers[,shortvarlist],varnames=shortvarlist,auto.key=list(text=knames,space="top",columns=4,lines=T))
# let's do a pairwise plot with the short list of variables
pairs(loans.analysis[shortvarlist],col=clusters4$cluster)

# PCA


pcs <- prcomp(loans.analysis[,cluster_columns],scale=TRUE)

summary(pcs)
print(pcs)

biplot(pcs, scale=0)

# The stdevn of each component 
pcs$sdev
# squaring which gives the variance explained by each of these components
pr.var=pcs$sdev^2
pr.var
# Percentage of variance explained by each component can be obtained by normalizing 
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')

# create heatmap
if (!require(corrplot)) {install.packages("corrplot")}
library(corrplot)
corrplot(pcs$rotation,main="Correlation of each feature on PC's")



#===============================================================================

## Step 7: Resave the data

write.csv(loans.analysis,'loans_analysis.csv')
write.csv(loans.final,'loans_data.csv')
write.csv(continuous_features,'cont_features.csv')
write.csv(discrete_features,'disc_features.csv')


