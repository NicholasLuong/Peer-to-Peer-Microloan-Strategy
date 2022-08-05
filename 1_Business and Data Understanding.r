# Week1.r
# # Phase 1 - Ingestion and Cleaning

# This notebook carries out the following steps
#   - Ingest data downloaded from the LendingClub website
#   - Prepare the data set
#   - Visualizes single variable summaries
#   - Removes outliers
#   - Output a combined dataset ready for analysis

# Ensure that the file LoanStats3a_securev1.csv is in the current working
# directory when this notebook is run.
# Change the name of the csv file if you want to analyze other years



#===============================================================================

### Step 1: Define Parameters

# Columns we are interested in
columns_to_pick <- c('id','loan_amnt','funded_amnt','term','int_rate',
                'installment','grade','emp_length', 'home_ownership',
                'annual_inc','verification_status','issue_d',
                'loan_status','purpose','dti', 'delinq_2yrs',
                'earliest_cr_line','open_acc','pub_rec', 'fico_range_high',
                'fico_range_low', 'revol_bal','revol_util', 'total_pymnt',
                'last_pymnt_d', 'recoveries')


# Identify the type of each of these columns
float_columns <- c('loan_amnt', 'funded_amnt', 'installment', 'annual_inc',
              'dti', 'revol_bal', 'delinq_2yrs', 'open_acc', 'pub_rec',
              'fico_range_high', 'fico_range_low', 'total_pymnt', 'recoveries')

categorical_columns <- c('term', 'grade', 'emp_length', 'home_ownership',
            'verification_status', 'loan_status', 'purpose')

percentage_columns <- c('int_rate', 'revol_util')

date_columns <- c('issue_d', 'earliest_cr_line', 'last_pymnt_d')

# Check if each column has a type
if( !all(sort(columns_to_pick) == sort(c(float_columns,categorical_columns,percentage_columns,date_columns,'id'))) ) {
  stop("Error: Not all data columns are classified by type")
}



# discrete and continuous features

# All categorical columns other than "loan_status" will be used as
# discrete features
discrete_features <- categorical_columns[categorical_columns != 'loan_status']

# All numeric columns will be used as continuous features
continuous_features <- c(float_columns,percentage_columns)



#===============================================================================

### Step 2: Read the Data

# Read all the lines from the data file
#loans.full <- readLines("LoanStats3a_securev1.csv")
# Skip the first line of the file (the header is contained in the second line)
#loans.full <- loans.full[-1]
# Read the data into a data frame
#loans.full <- read.csv(textConnection(loans.full))

# Skip the first line of the file (the header is contained in the second line)
loans.full <- read.csv("LoanStats3a_securev1.csv", skip=1L)

# Filter out garbage lines (lines without an integer ID)

# Filtering function, checks the values in the "id" column
is_ID <- function(x) {
  # cast to an integer value, yields NA if not possible
  y <- as.integer(x)
  # check if integer and not NA
  return(is.integer(y) & !is.na(y))
}

intidx = is_ID(loans.full$id)

# Apply the filtering function
loans.filtered <- loans.full[intidx,]

# Get column names
columns <- colnames(loans.filtered)

# Display summaries of the data (uncomment the next two lines if interested)
str(loans.filtered)
summary(loans.filtered)




#===============================================================================

### Step 3: Prepare the data set

# Keep only the columns of interest
loans.final <- loans.filtered[columns_to_pick]  # Check w/ Ravi about copying
print(paste("Starting with",nrow(loans.final),"rows"))

str(loans.final)
summary(loans.final)


# Typecast the columns, i.e. give each variable the type you want it to have in the analysis

# Handle the numeric/float columns
loans.final[, float_columns] <- sapply( loans.final[, float_columns], as.numeric)

# Function for processing the percentage columns
clean_percentage <- function(x){
  if(is.na(x) | is.null(x) | is.nan(x)){
    return(NA)
  } else {
    return(as.numeric(substr(x,1,nchar(x)-1)))
  }
}


loans.final[, percentage_columns] <- sapply( loans.final[, percentage_columns], as.character)

# Apply the function to the dataset
for (i in percentage_columns) {
  loans.final[,i] <- sapply(loans.final[,i], clean_percentage)
}

# Convert data columns to characters before processing them
loans.final[, date_columns] <- sapply( loans.final[, date_columns], as.character)

# Clean the date columns
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
  loans.final[,i] <- sapply(loans.final[,i], clean_date)
}



# Clean the categorical columns
clean_cat <- function(x) {
  if(is.na(x) | is.null(x) | is.nan(x)) {
    return(NA)
  } else {
    return(x)
  }
}

for (i in categorical_columns) {
  loans.final[,i] <- sapply(loans.final[,i], clean_cat)
}



#===============================================================================

### Step 4: Visualize the dataset

# Define the visualization routine
visualize_data <- function() {

  # Visualize continuous variables using box plots
  for(i in c(float_columns,percentage_columns)) {
    boxplot(loans.final[,i],horizontal = TRUE,xlab=i)
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

# Display the visualizations
visualize_data()


#===============================================================================

### Step 5: Handle outliers

# There are quite a few outliers, but the two most obvious
# ones to remove are in annual_inc, revol_bal.  
# Let's remove these.

n <- nrow(loans.final)
loans.final <- loans.final[ which(loans.final$annual_inc <1000000) ,]
loans.final <- loans.final[ which(loans.final$revol_bal < 200000) ,]
print(paste("Removed",n-nrow(loans.final),"rows"))

# Remove all loans that are too recent to have been paid off or
# defaulted
n <- nrow(loans.final)
loans.final <- loans.final[which(loans.final$loan_status %in% c("Fully Paid","Charged Off", "Default")) ,]
print(paste("Removed",n-nrow(loans.final),"rows"))

# Only include loans isssued since 2009
loans.final <- loans.final[ which(as.Date(loans.final$issue_d, origin = "1970-01-01") >= "2009-01-01") ,]

# Now re-visualize the data

visualize_data()

# Before saving the data, lets drop null values

# Deal with null values. We allow categorical variables to be null
# OTHER than grade, which is a particularly important categorical.
# All non-categorical variables must be non-null, and we drop
# rows that do not meet this requirement
required_columns <- columns_to_pick[!(columns_to_pick %in% c(categorical_columns,"id"))]
required_columns <- c(required_columns,"grade")

n <- nrow(loans.final)
loans.final <- na.omit(loans.final)
# loans.final[,required_columns] <- na.omit(loans.final[,required_columns])
print(paste("Removed",n-nrow(loans.final),"rows"))


# Remove the total_pymnt from the list of continuous features; this
# variable is highly predictive of the outcome but is not known at
# the time the loan is issued
continuous_features <- continuous_features[!(continuous_features %in% c("total_pymnt", "recoveries"))]


#===============================================================================

# Step 6: Save the data for easy loading later on

write.csv(loans.final,'loans_data.csv')
write.csv(continuous_features,'cont_features.csv')
write.csv(discrete_features,'disc_features.csv')

# Data can be read back into storage using the following commands
# We will drop the first column that read.csv adds as it isn't necessary
loans.final <- read.csv('loans_data.csv')[-1]
continuous_features <- as.vector(read.csv('cont_features.csv')[[-1]])
discrete_features <- as.vector(read.csv('disc_features.csv')[[-1]])

# Alternate: save as Rda files
#save(loans.final,file="loans_data.Rda")
#save(continuous_features,file="cont_features.Rda")
#save(discrete_features,file="disc_features.Rda")
 

