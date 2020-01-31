
###### Checking the current working directory

getwd()

###### loading the Libraries

library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)

###### GET THE DATA

# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Student Performance Data Set

###### Reading the csv file

df <- read.csv('data/student-mat.csv', sep= ';')

#### Exploratory Data Analysis

## Checking the head and tail of the dataframe

head(df)

tail(df)

## Checking the structure of the dataframe

str(df)

## Checking the summary of the dataframe

summary(df)

### Checking if there is any null values or NA values in the dataframe

any(is.na(df))  
# We got a FALSE as output which means that there are no NA values.

### Making sure that Categorical variables have a factor set to them

str(df) 
# Factor levels are maintained, so no need for any changes.

### Using ggplot2 to Explore the data a bit more

# Creating a sort of plot that allows us to understand the correaltions between each of the features
# Correlation means a statistical relationship between two variables.
# Correlation plots are a great way of exploring data and Seeing if there are any interaction terms

# Checking the Correlation between the numeric variables
# Num only

num.cols <- sapply(df,is.numeric)

# Filtering the numeric columns for correlation

cor.data <- cor(df[,num.cols])
print(cor.data)

### Visualizing the correlation data

# installing and loading the packages
# install.packages('corrgram')  # To visualize a  Correlation Matrix for the entire data frame    
# install.packages('corrplot')  # To visualize a  Correlation Matrix (Only for Numeric Data)    

library(corrplot)
library(corrgram)

help("corrplot")
help("corrgram")

## Plotting the correlation plot only for the numeric data in the data frame

print(corrplot(cor.data, method = "color"))
# Here the Diagonal dark blue means 'Perfect Correlation', 
# because every feature is perfectly correlated to itself

# There is a High correlation between G1, G2, G3.
# It makes sense because if u did a good grade on your first period, 
# you will probably have a good grade on your second and third period.
# OR
# if u had a bad grade on your first period, 
# you will probably have a bad grade on your second and third period.  

# It also makes sense that having a high grade is inversely correlated to the failure features.

# The mother and father education levels are also correlated,
# as it kind of makes sense that usually couples pair up at similar educational levels, not always.


# Plotting the correlation plot of the data frame

corrgram(df)
# All the above conclusions can be drawn from here too.

corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
# It has a diagonal of all features
# The lower panel is the regular shaded box the correlation
# The upper panel is a pie chart depicting the correlation,
# Here Blue indicates Positive Correlation,  
# And Red indicates Negative Correlation 
# where a full pie-chart would be equal to 1(Perfectly Correlated)

### Plotting the Histogram for the Grades

ggplot(df, aes(x= G3)) + geom_histogram(bins= 20, alpha= 0.5, fill= 'blue')

ggplot(df, aes(x= G2)) + geom_histogram(bins= 20, alpha= 0.5, fill= 'Red')

ggplot(df, aes(x= G1)) + geom_histogram(bins= 20, alpha= 0.5, fill= 'Black')

# There are a bunch of Zero(0) G3, G2, G1 values.. 
# This brings out a question, like are students missing the test? 
# Also why is the mean occurence so high? 
# Is this test curved?

####  Continuing to build the model

# Splitting data into Train and Test Set
# We will use the training data to train the Model(Train Set)
# And We'll use the testing data to make Predictions from the Trained Model(Test Set)

# Installing caTools package
# It makes it really easy to Randomly split up the data into Training and Testing Set

# install.packages('caTools')

library(caTools)

# Set A Seed (Setting a random seed)
# The reason for setting a seed is that since the splits are going to be based on Random sequence, 
# set.seed() ensures that you get the same result if you start with seed each time you run the same Process.

set.seed(101)

### Splitting up the data into Sample

# When you call sample.split()
# It splits the data from a vector in two sets from a pre-defined ratio
# And then we take in the data frame and pass a column of the data frame
# It will actually work for any column in the data frame
# But juat by convention we pass in the column that we are trying to predict
# In this Case We are trying to Predict the final third period score OR Grade of the students,
# that is G3.
# Then we put a split ratio

# The split ratio is going to be the Percentage of the data that we want to use for training.
# By convention we use 70% of the data as training data
# And 30% of the data as testing data

sample <- sample.split(df$G3,SplitRatio = 0.7)

# It will just create a new column called sample on the data frame, 
# And it randomly puts in a Boolean or a Logical value Either (True/False)

# Creating a new data frame called train using the subset of the data frame for the condition,
# where sample == TRUE

train <- subset(df,sample == TRUE)
# 70% of the data -> train

test <- subset(df, sample == FALSE)
# 30% of data -> test

# The general model of building a Linear Regression Model in R Looks like this:
# model <- lm(y ~ x1 + x2, data)
# we call the lm() function and pass in the feature column that we are trying to Predict,
# tilde sign and add up the features we want to use in order to make that Prediction.
# Or to use all the features from the data frame
# model <- lm(y ~ ., data)

###### TRAIN and BUILD MODEL

model <- lm(G3 ~ ., data = train)
# model <- lm(G3 ~ absences + G1 + G2 + age + activities, data = train)
###### Interpreting the MODEL

summary(model)

####### Looks like Absences, G1, and G2 scores are good predictors, With age and activities also possibly contributing to a good model. ######
 
### Visualizing the Model by plotting out the Residuals

# Residuals are the diffrence between the actual data points and 
# our predicted Linear Regression Model 
# We always try to miimize the residual value
res <- residuals(model)

class(res)
# It gives a numeric vector 

# calling as.dataframe() on residuals i.e. res, that can be used to plot out the Residuals

res <- as.data.frame(res)
class(res)
head(res)

## plotting the Residuals

ggplot(res, aes(res)) + geom_histogram(fill='blue', alpha=0.5)

# We want a histogram of our residuals to be normally distributed.
# Because if our Residuals are Normally Distributed, this indicates the mean othe the
# diffrence between our Predictions and the actual values is close to ZERO(0).
# Which means that when we miss, we are missing both the short and long of the actual values
# and the liklehood of a miss being far from the actual values gets smaller 
# as the distance from the actual value gets larger

# Looks like there are some suspicious residual values that have a value less than -5.
# The model is Prdicting negative test scores for really poor performing students.
# However the lowest score possible on a test is zero(0)

### Advanced Visualization with the Model

plot(model)

##There are various Plots such as: 
# Residuals vs Fitted values
# Normal Q-Q PLot
# the scale-Location Plot
# the Residuals vs Leverage Plot

###### PREDICTIONS
# Testing our model by predicting on our testing set

G3.predictions <- predict(model,test)

# Transforming G3.predictions into data frame and combing it to the actual values

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted', 'actual')
results <- as.data.frame(results)  
head(results) 

# Now let's take care of negative predictions

# When we plotted out the Residual values, we noticed that we had a large amount of negative values.
# The reason for that was because our model was predicting negative final score test values.

to_zero <- function(x){
  if (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Applying the ZERO Function

results$predicted <- sapply(results$predicted, to_zero) 

min(results$predicted)
# The minimum value is now zero(0), hence taken care of negative predictions.

### MEAN SQUARED ERROR (MSE)

mse <- mean( (results$actual - results$predicted)^2 )
mse

### ROOT MEAN SQUARED ERROR (RSME)

mse^0.5

### SUM of SQUARED ERROR (SSE)

SSE <- sum( (results$predicted - results$actual)^2 )
SSE

### SUM of SQUARED TOTAL (SST)

SST <- sum( (mean(df$G3) - results$actual)^2 )
SST

### R-SQUARED

R2 <- 1 - SSE/SST
R2

# The R-SQUARED is 0.8, which is not so bad for the test data.

# It means that we are Explaining about 80% Variance on the Test data.
