#Clear Environment
rm(list=ls())
#load libraries
x= c("corrplot", "ggplot2", "dplyr","rcompanion","mlr", "caTools","MASS","Metrics","randomForest","C50","corrgram")
lapply(x, require, character.only = TRUE)
rm(x)
#Set working directory
setwd("F:/EdwisorVanusha/Project/Given")
#Get working directory
getwd()
#load data
bike_rental=read.csv("bike_rental.csv")
##########################################Exploratory Data Analysis#########################################
class(bike_rental)
dim(bike_rental)
names(bike_rental)
head(bike_rental)
str(bike_rental)
summary(bike_rental)
#Remove the instant variable (as it is index in dataset),Remove date variable (as we have to predict count on seasonal basis not date basis), 
bike_rental=bike_rental[,!(names(bike_rental) %in% c("instant","dteday"))]
#check the remaining variables-
names(bike_rental)
#rename the variables
names(bike_rental)[1]="Season"
names(bike_rental)[2]="Year"
names(bike_rental)[3]="Month"
names(bike_rental)[4]="Holiday"
names(bike_rental)[5]="Weekday"
names(bike_rental)[6]="Workingday"
names(bike_rental)[7]="Weather"
names(bike_rental)[8]="Temperature"
names(bike_rental)[9]="Atemperature"
names(bike_rental)[10]="Humidity"
names(bike_rental)[11]="Windspeed"
names(bike_rental)[12]="Casual"
names(bike_rental)[13]="Registered"
names(bike_rental)[14]="Count"
str(bike_rental)
##########################################Missing Values Analysis#########################################
#check missing values in the dataset
numeric_index = sapply(bike_rental,is.numeric) #selecting only numeric
numeric_data = bike_rental[,numeric_index]
cnames = colnames(numeric_data)

miss_val_bike_rental=sum(is.na(cnames))
miss_val_bike_rental
#missing values=0
##########################################Data Manipulation;#######################################################
bike_rental$Season <- as.factor(bike_rental$Season)
levels(bike_rental$Season) <- c("spring", "summer", "fall", "winter")
bike_rental$Year <- as.factor(bike_rental$Year)
levels(bike_rental$Year) <- c(2011, 2012)
bike_rental$Month <- as.factor(bike_rental$Month)
levels(bike_rental$Month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
bike_rental$Holiday <- as.factor(bike_rental$Holiday)
levels(bike_rental$Holiday) <- c("Not Holiday", "Holiday")
bike_rental$Weekday <- as.factor(bike_rental$Weekday)
levels(bike_rental$Weekday) <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
bike_rental$Workingday <- as.factor(bike_rental$Workingday)
levels(bike_rental$Workingday) <- c("Holiday", "Workingday")
bike_rental$Weather<- as.factor(bike_rental$Weather)
levels(bike_rental$Weather) <- c("Clear", "Cloudy", "Rainy")
str(bike_rental)
#########################################Outlier Analysis#########################################################################
# ## BoxPlots - Distribution and Outlier Check
outlierKD <- function(bike_rental, var) {
  var_name <- eval(substitute(var), eval(bike_rental))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
  boxplot(var_name, main = "With outliers")
  hist(var_name,
       main = "With outliers",
       xlab = NA,
       ylab = NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main = "Without outliers")
  hist(var_name,
       main = "Without outliers",
       xlab = NA,
       ylab = NA)
  title("Outlier Check", outer = TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name)) *
                                            100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  
}
outlierKD(bike_rental, Temperature)
# Outliers identified: 0 nPropotion (%) of outliers: 0 nMean of the outliers: NaN nMean without removing outliers: 0.5 nMean if we remove outliers: 0.5 n
outlierKD(bike_rental, Atemperature)
#Outliers identified: 0 nPropotion (%) of outliers: 0 nMean of the outliers: NaN nMean without removing outliers: 0.47 nMean if we remove outliers: 0.47 n
outlierKD(bike_rental, Humidity)
#Outliers identified: 2 nPropotion (%) of outliers: 0.3 nMean of the outliers: 0.09 nMean without removing outliers: 0.63 nMean if we remove outliers: 0.63 n
outlierKD(bike_rental, Registered)
#Outliers identified: 0 nPropotion (%) of outliers: 0 nMean of the outliers: NaN nMean without removing outliers: 3656.17 nMean if we remove outliers: 3656.17 n
outlierKD(bike_rental, Casual)
#Outliers identified: 44 nPropotion (%) of outliers: 6.4 nMean of the outliers: 2661.95 nMean without removing outliers: 848.18 nMean if we remove outliers: 732.01 n
outlierKD(bike_rental, Count)
#Outliers identified: 0 nPropotion (%) of outliers: 0 nMean of the outliers: NaN nMean without removing outliers: 4504.35 nMean if we remove outliers: 4504.35 n
##################################Feature Selection################################################
## Correlation Plot 
library(corrgram)
corrgram(bike_rental[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Findings: Temperature and Atemperature are highly correlated, casual and registered are removed 
#because count= registered+casual, we have to predict count

# Plotting Dependent variable to check for normality 
ggplot(data = bike_rental, aes(Count)) +
  geom_histogram(aes(
    y = ..density..,
    binwidth = .10,
    colour = "blue"
  ))
#Dependent variable= count is normally  distributed

## Chi-squared Test of Independence
factor_index = sapply(bike_rental,is.factor)
factor_data = bike_rental[,factor_index]
for (i in 1:10)
{
  print(names(factor_data)[i])
  print(chisq.test(table(numeric_data$Count,factor_data[,i])))
}
# As p-values of all variables are higher than 0.05, we can reject null hypothesis and say that the variables are independent

## Dimension Reduction
bike_rental = subset(bike_rental, 
                         select = -c(Atemperature, Casual, Registered))
bike_rental
##################################Feature Scaling################################################
#Normality check
qqnorm(bike_rental$Count)
hist(bike_rental$Count)


#As already given in problem statement varaibles like Temperature, Humidity, Windspeed are already normalised so now only target variable should be normalised.
#Normalisation
cnames = c("Count")
cnames
for(i in cnames){
  print(i)
  bike_rental[,i] = (bike_rental[,i] - min(bike_rental[,i]))/
    (max(bike_rental[,i] - min(bike_rental[,i])))
}
bike_rental
##########################Univariate Analysis######################################################################### 
# 1. Continous predictors
univariate_continuous <- function(dataset, variable, variableName) {
  var_name = eval(substitute(variable), eval(dataset))
  print(summary(var_name))
  ggplot(data = dataset, aes(var_name)) +
    geom_histogram(aes(binwidth = .8, colour = "black")) +
    labs(x = variableName) +
    ggtitle(paste("count of", variableName))
}

univariate_continuous(bike_rental, Count, "Count")
univariate_continuous(bike_rental, Temperature, "Temperature")
univariate_continuous(bike_rental, Atemperature, "Atemperature")
univariate_continuous(bike_rental, Humidity, "Humidity") # skwed towards left
univariate_continuous(bike_rental, Windspeed, "Windspeed") #skewed towards right
univariate_continuous(bike_rental, Casual, "Casual") # skwed towards right
univariate_continuous(bike_rental, Registered, "Registered")

#2. categorical variables
univariate_categorical  <- function(dataset, variable, variableName) {
  variable <- enquo(variable)
  
  percentage <- dataset %>%
    dplyr::select(!!variable) %>%
    group_by(!!variable) %>%
    summarise(n = n()) %>%
    mutate(percantage = (n / sum(n)) * 100)
  print(percentage)
  
  dataset %>%
    count(!!variable) %>%
    ggplot(mapping = aes_(
      x = rlang::quo_expr(variable),
      y = quote(n),
      fill = rlang::quo_expr(variable)
    )) +
    geom_bar(stat = 'identity',
             colour = 'white') +
    labs(x = variableName, y = "count") +
    ggtitle(paste("count of ", variableName)) +
    theme(legend.position = "bottom") -> p
  plot(p)
}

univariate_categorical(bike_rental, Season, 'Season')
univariate_categorical(bike_rental, Year, "Year")
univariate_categorical(bike_rental, Month, "Month")
univariate_categorical(bike_rental, Holiday, "Holiday")
univariate_categorical(bike_rental, Weekday, "Weekday")
univariate_categorical(bike_rental, Workingday, "Workingday")
univariate_categorical(bike_rental, Weather, "Weather")
##########################bivariate Analysis#################################################################
# bivariate analysis for categorical variables
bivariate_categorical <-
  function(dataset, variable, targetVariable) {
    variable <- enquo(variable)
    targetVariable <- enquo(targetVariable)
    
    ggplot(
      data = dataset,
      mapping = aes_(
        x = rlang::quo_expr(variable),
        y = rlang::quo_expr(targetVariable),
        fill = rlang::quo_expr(variable)
      )
    ) +
      geom_boxplot() +
      theme(legend.position = "bottom") -> p
    plot(p)
    
  }

bivariate_continous <-
  function(dataset, variable, targetVariable) {
    variable <- enquo(variable)
    targetVariable <- enquo(targetVariable)
    ggplot(data = dataset,
           mapping = aes_(
             x = rlang::quo_expr(variable),
             y = rlang::quo_expr(targetVariable)
           )) +
      geom_point() +
      geom_smooth() -> q
    plot(q)
    
  }

bivariate_categorical(bike_rental, Season, Count)
bivariate_categorical(bike_rental, Year, Count)
bivariate_categorical(bike_rental, Month, Count)
bivariate_categorical(bike_rental, Holiday, Count)
bivariate_categorical(bike_rental, Weekday, Count)
bivariate_categorical(bike_rental, Workingday, Count)
bivariate_categorical(bike_rental, Weather, Count)

bivariate_continous(bike_rental, Temperature, Count)
bivariate_continous(bike_rental, Humidity, Count)
bivariate_continous(bike_rental, Windspeed, Count)

########################################Error Metrics#################################################
# Function for calculating Mean Absolute Error
MAE <- function(actual,predicted){
  error = actual - predicted
  mean(abs(error))
}

###################################Model Development#######################################
#-------------------------Model 1 Random forest ------------------------------------------------
set.seed(700)
split <- sample.split(bike_rental$Count, SplitRatio = 0.80)
train <- subset(bike_rental, split == TRUE)
test <- subset(bike_rental, split == FALSE)

modelRF <- randomForest(Count ~.,
                       data = train,ntree = 500, mtry = 8, importance = TRUE)
print(modelRF)
par(mfrow = c(1,1))
plot(modelRF)

# 300 trees selected from the plot

tumedmodel <- tuneRF(train[,1:10], train[,11], stepFactor = 0.5, plot = TRUE, 
                     ntreeTry = 250, trace = TRUE, improve = 0.05)
# selected mtry = 6 from the plot

tuned_randomForest <-  randomForest(Count ~ . ,
                                    data = train,ntree = 250, mtry = 6, importance = TRUE)
tuned_randomForest

# predicting using random forest model 1
rf1_prediction <- predict(tuned_randomForest,test[,-11])
rmse(rf1_prediction,test$Count)
print(paste("Mean Absolute Error for Random forest regressor  is ",
            MAE(test$Count,rf1_prediction)))

# Tuned Random Forest

varImpPlot(tuned_randomForest)

#rmse(rf1_prediction,test$Count): 0.07623436


#-------------------------Model 2 Linear Regression------------------------------------------------

model1 <- lm(Count ~ ., data = train)
# step wise model selection
modelAIC <- stepAIC(model1, direction = "both")
summary(modelAIC)

# Apply prediction on test set
test_prediction <- predict(modelAIC, newdata = test)
test_rmse <- rmse(test$Count, test_prediction)
print(paste("root-mean-square error for linear regression model is ", test_rmse))
print(paste("Mean Absolute Error for linear regression model is ",MAE(test$Count,test_prediction)))
print("summary of predicted count values")
summary(test_prediction)
print("summary of actual Count values")
summary(test$Count)
# root-mean-square error for linear regression model is  0.0843791592998878
predictions_LR = predict(model1, test[,1:10])


# Random forest is performing better than linear regression.
# Model input and output for linear regression and Random forest
write.csv(test, file = "InputLinearRegressionR.csv")
write.csv(test, file = "InputRandomForestR.csv")
write.csv(predict_test_nonlog, file="outputLogisticRegressionR.csv")
