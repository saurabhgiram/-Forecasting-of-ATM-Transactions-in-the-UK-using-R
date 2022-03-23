# Library and dataset import ----------------------------------------------

#Importing libraries
library("forecast")
library("tsutils")
library("imputES")
library("tseries")
library("readxl")
library("xts")
library("seastests")
library("tinytex")
library("tsibble")
library("dplyr")
library("outliers")
library("moments")
library("VIM")
library("naniar")
library("ggplot2")
library("imputeTS")
library("knitr")
library("regclass")


#Import sdata
dataset <- read_excel("Assignment 1 Data.xls")

#colnames(data) <- c("Date","Transactions")
colnames(dataset) <- c("Transactions")

#Converting data to time series
dataset <- ts(dataset, frequency = 7, start = c(1996,77))

# Histogram of whole distribution before cleaning the data set
hist(dataset)

# Missing values and outliers distribution ---------------------------------------------

# Missing values distribution
ggplot_na_distribution(dataset)
ggplot_na_intervals(dataset)
ggplot_na_gapsize(dataset)

#Filing missing values

# Approach 1 -Replace with corresponding day of week data
which_na(dataset)

# Approach 2 - Replace with interpolation method
dataset <- na_interpolation(dataset)

summary(dataset)
plot(dataset)
which_na(dataset)

#daily <- as.xts(unlist(list(dataset1)),order.by=as.Date(data$Date))
#plot(daily, type = "l", col = "BLACK")

# Box plot the outliers
boxplot(dataset)
# Check how many are the outliers
boxplot.stats(dataset)

# Statistical test for outliers
grubbs.test(dataset,type = 11, opposite = FALSE, two.sided = TRUE)
chisq.out.test(dataset)

# Find the outliers in the series
out <- boxplot.stats(dataset)$out 
out
out_ind <- which(dataset %in% c(out))
out_ind
dataset[c(163,167,196,235,265,283,538,557,558,559,561,562,563,565,586,587,592,593,594,600
                 ,621,628,649,670,684,691,698,711,712,726)]

#Replacing the outliers with median values 
series_median = median(dataset)
series_median
dataset[c(163,167,196,235,265,283,538,557,558,559,561,562,563,565,586,587,592,593,594,600
                 ,621,628,649,670,684,691,698,711,712,726)] = series_median


# Summary of dataset
summary(dataset)

# Box plot the outliers after replacing previously identified outliers with series median
boxplot(dataset)
# Check how many are the outliers after replacing previously identified outliers with series median
boxplot.stats(dataset)

# Length of data set 
length(dataset)

# Frequency of dataset 
frequency(dataset)

# Visualize the time series of data sES
plot(dataset)

# Histogram of whole distribution
hist(dataset) 

#Test for trend
trendtest(dataset)

#Checking for seasonality
isSeasonal(dataset, freq = 365)
isSeasonal(dataset, freq= 7)
isSeasonal(dataset, test = "combined")

seastests::welch(dataset)

#Decomposition of the dataset 3 using decomp function
decomp_dataset <- decomp(dataset,outplot = TRUE)

# Plot the season
seasplot(dataset,m = 7)

#ACF and PACF analysis on the entire dataset
tsdisplay(dataset)

# Perform KPSS and ADF test on time series
kpss.test(dataset)
adf.test(dataset)

# Split into training and test --------------------------------------------

# Find the total number of observations
dataset_length <- length(dataset)
# Write down size of training set
dataset_train_length <- 721

# Split into training and test
dataset_train_length<- 721
dataset_train <- ts(dataset[1:dataset_train_length], frequency = 7)
length(dataset_train)
dataset_test <- dataset[(dataset_train_length+1):length(dataset)]
length(dataset_test)

#Setting the horizon
h=14

# Rolling origin ----------------------------------------------------------

# Set horizon and number of rolling origins
H <- 14 #42 # 14 # 32
origins <- 14 #14
dataset_length <- length(dataset)
dataset_rolling_train_length <- dataset_length - H - origins + 1
dataset_rolling_test_length <- H + origins - 1
dataset_rolling_train <- ts(dataset[1:dataset_rolling_train_length],
                            frequency=frequency(dataset),
                            start=start(dataset))
dataset_rolling_test <- dataset[(dataset_rolling_train_length+1):dataset_length]


# Arithmetic Mean model ---------------------------------------------------

#Arithmetic mean model
Arithmetic_mean_dataset<- mean(dataset_train)
Forecast_Arithmetic_mean_dataset<- forecast(Arithmetic_mean_dataset,h=h)$mean
Forecast_Arithmetic_mean_dataset
#Error Measures for Arithmetic mean model
Arithmetic_errors_dataset<- dataset_test - Forecast_Arithmetic_mean_dataset
Arithmetic_ME_dataset<- mean(Arithmetic_errors_dataset) #Mean error
Arithmetic_MSE_dataset<- mean(Arithmetic_errors_dataset^2) #Mean squared error
Arithmetic_MAE_dataset<- mean(abs(Arithmetic_errors_dataset)) #Mean absolute error
Arithmetic_MAPE_dataset<- 100 * mean(abs(Arithmetic_errors_dataset)/dataset_test) #Mean absolute percentage error
Arithmetic_RMSE_dataset<- sqrt(mean(Arithmetic_errors_dataset^2)) #Root mean squared error

Arithmetic_errors_dataset
## Rolling origin for Arithmetic mean

dataset_rolling_forecasts_Arithmeticmean <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Arithmeticmean <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Arithmeticmean) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Arithmeticmean) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Arithmeticmean) <- dimnames(dataset_rolling_forecasts_Arithmeticmean)

for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Arithmeticmean[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Arithmeticmean[i,] <- forecast(mean(dataset_rolling_train_set),h=H)$mean
}

## MAPE for Rolling origin of Arithmetic mean
colMeans(abs(dataset_rolling_holdout_Arithmeticmean - dataset_rolling_forecasts_Arithmeticmean))
Rolling_errors_dataset_Arithmetic<- dataset_rolling_holdout_Arithmeticmean - dataset_rolling_forecasts_Arithmeticmean
Rolling_ME_errors_dataset_Arithmetic<- mean(Rolling_errors_dataset_Arithmetic) #Mean error
Rolling_MSE_errors_dataset_Arithmetic<- mean(Rolling_errors_dataset_Arithmetic^2) #Mean squared error
Rolling_MAE_errors_dataset_Arithmetic<- mean(abs(Rolling_errors_dataset_Arithmetic)) #Mean absolute error
Rolling_MAPE_dataset_Arithmetic<- 100 * mean(abs(Rolling_errors_dataset_Arithmetic)/dataset_rolling_holdout_Arithmeticmean)
Rolling_RMSE_errors_dataset_Arithmetic<- sqrt(mean(Rolling_errors_dataset_Arithmetic^2)) #Root mean squared error


# Create summary table for error measure of Arithmetic Mean Model
Summary_stats <- matrix(c(Arithmetic_ME_dataset,Arithmetic_MSE_dataset,Arithmetic_MAE_dataset,Arithmetic_MAPE_dataset,Arithmetic_RMSE_dataset), ncol=5, byrow=TRUE)
Summary_stats <- rbind(Summary_stats,c(Rolling_ME_errors_dataset_Arithmetic,Rolling_MSE_errors_dataset_Arithmetic,Rolling_MAE_errors_dataset_Arithmetic,Rolling_MAPE_dataset_Arithmetic,Rolling_RMSE_errors_dataset_Arithmetic))
colnames(Summary_stats) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary_stats) <- c('Arithmetic Mean Model Error Measures','Arithmetic Mean Model Error Measures with Rolling origin')
Summary_stats <- as.table(Summary_stats)
Summary_stats
names(Summary_stats) <- c("Arithmetic Mean Model Error Measures","Arithmetic Mean Model Error Measures with Rolling origin")
knitr::kable(Summary_stats)


# Simple Moving Average Model ---------------------------------------------------

#Fitting a Simple average model
SMA_dataset <- ma(dataset_train, order=8, centre=FALSE)

SMA_no_NAs_dataset <- SMA_dataset[!is.na(SMA_dataset)]
Forecast_SMA_dataset <- ts(rep(SMA_no_NAs_dataset[length(SMA_no_NAs_dataset)],h), frequency=7)

#Calculating the error measures for Simple average model
SMA_errors_dataset <- dataset_test - Forecast_SMA_dataset
SMA_ME_dataset <- mean(SMA_errors_dataset) #Mean error
SMA_MSE_dataset <- mean(SMA_errors_dataset^2) #Mean squared error
SMA_MAE_dataset<- mean(abs(SMA_errors_dataset)) #Mean absolute error
SMA_MAPE_dataset <- 100 * mean(abs(SMA_errors_dataset)/dataset_test) #Mean absolute percentage error
SMA_RMSE_dataset<- sqrt(mean(SMA_errors_dataset^2)) #Root mean squared error

## Rolling origin for SMA

dataset_rolling_forecasts_SMA <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_SMA <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_SMA) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_SMA) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_SMA) <- dimnames(dataset_rolling_forecasts_SMA)

for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_SMA[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_SMA[i,] <- forecast(ma(dataset_rolling_train_set, order=8, centre=FALSE),h=H)$mean
}

## MAPE for Rolling origin of SMA
Rolling_errors_dataset_SMA<- dataset_rolling_holdout_SMA - dataset_rolling_forecasts_SMA
Rolling_ME_dataset_SMA<- mean(Rolling_errors_dataset_SMA) #Mean error
Rolling_MSE_dataset_SMA<- mean(Rolling_errors_dataset_SMA^2) #Mean squared error
Rolling_MAE_dataset_SMA<- mean(abs(Rolling_errors_dataset_SMA)) #Mean absolute error
Rolling_MAPE_dataset_SMA<- 100 * mean(abs(Rolling_errors_dataset_SMA)/dataset_rolling_holdout_SMA)
Rolling_RMSE_dataset_SMA<- sqrt(mean(Rolling_errors_dataset_SMA^2)) #Root mean squared error

# Create summary table for error measure of SMA Model

Summary_stats <- rbind(Summary_stats,c(SMA_ME_dataset,SMA_MSE_dataset,SMA_MAE_dataset,SMA_MAPE_dataset,SMA_RMSE_dataset))
Summary_stats <- rbind(Summary_stats,c(Rolling_ME_dataset_SMA,Rolling_MSE_dataset_SMA,Rolling_MAE_dataset_SMA,Rolling_MAPE_dataset_SMA,Rolling_RMSE_dataset_SMA))
colnames(Summary_stats) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary_stats) <- c('Arithmetic Mean Model Error Measures','Arithmetic Mean Model Error Measures with Rolling origin','Simple Moving Average Model Error Measures','Simple Moving Average Model Error Measures with Rolling origin')
Summary_stats <- as.table(Summary_stats)
Summary_stats
names(Summary_stats) <- c("Arithmetic Mean Model Error Measures","Arithmetic Mean Model Error Measures with Rolling origin","Simple Moving Average Model Error Measures","Simple Moving Average Model Error Measures with Rolling origin")
knitr::kable(Summary_stats)

# Naive Model -------------------------------------------------------------

# Naive model
Naive_method_dataset <- naive(dataset_train, h=h)
Forecast_naive_dataset <- Naive_method_dataset$mean
plot(Naive_method_dataset)
#Calculating the error measures for Naive
Naive_errors_dataset<- dataset_test - Forecast_naive_dataset
Naive_ME_dataset <- mean(Naive_errors_dataset) #Mean error
Naive_MSE_dataset <- mean(Naive_errors_dataset^2) #Mean squared error
Naive_MAE_dataset<- mean(abs(Naive_errors_dataset)) #Mean absolute error
Naive_MAPE_dataset <- 100 * mean(abs(Naive_errors_dataset)/dataset_test) #Mean absolute percentage error
Naive_RMSE_dataset<- sqrt(mean(Naive_errors_dataset^2)) #Root mean squared error


## Rolling origin for Naive

dataset_rolling_forecasts_Naive <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Naive <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Naive) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Naive) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Naive) <- dimnames(dataset_rolling_forecasts_Naive)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Naive[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Naive[i,] <- naive(dataset_rolling_train_set,h=H)$mean
}

## MAPE for Rolling origin of Naive
Rolling_errors_dataset_Naive<- dataset_rolling_holdout_Naive - dataset_rolling_forecasts_Naive
Rolling_ME_dataset_Naive<- mean(Rolling_errors_dataset_Naive) #Mean error
Rolling_MSE_dataset_Naive<- mean(Rolling_errors_dataset_Naive^2) #Mean squared error
Rolling_MAE_dataset_Naive<- mean(abs(Rolling_errors_dataset_Naive)) #Mean absolute error
Rolling_MAPE_dataset_Naive<- 100 * mean(abs(Rolling_errors_dataset_Naive)/dataset_rolling_holdout_Naive)
Rolling_RMSE_dataset_Naive<- sqrt(mean(Rolling_errors_dataset_Naive^2)) #Root mean squared error

# Create summary table for error measure of Naive Model
Summary_table <- matrix(c(Naive_ME_dataset,Naive_MSE_dataset,Naive_MAE_dataset,Naive_MAPE_dataset,Naive_RMSE_dataset), ncol=5, byrow=TRUE)
Summary_table <- rbind(Summary_table,c(Rolling_ME_dataset_Naive,Rolling_MSE_dataset_Naive,Rolling_MAE_dataset_Naive,Rolling_MAPE_dataset_Naive,Rolling_RMSE_dataset_Naive))
colnames(Summary_table) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary_table) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin')
Summary_table <- as.table(Summary_table)
Summary_table
names(Summary_table) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin")
knitr::kable(Summary_table)

# Seasonal Naive Model ----------------------------------------------------

#Seasonal Naive model
SNaive_method_dataset <- snaive(dataset_train, h=h)
Forecast_Snaive_dataset <- SNaive_method_dataset$mean
plot(SNaive_method_dataset)
#Calculating the error measures for Naive
SNaive_errors_dataset<- dataset_test - Forecast_Snaive_dataset
SNaive_ME_dataset <- mean(SNaive_errors_dataset) #Mean error
SNaive_MSE_dataset <- mean(SNaive_errors_dataset^2) #Mean squared error
SNaive_MAE_dataset<- mean(abs(SNaive_errors_dataset)) #Mean absolute error
SNaive_MAPE_dataset <- 100 * mean(abs(SNaive_errors_dataset)/dataset_test) #Mean absolute percentage error
SNaive_RMSE_dataset<- sqrt(mean(SNaive_errors_dataset^2)) #Root mean squared error

## Rolling origin for SNaive

dataset_rolling_forecasts_SNaive <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_SNaive <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_SNaive) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_SNaive) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_SNaive) <- dimnames(dataset_rolling_forecasts_SNaive)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_SNaive[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_SNaive[i,] <- snaive(dataset_rolling_train_set,h=H)$mean
}

## MAPE for Rolling origin of SNaive
Rolling_errors_dataset_SNaive<- dataset_rolling_holdout_SNaive - dataset_rolling_forecasts_SNaive
Rolling_ME_dataset_SNaive<- mean(Rolling_errors_dataset_SNaive) #Mean error
Rolling_MSE_dataset_SNaive<- mean(Rolling_errors_dataset_SNaive^2) #Mean squared error
Rolling_MAE_dataset_SNaive<- mean(abs(Rolling_errors_dataset_SNaive)) #Mean absolute error
Rolling_MAPE_dataset_SNaive<- 100 * mean(abs(Rolling_errors_dataset_SNaive)/dataset_rolling_holdout_SNaive)
Rolling_RMSE_dataset_SNaive<- sqrt(mean(Rolling_errors_dataset_SNaive^2)) #Root mean squared error

# Create summary table for error measure of Seasonal Naive Model

Summary_table<- rbind(Summary_table,c(SNaive_ME_dataset,SNaive_MSE_dataset,SNaive_MAE_dataset,SNaive_MAPE_dataset,SNaive_RMSE_dataset))
Summary_table<- rbind(Summary_table,c(Rolling_ME_dataset_SNaive,Rolling_MSE_dataset_SNaive,Rolling_MAE_dataset_SNaive,Rolling_MAPE_dataset_SNaive,Rolling_RMSE_dataset_SNaive))
colnames(Summary_table) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary_table) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin', 'Seasonal Naive Model Error Measures', 'Seasonal Naive Model Error Measures with Rolling origin')
Summary_table <- as.table(Summary_table)
Summary_table
names(Summary_table) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin","Seasonal Naive Model Error Measures","Seasonal Naive Model Error Measures with Rolling origin")
knitr::kable(Summary_table)

# Forecast using Seasonal Naive -------------------------------------------
SNaive_method_dataset <- snaive(dataset, h=h)
Forecast_Snaive_dataset <- SNaive_method_dataset$mean
Forecast_Snaive_dataset

# Exponential Smoothing ---------------------------------------------------

## Exponential Smoothing ZZZ ---------------------------------------------------

# Calculate an Optimized ES Method using ETS()
ETS_optimised_dataset <- ets(dataset_train, model = "ZZZ")
# Check the AIC
ETS_optimised_dataset
# Coefficient of ETS optimized method
coef(ETS_optimised_dataset)
checkresiduals(ETS_optimised_dataset)

#Forecating the ETS optimized model
Forecast_ETS_optimised_dataset <- forecast(ETS_optimised_dataset, h=h)
plot(Forecast_ETS_optimised_dataset)

# Error check for Forecast for ETS optimized
ETS_optimised_dataset_errors <- dataset_test - (Forecast_ETS_optimised_dataset$mean)
ETS_optimised_ME_dataset<- mean(ETS_optimised_dataset_errors) #Mean error
ETS_optimised_MSE_dataset<- mean(ETS_optimised_dataset_errors^2) #Mean squared error
ETS_optimised_MAE_dataset<- mean(abs(ETS_optimised_dataset_errors)) #Mean absolute error
ETS_optimised_MAPE_dataset<- 100 * mean(abs(ETS_optimised_dataset_errors)/dataset_test) #Mean absolute percentage error
ETS_optimised_RMSE_dataset<- sqrt(mean(ETS_optimised_dataset_errors^2)) # Root mean squared error

ETS_optimised_MAPE_dataset
ETS_optimised_RMSE_dataset


## Rolling origin for ETS optimized

dataset_rolling_forecasts_ETS_optimised<- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_ETS_optimised <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_ETS_optimised) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_ETS_optimised) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_ETS_optimised) <- dimnames(dataset_rolling_forecasts_ETS_optimised)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_ETS_optimised[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_ETS_optimised[i,] <- forecast(ets(dataset_rolling_train_set,"ZZZ"),h=H)$mean
}

## MAPE for Rolling origin of ETS optimized
Rolling_errors_dataset_ETS_optimised<- dataset_rolling_holdout_ETS_optimised - dataset_rolling_forecasts_ETS_optimised
Rolling_ME_dataset_ETS_optimised<- mean(Rolling_errors_dataset_ETS_optimised) #Mean error
Rolling_MSE_dataset_ETS_optimised<- mean(Rolling_errors_dataset_ETS_optimised^2) #Mean squared error
Rolling_MAE_dataset_ETS_optimised<- mean(abs(Rolling_errors_dataset_ETS_optimised)) #Mean absolute error
Rolling_MAPE_dataset_ETS_optimised<- 100 * mean(abs(Rolling_errors_dataset_ETS_optimised)/dataset_rolling_holdout_ETS_optimised)
Rolling_RMSE_dataset_ETS_optimised<- sqrt(mean(Rolling_errors_dataset_ETS_optimised^2)) #Root mean squared error

# Create summary table for error measure of ETA Optimised Model
Summary<- rbind(Summary,c(ETS_optimised_ME_dataset,ETS_optimised_MSE_dataset,ETS_optimised_MAE_dataset,ETS_optimised_MAPE_dataset,ETS_optimised_RMSE_dataset))
Summary<- rbind(Summary,c(Rolling_ME_dataset_ETS_optimised,Rolling_MSE_dataset_ETS_optimised,Rolling_MAE_dataset_ETS_optimised,Rolling_MAPE_dataset_ETS_optimised,Rolling_RMSE_dataset_ETS_optimised))
colnames(Summary) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin', 'Seasonal Naive Model Error Measures', 'Seasonal Naive Model Error Measures with Rolling origin', ' ETS ANA Model Error Measures', ' ETS ANA Model Error Measures with Rolling origin',
                       ' ETS AAA Model Error Measures', ' ETS AAA Model Error Measures with Rolling origin',
                       ' ETS MAM Model Error Measures', ' ETS MAM Model Error Measures with Rolling origin',
                       ' ETS MNA Model Error Measures', ' ETS MNA Model Error Measures with Rolling origin',
                       ' ETS Optimised Model Error Measures', ' ETS Optimised Model Error Measures with Rolling origin')
Summary <- as.table(Summary)
Summary
names(Summary) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin","Seasonal Naive Model Error Measures","Seasonal Naive Model Error Measures with Rolling origin"
                    ,"ETS ANA Model Error Measures","ETS ANA Model Error Measures with Rolling origin"
                    ,"ETS AAA Model Error Measures","ETS AAA Model Error Measures with Rolling origin",
                    ,"ETS MAM Model Error Measures","ETS MAM Model Error Measures with Rolling origin",
                    ,"ETS MNA Model Error Measures","ETS MNA Model Error Measures with Rolling origin",
                    ,"ETS Optimised Model Error Measures","ETS Optimised Model Error Measures with Rolling origin")
knitr::kable(Summary)
# Forecast using Best ETS -------------------------------------------------
ETS_optimised_dataset <- ets(dataset, model = "ZZZ")

#Forecating the ETS optimized model
Forecast_ETS_optimised_dataset <- forecast(ETS_optimised_dataset, h=h)
Forecast_ETS_optimised_dataset

## Exponential Smoothing ANA -----------------------------------------------

# Fit a model using ETS(A,N,A):
ETS_ANA_opt_dataset <- ets(dataset_train, model = "ANA")
# Check the AIC
ETS_ANA_opt_dataset
#Finding the coefficients
coef(ETS_ANA_opt_dataset)
#Forecating the ANA model
Forecast_ETS_ANA_opt_dataset <- forecast(ETS_ANA_opt_dataset, h=h)
plot(Forecast_ETS_ANA_opt_dataset)
checkresiduals(ETS_ANA_opt_dataset)
# Error check for Forecast for ETS(A,N,A)
ETS_ANA_opt_dataset_errors <- dataset_test - forecast(ETS_ANA_opt_dataset, h=h)$mean
ETS_ANA_ME_dataset<- mean(ETS_ANA_opt_dataset_errors) #Mean error
ETS_ANA_MSE_dataset<- mean(ETS_ANA_opt_dataset_errors^2) #Mean squared error
ETS_ANA_MAE_dataset<- mean(abs(ETS_ANA_opt_dataset_errors)) #Mean absolute error
ETS_ANA_MAPE_dataset<- 100 * mean(abs(ETS_ANA_opt_dataset_errors)/dataset_test) #Mean absolute percentage error
ETS_ANA_RMSE_dataset<- sqrt(mean(ETS_ANA_opt_dataset_errors^2)) # Root mean squared error

ETS_ANA_MAPE_dataset
ETS_ANA_RMSE_dataset
## Rolling origin for ETS ANA

dataset_rolling_forecasts_ETS_ANA <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_ETS_ANA <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_ETS_ANA) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_ETS_ANA) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_ETS_ANA) <- dimnames(dataset_rolling_forecasts_ETS_ANA)

for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout valuETS from the test set
  dataset_rolling_holdout_ETS_ANA[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_ETS_ANA[i,] <- forecast(ets(dataset_rolling_train_set,"ANA"),h=H)$mean
}

## MAPE for Rolling origin of ETA ANA

Rolling_errors_dataset_ETS_ANA<- dataset_rolling_holdout_ETS_ANA - dataset_rolling_forecasts_ETS_ANA
Rolling_ME_dataset_ETS_ANA<- mean(Rolling_errors_dataset_ETS_ANA) #Mean error
Rolling_MSE_dataset_ETS_ANA<- mean(Rolling_errors_dataset_ETS_ANA^2) #Mean squared error
Rolling_MAE_dataset_ETS_ANA<- mean(abs(Rolling_errors_dataset_ETS_ANA)) #Mean absolute error
Rolling_MAPE_dataset_ETS_ANA<- 100 * mean(abs(Rolling_errors_dataset_ETS_ANA)/dataset_rolling_holdout_ETS_ANA)
Rolling_RMSE_dataset_ETS_ANA<- sqrt(mean(Rolling_errors_dataset_ETS_ANA^2)) #Root mean squared error

Rolling_MAPE_dataset_ETS_ANA
Rolling_RMSE_dataset_ETS_ANA

# Create summary table for error measure of ETA ANA Model

Summary<- rbind(Summary,c(ETS_ANA_ME_dataset,ETS_ANA_MSE_dataset,ETS_ANA_MAE_dataset,ETS_ANA_MAPE_dataset,ETS_ANA_RMSE_dataset))
Summary<- rbind(Summary,c(Rolling_ME_dataset_ETS_ANA,Rolling_MSE_dataset_ETS_ANA,Rolling_MAE_dataset_ETS_ANA,Rolling_MAPE_dataset_ETS_ANA,Rolling_RMSE_dataset_ETS_ANA))
colnames(Summary) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin', 'Seasonal Naive Model Error Measures', 'Seasonal Naive Model Error Measures with Rolling origin', ' ETS ANA Model Error Measures', ' ETS ANA Model Error Measures with Rolling origin'
                      )
Summary <- as.table(Summary)
Summary
names(Summary) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin","Seasonal Naive Model Error Measures","Seasonal Naive Model Error Measures with Rolling origin"
                    ,"ETS ANA Model Error Measures","ETS ANA Model Error Measures with Rolling origin")
knitr::kable(Summary)

## Exponential Smoothing AAA -----------------------------------------------

# Fit a model using ETS(A,A,A):
ETS_AAA_opt_dataset <- ets(dataset_train, model= "AAA")
# Check the AIC
ETS_AAA_opt_dataset
#Finding the coefficients
coef(ETS_AAA_opt_dataset)
#Forecating the ANA model
Forecast_ETS_AAA_opt_dataset <- forecast(ETS_AAA_opt_dataset, h=h)
plot(Forecast_ETS_AAA_opt_dataset)
checkresiduals(ETS_AAA_opt_dataset)

# Error check for Forecast for ETS(A,A,A)
ETS_AAA_opt_dataset_errors <- dataset_test - (Forecast_ETS_AAA_opt_dataset$mean)
ETS_AAA_ME_dataset<- mean(ETS_AAA_opt_dataset_errors) #Mean error
ETS_AAA_MSE_dataset<- mean(ETS_AAA_opt_dataset_errors^2) #Mean squared error
ETS_AAA_MAE_dataset<- mean(abs(ETS_AAA_opt_dataset_errors)) #Mean absolute error
ETS_AAA_MAPE_dataset<- 100 * mean(abs(ETS_AAA_opt_dataset_errors)/dataset_test)
ETS_AAA_RMSE_dataset<- sqrt(mean(ETS_AAA_opt_dataset_errors^2)) # Root mean squared error

ETS_AAA_MAPE_dataset
ETS_AAA_RMSE_dataset
## Rolling origin for ETS AAA

dataset_rolling_forecasts_ETS_AAA <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_ETS_AAA <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_ETS_AAA) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_ETS_AAA) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_ETS_AAA) <- dimnames(dataset_rolling_forecasts_ETS_AAA)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_ETS_AAA[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_ETS_AAA[i,] <- forecast(ets(dataset_rolling_train_set,"AAA"),h=H)$mean
}

## MAPE for Rolling origin of ETS AAA
Rolling_errors_dataset_ETS_AAA<- dataset_rolling_holdout_ETS_AAA - dataset_rolling_forecasts_ETS_AAA
Rolling_ME_dataset_ETS_AAA<- mean(Rolling_errors_dataset_ETS_AAA) #Mean error
Rolling_MSE_dataset_ETS_AAA<- mean(Rolling_errors_dataset_ETS_AAA^2) #Mean squared error
Rolling_MAE_dataset_ETS_AAA<- mean(abs(Rolling_errors_dataset_ETS_AAA)) #Mean absolute error
Rolling_MAPE_dataset_ETS_AAA<- 100 * mean(abs(Rolling_errors_dataset_ETS_AAA)/dataset_rolling_holdout_ETS_AAA)
Rolling_RMSE_dataset_ETS_AAA<- sqrt(mean(Rolling_errors_dataset_ETS_AAA^2)) #Root mean squared error

Rolling_MAPE_dataset_ETS_AAA
Rolling_RMSE_dataset_ETS_AAA

# Create summary table for error measure of ETA AAA Model

Summary<- rbind(Summary,c(ETS_AAA_ME_dataset,ETS_AAA_MSE_dataset,ETS_AAA_MAE_dataset,ETS_AAA_MAPE_dataset,ETS_AAA_RMSE_dataset))
Summary<- rbind(Summary,c(Rolling_ME_dataset_ETS_AAA,Rolling_MSE_dataset_ETS_AAA,Rolling_MAE_dataset_ETS_AAA,Rolling_MAPE_dataset_ETS_AAA,Rolling_RMSE_dataset_ETS_AAA))
colnames(Summary) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin', 'Seasonal Naive Model Error Measures', 'Seasonal Naive Model Error Measures with Rolling origin', ' ETS ANA Model Error Measures', ' ETS ANA Model Error Measures with Rolling origin',
                       ' ETS AAA Model Error Measures', ' ETS AAA Model Error Measures with Rolling origin')
Summary <- as.table(Summary)
Summary
names(Summary) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin","Seasonal Naive Model Error Measures","Seasonal Naive Model Error Measures with Rolling origin"
                    ,"ETS ANA Model Error Measures","ETS ANA Model Error Measures with Rolling origin"
                    ,"ETS AAA Model Error Measures","ETS AAA Model Error Measures with Rolling origin")
knitr::kable(Summary)

## Exponential Smoothing MAM -----------------------------------------------

# Fit a model using ETS(M,A,M):
ETS_MAM_dataset <- ets(dataset_train, model = "MAM")
# Check the AIC
ETS_MAM_dataset
#Finding the coefficients
coef(ETS_MAM_dataset)
#Forecating the ANA model
Forecast_ETS_MAM_dataset <- forecast(ETS_MAM_dataset, h=h)
plot(Forecast_ETS_MAM_dataset)
checkresiduals(ETS_MAM_dataset)

# Error check for Forecast for ETS(A,N,M)
ETS_MAM_dataset_errors <- dataset_test - (Forecast_ETS_MAM_dataset$mean)
ETS_MAM_ME_dataset<- mean(ETS_MAM_dataset_errors) #Mean error
ETS_MAM_MSE_dataset<- mean(ETS_MAM_dataset_errors^2) #Mean squared error
ETS_MAM_MAE_dataset<- mean(abs(ETS_MAM_dataset_errors)) #Mean absolute error
ETS_MAM_MAPE_dataset<- 100 * mean(abs(ETS_MAM_dataset_errors)/dataset_test) #Mean absolute percentage error
ETS_MAM_RMSE_dataset<- sqrt(mean(ETS_MAM_dataset_errors^2)) # Root mean squared error

ETS_MAM_MAPE_dataset
ETS_MAM_RMSE_dataset
## Rolling origin for ETS MAM

dataset_rolling_forecasts_ETS_MAM <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_ETS_MAM <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_ETS_MAM) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_ETS_MAM) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_ETS_MAM) <- dimnames(dataset_rolling_forecasts_ETS_MAM)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_ETS_MAM[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_ETS_MAM[i,] <- forecast(ets(dataset_rolling_train_set,"MAM"),h=H)$mean
}

## MAPE for Rolling origin of ETS MAM
Rolling_errors_dataset_ETS_MAM<- dataset_rolling_holdout_ETS_MAM - dataset_rolling_forecasts_ETS_MAM
Rolling_ME_dataset_ETS_MAM<- mean(Rolling_errors_dataset_ETS_MAM) #Mean error
Rolling_MSE_dataset_ETS_MAM<- mean(Rolling_errors_dataset_ETS_MAM^2) #Mean squared error
Rolling_MAE_dataset_ETS_MAM<- mean(abs(Rolling_errors_dataset_ETS_MAM)) #Mean absolute error
Rolling_MAPE_dataset_ETS_MAM<- 100 * mean(abs(Rolling_errors_dataset_ETS_MAM)/dataset_rolling_holdout_ETS_MAM)
Rolling_RMSE_dataset_ETS_MAM<- sqrt(mean(Rolling_errors_dataset_ETS_MAM^2)) #Root mean squared error

Rolling_MAPE_dataset_ETS_MAM
Rolling_RMSE_dataset_ETS_MAM

# Create summary table for error measure of ETA MAM Model

Summary<- rbind(Summary,c(ETS_MAM_ME_dataset,ETS_MAM_MSE_dataset,ETS_MAM_MAE_dataset,ETS_MAM_MAPE_dataset,ETS_MAM_RMSE_dataset))
Summary<- rbind(Summary,c(Rolling_ME_dataset_ETS_MAM,Rolling_MSE_dataset_ETS_MAM,Rolling_MAE_dataset_ETS_MAM,Rolling_MAPE_dataset_ETS_MAM,Rolling_RMSE_dataset_ETS_MAM))
colnames(Summary) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin', 'Seasonal Naive Model Error Measures', 'Seasonal Naive Model Error Measures with Rolling origin', ' ETS ANA Model Error Measures', ' ETS ANA Model Error Measures with Rolling origin',
                       ' ETS AAA Model Error Measures', ' ETS AAA Model Error Measures with Rolling origin',
                       ' ETS MAM Model Error Measures', ' ETS MAM Model Error Measures with Rolling origin')
Summary <- as.table(Summary)
Summary
names(Summary) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin","Seasonal Naive Model Error Measures","Seasonal Naive Model Error Measures with Rolling origin"
                    ,"ETS ANA Model Error Measures","ETS ANA Model Error Measures with Rolling origin"
                    ,"ETS AAA Model Error Measures","ETS AAA Model Error Measures with Rolling origin",
                    ,"ETS MAM Model Error Measures","ETS MAM Model Error Measures with Rolling origin")
knitr::kable(Summary)

## Exponential Smoothing MNA -----------------------------------------------
# Fit a model using ETS(M,N,A):
ETS_MNA_dataset <- ets(dataset_train, model = "MNA")
# Check the AIC
ETS_MNA_dataset
#Finding the coefficients
coef(ETS_MNA_dataset)
#Forecating the MNA model
Forecast_ETS_MNA_dataset <- forecast(ETS_MNA_dataset, h=h)
plot(Forecast_ETS_MNA_dataset)
checkresiduals(ETS_MNA_dataset)

# Error check for Forecast for ETS(M,N,A)
ETS_MNA_dataset_errors <- dataset_test - (Forecast_ETS_MNA_dataset$mean)
ETS_MNA_ME_dataset<- mean(ETS_MNA_dataset_errors) #Mean error
ETS_MNA_MSE_dataset<- mean(ETS_MNA_dataset_errors^2) #Mean squared error
ETS_MNA_MAE_dataset<- mean(abs(ETS_MNA_dataset_errors)) #Mean absolute error
ETS_MNA_MAPE_dataset<- 100 * mean(abs(ETS_MNA_dataset_errors)/dataset_test) #Mean absolute percentage error
ETS_MNA_RMSE_dataset<- sqrt(mean(ETS_MNA_dataset_errors^2)) # Root mean squared error


## Rolling origin for ETS MNA

dataset_rolling_forecasts_ETS_MNA <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_ETS_MNA <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_ETS_MNA) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_ETS_MNA) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_ETS_MNA) <- dimnames(dataset_rolling_forecasts_ETS_MNA)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_ETS_MNA[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_ETS_MNA[i,] <- forecast(ets(dataset_rolling_train_set,"MNA"),h=H)$mean
}

## MAPE for Rolling origin of ETS MNA
Rolling_errors_dataset_ETS_MNA<- dataset_rolling_holdout_ETS_MNA - dataset_rolling_forecasts_ETS_MNA
Rolling_ME_dataset_ETS_MNA<- mean(Rolling_errors_dataset_ETS_MNA) #Mean error
Rolling_MSE_dataset_ETS_MNA<- mean(Rolling_errors_dataset_ETS_MNA^2) #Mean squared error
Rolling_MAE_dataset_ETS_MNA<- mean(abs(Rolling_errors_dataset_ETS_MNA)) #Mean absolute error
Rolling_MAPE_dataset_ETS_MNA<- 100 * mean(abs(Rolling_errors_dataset_ETS_MNA)/dataset_rolling_holdout_ETS_MNA)
Rolling_RMSE_dataset_ETS_MNA<- sqrt(mean(Rolling_errors_dataset_ETS_MNA^2)) #Root mean squared error

Rolling_MAPE_dataset_ETS_MNA
Rolling_RMSE_dataset_ETS_MNA

# Create summary table for error measure of ETA MNA Model

Summary<- rbind(Summary,c(ETS_MNA_ME_dataset,ETS_MNA_MSE_dataset,ETS_MNA_MAE_dataset,ETS_MNA_MAPE_dataset,ETS_MNA_RMSE_dataset))
Summary<- rbind(Summary,c(Rolling_ME_dataset_ETS_MNA,Rolling_MSE_dataset_ETS_MNA,Rolling_MAE_dataset_ETS_MNA,Rolling_MAPE_dataset_ETS_MNA,Rolling_RMSE_dataset_ETS_MNA))
colnames(Summary) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin', 'Seasonal Naive Model Error Measures', 'Seasonal Naive Model Error Measures with Rolling origin', ' ETS ANA Model Error Measures', ' ETS ANA Model Error Measures with Rolling origin',
                       ' ETS AAA Model Error Measures', ' ETS AAA Model Error Measures with Rolling origin',
                       ' ETS MAM Model Error Measures', ' ETS MAM Model Error Measures with Rolling origin',
                       ' ETS MNA Model Error Measures', ' ETS MNA Model Error Measures with Rolling origin')
Summary <- as.table(Summary)
Summary
names(Summary) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin","Seasonal Naive Model Error Measures","Seasonal Naive Model Error Measures with Rolling origin"
                    ,"ETS ANA Model Error Measures","ETS ANA Model Error Measures with Rolling origin"
                    ,"ETS AAA Model Error Measures","ETS AAA Model Error Measures with Rolling origin",
                    ,"ETS MAM Model Error Measures","ETS MAM Model Error Measures with Rolling origin",
                    ,"ETS MNA Model Error Measures","ETS MNA Model Error Measures with Rolling origin")
knitr::kable(Summary)

# ARIMA Prep -------------------------------------------------------------------

# Perform KPSS and ADF test on time series
kpss.test(dataset_train)
adf.test(dataset_train)

# Plot the ACF and PACF plots
tsdisplay(dataset)

#Differencing order
nsdiffs(dataset)  # Seasonal diff
ndiffs(dataset) # Normal diff

# First order Differencing
diff_dataset_train <- diff(dataset_train)

# Plot ACF and PACF of first differences
tsdisplay(diff_dataset_train)

# KPSS/ADF test of 1st order diff series
kpss.test(diff_dataset_train)
adf.test(diff_dataset_train)

# Plot ACF and PACF of first and seasonal differences
tsdisplay(diff(diff((dataset_train),lag=7)))

# Plot ACF and PACF of seasonal differences
tsdisplay(diff(dataset_train, lag=7))

# Second order diff
diff2_data <- diff(dataset_train, differences=2)
tsdisplay(diff2_data)

## Fitting the ARIMA/SARIMA Models
tsdisplay(dataset_train)


## Seasonal ARIMA Model (7,1,1)(0,1,3) -------------------------------------

# Model implementation
Arima711_Seasoanl013_dataset<- Arima(dataset_train, order=c(7,1,1),  seasonal=c(0,1,3))

# Check the coeff of SARIMA Models
Arima711_Seasoanl013_dataset   

# Check residuals of SARIMA model
checkresiduals(Arima711_Seasoanl013_dataset) 

# Check ACF/PACF plot of residuals
tsdisplay(residuals(Arima711_Seasoanl013_dataset))

# Forecasting
FC_Arima711_Seasoanl013_dataset <- forecast(Arima711_Seasoanl013_dataset, h=h)


# Error measures
Arima711_Seasoanl013_dataset_errors <- dataset_test - (FC_Arima711_Seasoanl013_dataset)$mean
Arima711_Seasoanl013_dataset_ME <- mean(Arima711_Seasoanl013_dataset_errors) #Mean error
Arima711_Seasoanl013_dataset_MSE <- mean(Arima711_Seasoanl013_dataset_errors^2) #Mean squared error
Arima711_Seasoanl013_dataset_MAE <- mean(abs(Arima711_Seasoanl013_dataset_errors)) #Mean absolute error
Arima711_Seasoanl013_dataset_MAPE <- 100 * mean(abs(Arima711_Seasoanl013_dataset_errors)/dataset_test)
Arima711_Seasoanl013_dataset_RMSE <- sqrt(mean(Arima711_Seasoanl013_dataset_errors^2)) # Root mean squared error
Arima711_Seasoanl013_dataset_errors
Arima711_Seasoanl013_dataset_MAPE
Arima711_Seasoanl013_dataset_RMSE
## Rolling origin for Arima711_Seasoanl013

dataset_rolling_forecasts_Arima711_Seasoanl013 <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Arima711_Seasoanl013 <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Arima711_Seasoanl013) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Arima711_Seasoanl013) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Arima711_Seasoanl013) <- dimnames(dataset_rolling_forecasts_Arima711_Seasoanl013)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Arima711_Seasoanl013[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Arima711_Seasoanl013[i,] <- forecast(arima(dataset_rolling_train_set, order=c(7,1,1),  seasonal=c(0,1,3)),h=H)$mean
}


## MAPE for Rolling origin of Arima711_Seasoanl013
Rolling_errors_dataset_Arima711_Seasoanl013<- dataset_rolling_holdout_Arima711_Seasoanl013 - dataset_rolling_forecasts_Arima711_Seasoanl013
Rolling_ME_dataset_Arima711_Seasoanl013<- mean(Rolling_errors_dataset_Arima711_Seasoanl013) #Mean error
Rolling_MSE_dataset_Arima711_Seasoanl013<- mean(Rolling_errors_dataset_Arima711_Seasoanl013^2) #Mean squared error
Rolling_MAE_dataset_Arima711_Seasoanl013<- mean(abs(Rolling_errors_dataset_Arima711_Seasoanl013)) #Mean absolute error
Rolling_MAPE_dataset_Arima711_Seasoanl013<- 100 * mean(abs(Rolling_errors_dataset_Arima711_Seasoanl013)/dataset_rolling_holdout_Arima711_Seasoanl013)
Rolling_RMSE_dataset_Arima711_Seasoanl013<- sqrt(mean(Rolling_errors_dataset_Arima711_Seasoanl013^2)) #Root mean squared error

Rolling_MAPE_dataset_Arima711_Seasoanl013
Rolling_RMSE_dataset_Arima711_Seasoanl013


# Create summary table for error measure of Seasonal Naive Model

Summary_table<- rbind(Summary_table,c(Arima711_Seasoanl013_dataset_ME,Arima711_Seasoanl013_dataset_MSE,Arima711_Seasoanl013_dataset_MAE,Arima711_Seasoanl013_dataset_MAPE,Arima711_Seasoanl013_dataset_RMSE))
Summary_table<- rbind(Summary_table,c(Rolling_ME_dataset_Arima711_Seasoanl013,Rolling_MSE_dataset_Arima711_Seasoanl013,Rolling_MAE_dataset_Arima711_Seasoanl013,Rolling_MAPE_dataset_Arima711_Seasoanl013,Rolling_RMSE_dataset_Arima711_Seasoanl013))
colnames(Summary_table) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary_table) <- c('Naive Model Error Measures','Naive Model Error Measures with Rolling origin', 'Seasonal Naive Model Error Measures', 'Seasonal Naive Model Error Measures with Rolling origin',
                             'Arima 711 Seasonal 013 Error Measures', 'Arima 711 Seasonal 013 Error Measures with Rolling origin')
Summary_table <- as.table(Summary_table)
Summary_table
names(Summary_table) <- c("Naive Model Error Measures","Naive Model Error Measures with Rolling origin","Seasonal Naive Model Error Measures","Seasonal Naive Model Error Measures with Rolling origin",
                          "Arima 711 Seasonal 013 Error Measures","Arima 711 Seasonal 013 Error Measures with Rolling origin")
knitr::kable(Summary_table)



## Seasonal ARIMA Model (2,0,2)(0,1,2) -------------------------------------

Arima202_Seasoanl012_dataset<- Arima(dataset_train, order=c(2,0,2),  seasonal=c(0,1,2))
Arima202_Seasoanl012_dataset # 2946.29   4385.27  

checkresiduals(Arima202_Seasoanl012_dataset)

tsdisplay(residuals(Arima202_Seasoanl012_dataset)) # Failed in Residuals plot

FC_Arima202_Seasoanl012_dataset <- forecast(Arima202_Seasoanl012_dataset, h=h)
FC_Arima202_Seasoanl012_dataset

Arima202_Seasoanl012_dataset_errors <- dataset_test - FC_Arima202_Seasoanl012_dataset$mean
Arima202_Seasoanl012_dataset_ME <- mean(Arima202_Seasoanl012_dataset_errors) #Mean error
Arima202_Seasoanl012_dataset_MSE <- mean(Arima202_Seasoanl012_dataset_errors^2) #Mean squared error
Arima202_Seasoanl012_dataset_MAE <- mean(abs(Arima202_Seasoanl012_dataset_errors)) #Mean absolute error
Arima202_Seasoanl012_dataset_MAPE <- 100 * mean(abs(Arima202_Seasoanl012_dataset_errors)/dataset_test)
Arima202_Seasoanl012_dataset_RMSE <- sqrt(mean(Arima202_Seasoanl012_dataset_errors^2)) # Root mean squared error

Arima202_Seasoanl012_dataset_MAPE
Arima202_Seasoanl012_dataset_RMSE
## Rolling origin for Arima202_Seasoanl012

dataset_rolling_forecasts_Arima202_Seasoanl012 <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Arima202_Seasoanl012 <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Arima202_Seasoanl012) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Arima202_Seasoanl012) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Arima202_Seasoanl012) <- dimnames(dataset_rolling_forecasts_Arima202_Seasoanl012)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Arima202_Seasoanl012[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Arima202_Seasoanl012[i,] <- forecast(arima(dataset_rolling_train_set, order=c(2,0,2),  seasonal=c(0,1,2)),h=H)$mean
}

## MAPE for Rolling origin of Arima202_Seasoanl012
Rolling_errors_dataset_Arima202_Seasoanl012<- dataset_rolling_holdout_Arima202_Seasoanl012 - dataset_rolling_forecasts_Arima202_Seasoanl012
Rolling_ME_dataset_Arima202_Seasoanl012<- mean(Rolling_errors_dataset_Arima202_Seasoanl012) #Mean error
Rolling_MSE_dataset_Arima202_Seasoanl012<- mean(Rolling_errors_dataset_Arima202_Seasoanl012^2) #Mean squared error
Rolling_MAE_dataset_Arima202_Seasoanl012<- mean(abs(Rolling_errors_dataset_Arima202_Seasoanl012)) #Mean absolute error
Rolling_MAPE_dataset_Arima202_Seasoanl012<- 100 * mean(abs(Rolling_errors_dataset_Arima202_Seasoanl012)/dataset_rolling_holdout_Arima202_Seasoanl012)
Rolling_RMSE_dataset_Arima202_Seasoanl012<- sqrt(mean(Rolling_errors_dataset_Arima202_Seasoanl012^2)) #Root mean squared error

Rolling_MAPE_dataset_Arima202_Seasoanl012
Rolling_RMSE_dataset_Arima202_Seasoanl012
## Seasonal ARIMA Model (2,0,2)(1,1,1) -------------------------------------

Arima202_Seasoanl111_dataset<- Arima(dataset_train, order=c(2,0,2),  seasonal=c(1,1,1))

Arima202_Seasoanl111_dataset # 2945.85   4385.02   

checkresiduals(Arima202_Seasoanl111_dataset)

tsdisplay(residuals(Arima202_Seasoanl111_dataset)) # Failed in Residuals plot

FC_Arima202_Seasoanl111_dataset <- forecast(Arima202_Seasoanl111_dataset, h=h)$mean

Arima202_Seasoanl111_dataset_errors <- dataset_test - FC_Arima202_Seasoanl111_dataset
Arima202_Seasoanl111_dataset_ME <- mean(Arima202_Seasoanl111_dataset_errors) #Mean error
Arima202_Seasoanl111_dataset_MSE <- mean(Arima202_Seasoanl111_dataset_errors^2) #Mean squared error
Arima202_Seasoanl111_dataset_MAE <- mean(abs(Arima202_Seasoanl111_dataset_errors)) #Mean absolute error
Arima202_Seasoanl111_dataset_MAPE <- 100 * mean(abs(Arima202_Seasoanl111_dataset_errors)/dataset_test)
Arima202_Seasoanl111_dataset_RMSE <- sqrt(mean(Arima202_Seasoanl111_dataset_errors^2)) # Root mean squared error

Arima202_Seasoanl111_dataset_MAPE
Arima202_Seasoanl111_dataset_RMSE
## Rolling origin for Arima202_Seasoanl111

dataset_rolling_forecasts_Arima202_Seasoanl111 <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Arima202_Seasoanl111 <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Arima202_Seasoanl111) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Arima202_Seasoanl111) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Arima202_Seasoanl111) <- dimnames(dataset_rolling_forecasts_Arima202_Seasoanl111)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Arima202_Seasoanl111[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Arima202_Seasoanl111[i,] <- forecast(arima(dataset_rolling_train_set, order=c(2,0,2),  seasonal=c(1,1,1)),h=H)$mean
}

## MAPE for Rolling origin of Arima202_Seasoanl111
Rolling_errors_dataset_Arima202_Seasoanl111<- dataset_rolling_holdout_Arima202_Seasoanl111 - dataset_rolling_forecasts_Arima202_Seasoanl111
Rolling_ME_dataset_Arima202_Seasoanl111<- mean(Rolling_errors_dataset_Arima202_Seasoanl111) #Mean error
Rolling_MSE_dataset_Arima202_Seasoanl111<- mean(Rolling_errors_dataset_Arima202_Seasoanl111^2) #Mean squared error
Rolling_MAE_dataset_Arima202_Seasoanl111<- mean(abs(Rolling_errors_dataset_Arima202_Seasoanl111)) #Mean absolute error
Rolling_MAPE_dataset_Arima202_Seasoanl111<- 100 * mean(abs(Rolling_errors_dataset_Arima202_Seasoanl111)/dataset_rolling_holdout_Arima202_Seasoanl111)
Rolling_RMSE_dataset_Arima202_Seasoanl111<- sqrt(mean(Rolling_errors_dataset_Arima202_Seasoanl111^2)) #Root mean squared error

Rolling_MAPE_dataset_Arima202_Seasoanl111


## Seasonal ARIMA Model (1,1,3)(2,1,2) -------------------------------------

Arima113_Seasoanl212_dataset<- Arima(dataset_train, order=c(1,1,3),  seasonal=c(2,1,2))

Arima113_Seasoanl212_dataset # 2953.3    4395.27   

checkresiduals(Arima113_Seasoanl212_dataset)

tsdisplay(residuals(Arima113_Seasoanl212_dataset))

FC_Arima113_Seasoanl212_dataset <- forecast(Arima113_Seasoanl212_dataset, h=h)$mean


Arima113_Seasoanl212_dataset_errors <- dataset_test - FC_Arima113_Seasoanl212_dataset
Arima113_Seasoanl212_dataset_ME <- mean(Arima113_Seasoanl212_dataset_errors) #Mean error
Arima113_Seasoanl212_dataset_MSE <- mean(Arima113_Seasoanl212_dataset_errors^2) #Mean squared error
Arima113_Seasoanl212_dataset_MAE <- mean(abs(Arima113_Seasoanl212_dataset_errors)) #Mean absolute error
Arima113_Seasoanl212_dataset_MAPE <- 100 * mean(abs(Arima113_Seasoanl212_dataset_errors)/dataset_test)
Arima113_Seasoanl212_dataset_RMSE <- sqrt(mean(Arima113_Seasoanl212_dataset_errors^2)) # Root mean squared error

Arima113_Seasoanl212_dataset_MAPE
Arima113_Seasoanl212_dataset_RMSE

## Rolling origin for Arima113_Seasoanl212

dataset_rolling_forecasts_Arima113_Seasoanl212 <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Arima113_Seasoanl212 <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Arima113_Seasoanl212) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Arima113_Seasoanl212) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Arima113_Seasoanl212) <- dimnames(dataset_rolling_forecasts_Arima113_Seasoanl212)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Arima113_Seasoanl212[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Arima113_Seasoanl212[i,] <- forecast(arima(dataset_rolling_train_set, order=c(1,1,3),  seasonal=c(2,1,2)),h=H)$mean
  
}

## MAPE for Rolling origin of Arima113_Seasoanl212
Rolling_errors_dataset_Arima113_Seasoanl212<- dataset_rolling_holdout_Arima113_Seasoanl212 - dataset_rolling_forecasts_Arima113_Seasoanl212
Rolling_ME_dataset_Arima113_Seasoanl212<- mean(Rolling_errors_dataset_Arima113_Seasoanl212) #Mean error
Rolling_MSE_dataset_Arima113_Seasoanl212<- mean(Rolling_errors_dataset_Arima113_Seasoanl212^2) #Mean squared error
Rolling_MAE_dataset_Arima113_Seasoanl212<- mean(abs(Rolling_errors_dataset_Arima113_Seasoanl212)) #Mean absolute error
Rolling_MAPE_dataset_Arima113_Seasoanl212<- 100 * mean(abs(Rolling_errors_dataset_Arima113_Seasoanl212)/dataset_rolling_holdout_Arima113_Seasoanl212)
Rolling_RMSE_dataset_Arima113_Seasoanl212<- sqrt(mean(Rolling_errors_dataset_Arima113_Seasoanl212^2)) #Root mean squared error

Rolling_MAPE_dataset_Arima113_Seasoanl212
Rolling_RMSE_dataset_Arima113_Seasoanl212
## Seasonal ARIMA Model (3,0,2)(1,1,2) -------------------------------------

Arima302_Seasoanl112_dataset<- Arima(dataset_train, order=c(3,0,2),  seasonal=c(1,1,2))

Arima302_Seasoanl112_dataset # 2947.62   4382.65   

checkresiduals(Arima302_Seasoanl112_dataset)

tsdisplay(residuals(Arima302_Seasoanl112_dataset)) # Failed in Residuals plot

FC_Arima302_Seasoanl112_dataset <- forecast(Arima302_Seasoanl112_dataset, h=h)

plot(FC_Arima302_Seasoanl112_dataset)

Arima302_Seasoanl112_dataset_errors <- dataset_test - FC_Arima302_Seasoanl112_dataset
Arima302_Seasoanl112_dataset_ME <- mean(Arima302_Seasoanl112_dataset_errors) #Mean error
Arima302_Seasoanl112_dataset_MSE <- mean(Arima302_Seasoanl112_dataset_errors^2) #Mean squared error
Arima302_Seasoanl112_dataset_MAE <- mean(abs(Arima302_Seasoanl112_dataset_errors)) #Mean absolute error
Arima302_Seasoanl112_dataset_MAPE <- 100 * mean(abs(Arima302_Seasoanl112_dataset_errors)/dataset_test)
Arima302_Seasoanl112_dataset_RMSE <- sqrt(mean(Arima302_Seasoanl112_dataset_errors^2)) # Root mean squared error

Arima302_Seasoanl112_dataset_MAPE
Arima302_Seasoanl112_dataset_RMSE
## Rolling origin for Arima302_Seasoanl112

dataset_rolling_forecasts_Arima302_Seasoanl112 <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Arima302_Seasoanl112 <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Arima302_Seasoanl112) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Arima302_Seasoanl112) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Arima302_Seasoanl112) <- dimnames(dataset_rolling_forecasts_Arima302_Seasoanl112)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Arima302_Seasoanl112[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Arima302_Seasoanl112[i,] <- forecast(arima(dataset_rolling_train_set, order=c(3,0,2),  seasonal=c(1,1,2)),h=H)$mean
}

## MAPE for Rolling origin of Arima302_Seasoanl112
Rolling_errors_dataset_Arima302_Seasoanl112<- dataset_rolling_holdout_Arima302_Seasoanl112 - dataset_rolling_forecasts_Arima302_Seasoanl112
Rolling_ME_dataset_Arima302_Seasoanl112<- mean(Rolling_errors_dataset_Arima302_Seasoanl112) #Mean error
Rolling_MSE_dataset_Arima302_Seasoanl112<- mean(Rolling_errors_dataset_Arima302_Seasoanl112^2) #Mean squared error
Rolling_MAE_dataset_Arima302_Seasoanl112<- mean(abs(Rolling_errors_dataset_Arima302_Seasoanl112)) #Mean absolute error
Rolling_MAPE_dataset_Arima302_Seasoanl112<- 100 * mean(abs(Rolling_errors_dataset_Arima302_Seasoanl112)/dataset_rolling_holdout_Arima302_Seasoanl112)
Rolling_RMSE_dataset_Arima302_Seasoanl112<- sqrt(mean(Rolling_errors_dataset_Arima302_Seasoanl112^2)) #Root mean squared error

Rolling_MAPE_dataset_Arima302_Seasoanl112
Rolling_RMSE_dataset_Arima302_Seasoanl112


# Forecast using Best ARIMA Model -----------------------------------------
Arima302_Seasoanl112_dataset<- Arima(dataset, order=c(3,0,2),  seasonal=c(1,1,2))

FC_Arima302_Seasoanl112_dataset <- forecast(Arima302_Seasoanl112_dataset, h=h)$mean

FC_Arima302_Seasoanl112_dataset

## Seasonal ARIMA Model (4,0,2)(3,1,1) -------------------------------------

Arima402_Seasoanl311_dataset<- Arima(dataset_train, order=c(4,0,2),  seasonal=c(3,1,1))

Arima402_Seasoanl311_dataset

checkresiduals(Arima402_Seasoanl311_dataset)

tsdisplay(residuals(Arima402_Seasoanl311_dataset))


FC_Arima402_Seasoanl311_dataset <- forecast(Arima402_Seasoanl311_dataset, h=h)$mean


Arima402_Seasoanl311_dataset_errors <- dataset_test - FC_Arima402_Seasoanl311_dataset
Arima402_Seasoanl311_dataset_ME <- mean(Arima402_Seasoanl311_dataset_errors) #Mean error
Arima402_Seasoanl311_dataset_MSE <- mean(Arima402_Seasoanl311_dataset_errors^2) #Mean squared error
Arima402_Seasoanl311_dataset_MAE <- mean(abs(Arima402_Seasoanl311_dataset_errors)) #Mean absolute error
Arima402_Seasoanl311_dataset_MAPE <- 100 * mean(abs(Arima402_Seasoanl311_dataset_errors)/dataset_test)
Arima402_Seasoanl311_dataset_RMSE <- sqrt(mean(Arima402_Seasoanl311_dataset_errors^2)) # Root mean squared error

Arima402_Seasoanl311_dataset_MAPE
Arima402_Seasoanl311_dataset_RMSE
## Rolling origin for Arima402_Seasoanl311

dataset_rolling_forecasts_Arima402_Seasoanl311 <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Arima402_Seasoanl311 <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Arima402_Seasoanl311) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Arima402_Seasoanl311) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Arima402_Seasoanl311) <- dimnames(dataset_rolling_forecasts_Arima402_Seasoanl311)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_Arima402_Seasoanl311[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Arima402_Seasoanl311[i,] <- forecast(arima(dataset_rolling_train_set, order=c(4,0,2),  seasonal=c(3,1,1)),h=H)$mean
  
}

## MAPE for Rolling origin of Arima402_Seasoanl311
Rolling_errors_dataset_Arima402_Seasoanl311<- dataset_rolling_holdout_Arima402_Seasoanl311 - dataset_rolling_forecasts_Arima402_Seasoanl311
Rolling_ME_dataset_Arima402_Seasoanl311<- mean(Rolling_errors_dataset_Arima402_Seasoanl311) #Mean error
Rolling_MSE_dataset_Arima402_Seasoanl311<- mean(Rolling_errors_dataset_Arima402_Seasoanl311^2) #Mean squared error
Rolling_MAE_dataset_Arima402_Seasoanl311<- mean(abs(Rolling_errors_dataset_Arima402_Seasoanl311)) #Mean absolute error
Rolling_MAPE_dataset_Arima402_Seasoanl311<- 100 * mean(abs(Rolling_errors_dataset_Arima402_Seasoanl311)/dataset_rolling_holdout_Arima402_Seasoanl311)
Rolling_RMSE_dataset_Arima402_Seasoanl311<- sqrt(mean(Rolling_errors_dataset_Arima402_Seasoanl311^2)) #Root mean squared error

Rolling_MAPE_dataset_Arima402_Seasoanl311
Rolling_RMSE_dataset_Arima402_Seasoanl311

#  Final recommendation of ARIMA Model ------------------------------------


# Best ARIMA Model
#Fitting the SARIMA model
Sarima_dataset <- Arima(dataset_train, order=c(3,0,2), seasonal=c(1,1,2))
tsdisplay(residuals(Sarima_dataset)) #The spikes become insignificant 
Sarima_dataset
checkresiduals(Sarima_dataset)
Forecast_Sarima_dataset <-forecast(Sarima_dataset, h = h)
Forecast_Sarima_dataset
plot(Forecast_Sarima_dataset)
#Error Measures
Sarima_errors_dataset = dataset_test - forecast(Sarima_dataset, h = h)$mean
Sarima_ME_dataset = mean(Sarima_errors_dataset) #Mean error
Sarima_MSE_dataset<- mean(Sarima_errors_dataset^2) #Mean squared error
Sarima_MAE_dataset<- mean(abs(Sarima_errors_dataset)) #Mean absolute error
Sarima_MAPE_dataset<- 100 * mean(abs(Sarima_errors_dataset)/dataset_test) #Mean absolute percentage error
Sarima_RMSE_dataset<- sqrt(mean(Sarima_errors_dataset^2)) #Root mean squared error


## Rolling origin for Sarima

dataset_rolling_forecasts_Sarima <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_Sarima <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_Sarima) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_Sarima) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_Sarima) <- dimnames(dataset_rolling_forecasts_Sarima)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  #dataset_rolling_holdout_ES_ANA[i,] <- dataset_rolling_test[i-1+(1:h)]
  dataset_rolling_holdout_Sarima[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_Sarima[i,] <- forecast(arima(dataset_rolling_train_set, order=c(0,1,2), seasonal=c(0,1,1)),h=H)$mean
}

## MAPE for Rolling origin of Sarima
Rolling_errors_dataset_Sarima <- dataset_rolling_holdout_Sarima - dataset_rolling_forecasts_Sarima
Rolling_ME_dataset_Sarima<- mean(Rolling_errors_dataset_Sarima) #Mean error
Rolling_MSE_dataset_Sarima<- mean(Rolling_errors_dataset_Sarima^2) #Mean squared error
Rolling_MAE_dataset_Sarima<- mean(abs(Rolling_errors_dataset_Sarima)) #Mean absolute error
Rolling_MAPE_dataset_Sarima<- 100 * mean(abs(Rolling_errors_dataset_Sarima)/dataset_rolling_holdout_Sarima)
Rolling_RMSE_dataset_Sarima<- sqrt(mean(Rolling_errors_dataset_Sarima^2)) #Root mean squared error


# Auto ARIMA Model --------------------------------------------------------


# Try Auto ARIMA Model
auto_fit_dataset <- auto.arima(dataset_train,  trace = TRUE)
auto_fit_dataset
Forecast_auto_fit_dataset <- forecast(auto_fit_dataset, h = h)
plot(Forecast_auto_fit_dataset)
checkresiduals(auto_fit_dataset)
#Error measures for auto arima
auto_errors_dataset = dataset_test - Forecast_auto_fit_dataset
auto_ME_dataset <- mean(auto_errors_dataset) #Mean error
auto_MSE_dataset <- mean(auto_errors_dataset^2) #Mean squared error
auto_MAE_dataset <- mean(abs(auto_errors_dataset)) #Mean absolute error
auto_MAPE_dataset <- 100 * mean(abs(auto_errors_dataset)/dataset_test) #Mean absolute percentage error
auto_RMSE_dataset <- sqrt(mean(auto_errors_dataset^2)) #Root mean squared error

auto_MAPE_dataset
auto_RMSE_dataset

## Rolling origin for Auto Arima

dataset_rolling_forecasts_AutoArima <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_AutoArima <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_AutoArima) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_AutoArima) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_AutoArima) <- dimnames(dataset_rolling_forecasts_AutoArima)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  #dataset_rolling_holdout_ES_ANA[i,] <- dataset_rolling_test[i-1+(1:h)]
  dataset_rolling_holdout_AutoArima[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_AutoArima[i,] <- forecast(auto.arima(dataset_rolling_train_set),h=H)$mean
}

## MAPE for Rolling origin of Auto Arima
Rolling_errors_dataset_AutoArima<- dataset_rolling_holdout_AutoArima - dataset_rolling_forecasts_AutoArima
Rolling_ME_dataset_AutoArima<- mean(Rolling_errors_dataset_AutoArima) #Mean error
Rolling_MSE_dataset_AutoArima<- mean(Rolling_errors_dataset_AutoArima^2) #Mean squared error
Rolling_MAE_dataset_AutoArima<- mean(abs(Rolling_errors_dataset_AutoArima)) #Mean absolute error
Rolling_MAPE_dataset_AutoArima<- 100 * mean(abs(Rolling_errors_dataset_AutoArima)/dataset_rolling_holdout_AutoArima)
Rolling_RMSE_dataset_AutoArima<- sqrt(mean(Rolling_errors_dataset_AutoArima^2)) #Root mean squared error

Rolling_MAPE_dataset_AutoArima
Rolling_RMSE_dataset_AutoArima
# Auto Seasonal ARIMA Model -----------------------------------------------

library("smooth")

# Try Auto Sarima Model
autosarima_fit_dataset <- auto.ssarima(dataset_train,  stepwise=FALSE , approximation=FALSE)
autosarima_fit_dataset
Forecast_autosarima_fit_dataset <- forecast(autosarima_fit_dataset, h = h)$mean
plot(Forecast_autosarima_fit_dataset)
#Error measures for auto arima
autosarima_errors_dataset = dataset_test - Forecast_autosarima_fit_dataset
autosarima_ME_dataset <- mean(autosarima_errors_dataset) #Mean error
autosarima_MSE_dataset <- mean(autosarima_errors_dataset^2) #Mean squared error
autosarima_MAE_dataset <- mean(abs(autosarima_errors_dataset)) #Mean absolute error
autosarima_MAPE_dataset <- 100 * mean(abs(autosarima_errors_dataset)/dataset_test) #Mean absolute percentage error
autosarima_RMSE_dataset <- sqrt(mean(autosarima_errors_dataset^2)) #Root mean squared error

#detach("package:smooth", unload = TRUE)

## Rolling origin for Auto Seasonal Arima

dataset_rolling_forecasts_AutoSArima <- matrix(NA, nrow=origins, ncol=H)
dataset_rolling_holdout_AutoSSarima <- matrix(NA, nrow=origins, ncol=H)
colnames(dataset_rolling_forecasts_AutoSArima) <- paste0("horizon",c(1:H))
rownames(dataset_rolling_forecasts_AutoSArima) <- paste0("origin",c(1:origins))
dimnames(dataset_rolling_holdout_AutoSSarima) <- dimnames(dataset_rolling_forecasts_AutoSArima)
for(i in 1:origins)
{
  # Create a ts object out of the dataset data
  dataset_rolling_train_set <- ts(dataset[1:(dataset_rolling_train_length+i-1)],
                                  frequency=frequency(dataset),
                                  start=start(dataset))
  
  # Write down the holdout values from the test set
  dataset_rolling_holdout_AutoSSarima[i,] <- dataset_rolling_test[i-1+(1:H)]
  
  # Produce forecasts and write them down
  dataset_rolling_forecasts_AutoSArima[i,] <- forecast(auto.ssarima(dataset_rolling_train_set),h=H)$mean
  
}

## MAPE for Rolling origin of Auto Seasonal Arima
Rolling_errors_dataset_AutoSSarima<- dataset_rolling_holdout_AutoSSarima - dataset_rolling_forecasts_AutoSArima
Rolling_ME_dataset_AutoSSarima<- mean(Rolling_errors_dataset_AutoSSarima) #Mean error
Rolling_MSE_dataset_AutoSSarima<- mean(Rolling_errors_dataset_AutoSSarima^2) #Mean squared error
Rolling_MAE_dataset_AutoSSarima<- mean(abs(Rolling_errors_dataset_AutoSSarima)) #Mean absolute error
Rolling_MAPE_dataset_AutoSSarima<- 100 * mean(abs(Rolling_errors_dataset_AutoSSarima)/dataset_rolling_holdout_AutoSSarima)
Rolling_RMSE_dataset_AutoSSarima<- sqrt(mean(Rolling_errors_dataset_AutoSSarima^2)) #Root mean squared error

# Neural Network ----------------------------------------------------------

#Neural Network Auto regression
ann1 <-nnetar(dataset_train)

# Check the summary
summary(ann1)

# Check the ACF/PACF plot of residuals
tsdisplay(residuals(ann1))

# Forecast using NN
accnfcst<-forecast(ann1,h=h)
accnfcst
# Plot the forecast
autoplot(accnfcst)

#We create a simulation matrix to support 9 different outputs.
sim <- ts(matrix(0, nrow=12L, ncol=9L),start = end(dataset_train)[1L]+1L, frequency = 7)
#Simulate 9 possible future sample paths using bootstrapping. You will get a warning related to the 
#column names, just ignore it: 
for(i in seq(9))
  sim[,i] <- simulate(ann1, nsim=12)

autoplot(dataset_train) + autolayer(sim)

fcast <- forecast(ann1, PI=TRUE, h=14)
autoplot(fcast)

#Error measures for NN
NN_errors_dataset = dataset_test - (accnfcst$mean)
NN_ME_dataset <- mean(NN_errors_dataset) #Mean error
NN_MSE_dataset <- mean(NN_errors_dataset^2) #Mean squared error
NN_MAE_dataset <- mean(abs(NN_errors_dataset)) #Mean absolute error
NN_MAPE_dataset <- 100 * mean(abs(NN_errors_dataset)/dataset_test) #Mean absolute percentage error
NN_RMSE_dataset <- sqrt(mean(NN_errors_dataset^2)) #Root mean square

# Simple Regression --------------------------------------------------------------
#Import sdata
dataset <- read_excel("Assignment 1 Data.xls")

#colnames(data) <- c("Date","Transactions")
colnames(dataset) <- c("Transactions")

#Converting data to time series
dataset <- ts(dataset, frequency = 365, start = c(1996,77))

# Approach 1 -Replace with corresponding day of week data
which_na(dataset)

# Approach 2 - Replace with interpolation method
dataset <- na_interpolation(dataset)


# Find the outliers in the series
out <- boxplot.stats(dataset)$out 
out
out_ind <- which(dataset %in% c(out))
out_ind
dataset[c(163,167,196,235,265,283,538,557,558,559,561,562,563,565,586,587,592,593,594,600
          ,621,628,649,670,684,691,698,711,712,726)]

#Replacing the outliers with median values 
series_median = median(dataset)
series_median
dataset[c(163,167,196,235,265,283,538,557,558,559,561,562,563,565,586,587,592,593,594,600
          ,621,628,649,670,684,691,698,711,712,726)] = series_median

# Split the data into train and test sets
dataset_train <- window(dataset, start(dataset), (1998+66/365))
dataset_train
# Split the data into train and test sets
dataset_test <- window(dataset, (1998+67/365), end(dataset))
dataset_test

# Fit Simple Regression
Simple_Regression <- lm(Transactions ~ 1  , data=dataset_train)
# Summary
summary(Simple_Regression)
#Extract Residuals
Simple_Regression_residuals <- residuals(Simple_Regression)
#We will also need fitted values for our analysis, which can be extracted using fitted():
#Extract Residuals
Simple_Regression_fitted <- fitted(Simple_Regression)
#Plot Histogram
hist(Simple_Regression_residuals)
#QQ-Plot
qqnorm(Simple_Regression_residuals)
qqline(Simple_Regression_residuals)
#Jarque-Bera test
jarque.bera.test(Simple_Regression_residuals)
#Shapiro-Wilk test
shapiro.test(Simple_Regression_residuals)
#Kolmogorov-Smirnov test
ks.test(Simple_Regression_residuals,y="rnorm")
#Plot Residuals against Fitted Values
plot(Simple_Regression_fitted, Simple_Regression_residuals)
#Plot Residuals against Fitted Values
plot(Simple_Regression_fitted, Simple_Regression_residuals^2)
#ACF and PACF of the residuals
tsdisplay(Simple_Regression_residuals)

# Create Studentised Residuals
Simple_Regression_st <- rstandard(Simple_Regression)
# Plot the Residuals
plot(Simple_Regression_st)
# Draw two horizontal lines at 2 and -2 in red
abline(h=c(-2,2),col="red")

#Forecast from Simple Regression
Forecast_Simple_Regression <- predict(Simple_Regression, (as.data.frame(dataset_test)))
Forecast_Simple_Regression

#Error measures for Simple Regression
Simple_Regression_errors_dataset = dataset_test - Forecast_Simple_Regression
Simple_Regression_ME_dataset <- mean(Simple_Regression_errors_dataset) #Mean error
Simple_Regression_MSE_dataset <- mean(Simple_Regression_errors_dataset^2) #Mean squared error
Simple_Regression_MAE_dataset <- mean(abs(Simple_Regression_errors_dataset)) #Mean absolute error
Simple_Regression_MAPE_dataset <- 100 * mean(abs(Simple_Regression_errors_dataset)/dataset_test) #Mean absolute percentage error
Simple_Regression_RMSE_dataset <- sqrt(mean(Simple_Regression_errors_dataset^2)) #Root mean squared error

Simple_Regression_errors_dataset
Simple_Regression_MAPE_dataset

# Multiple Regression Model Parameters-----------------------------------------------------


library("greybox")

# Add dummy variables for lag and seasonal variables

# Auto regressive model with only lag - seasonal variable

# The second one assumes that the
#seasonality has a stochastic structure (implying that it may change over time) and uses lagged
#variables.

#Lags of dataset

L1_dataset <- lag((as.vector(dataset)),k=1)
L1_dataset
L2_dataset <- lag((as.vector(L1_dataset)),k=1)
L2_dataset
L3_dataset <- lag((as.vector(L2_dataset)),k=1)
L3_dataset
L4_dataset <- lag((as.vector(L3_dataset)),k=1)
L4_dataset
L5_dataset <- lag((as.vector(L4_dataset)),k=1)
L5_dataset
L6_dataset <- lag((as.vector(L5_dataset)),k=1)
L6_dataset
L7_dataset <- lag((as.vector(L6_dataset)),k=1)
L7_dataset
L8_dataset <- lag((as.vector(L7_dataset)),k=1)
L8_dataset
L9_dataset <- lag((as.vector(L8_dataset)),k=1)
L9_dataset
L10_dataset <- lag((as.vector(L9_dataset)),k=1)
L10_dataset
L11_dataset <- lag((as.vector(L10_dataset)),k=1)
L11_dataset
L12_dataset <- lag((as.vector(L11_dataset)),k=1)
L12_dataset
L13_dataset <- lag((as.vector(L12_dataset)),k=1)
L13_dataset
L14_dataset <- lag((as.vector(L13_dataset)),k=1)
L14_dataset


#Add all Lags to the Data
dataset_colnames <- colnames(dataset)
dataset <- cbind(dataset, L1_dataset, L2_dataset, L3_dataset, L4_dataset, L5_dataset, L6_dataset, L7_dataset, L8_dataset, L9_dataset, L10_dataset, L11_dataset, L12_dataset, L13_dataset, L14_dataset)

# Change the column names
colnames(dataset) <- c("Transactions", "L1_dataset", "L2_dataset", "L3_dataset", "L4_dataset", "L5_dataset", "L6_dataset", "L7_dataset", "L8_dataset", "L9_dataset", "L10_dataset", "L11_dataset", "L12_dataset", "L13_dataset", "L14_dataset")

# Add the seasonal dummies

#Create Seasonal Dummies
Mon <- rep(c(1,0,0,0,0,0,0),105)
Tue <- rep(c(0,1,0,0,0,0,0),105)
Wed <- rep(c(0,0,1,0,0,0,0),105)
Thu <- rep(c(0,0,0,1,0,0,0),105)
Fri <- rep(c(0,0,0,0,1,0,0),105)
Sat <- rep(c(0,0,0,0,0,1,0),105)
Sun <- rep(c(0,0,0,0,0,0,1),105)

# Add the seasonal dummies to the dataset
dataset <- cbind(dataset,Mon,Tue,Wed,Thu,Fri, Sat, Sun)

# Change the column names
colnames(dataset) <- c("Transactions", "L1_dataset", "L2_dataset", "L3_dataset", "L4_dataset", "L5_dataset", "L6_dataset", "L7_dataset", "L8_dataset", "L9_dataset", "L10_dataset", "L11_dataset", "L12_dataset", "L13_dataset", "L14_dataset","Mon","Tue","Wed","Thu", "Fri", "Sat", "Sun")

# Add the trend dummy

## Create Trend
dataset_trend <- c(1:735)

#Add trebd to the Data
data_colnames <- colnames(dataset)
dataset <- cbind(dataset, dataset_trend)

# Change the column names
colnames(dataset) <- c("Transactions", "L1_dataset", "L2_dataset", "L3_dataset", "L4_dataset", "L5_dataset", "L6_dataset", "L7_dataset", "L8_dataset", "L9_dataset", "L10_dataset", "L11_dataset", "L12_dataset", "L13_dataset", "L14_dataset","Mon","Tue","Wed","Thu", "Fri", "Sat", "Sun", "dataset_trend")

# Split the data into train and test sets
dataset_train <- window(dataset, start(dataset), (1998+66/365))
dataset_train
# Split the data into train and test sets
dataset_test <- window(dataset, (1998+67/365), end(dataset))
dataset_test

# Regression model with lags Model ---------------------------------------------------------

# Model
lags_model <- lm(Transactions ~   L1_dataset  + L2_dataset + L3_dataset+ L4_dataset + L5_dataset + L6_dataset +  L7_dataset + L8_dataset + L9_dataset + L10_dataset + L11_dataset + L12_dataset + L13_dataset, data=dataset_train)

# L1_dataset + L2_dataset + L4_dataset + L7_dataset
summary(lags_model)

tsdisplay(residuals(lags_model)) 

# Check for multi collinearity
#VIF for fit4
VIF(lags_model)

#Extract Residuals
lags_model_residuals <- residuals(lags_model)
#We will also need fitted values for our analysis, which can be extracted using fitted():
#Extract Residuals
lags_model_fitted <- fitted(lags_model)
#Plot Histogram
hist(lags_model_residuals)
#QQ-Plot
qqnorm(lags_model_residuals)
qqline(lags_model_residuals)
#Jarque-Bera test
jarque.bera.test(lags_model_residuals)
#Shapiro-Wilk test
shapiro.test(lags_model_residuals)
#Kolmogorov-Smirnov test
ks.test(lags_model_residuals,y="rnorm")
#Plot Residuals against Fitted Values
plot(lags_model_fitted, lags_model_residuals)
#Plot Residuals against Fitted Values
plot(lags_model_fitted, lags_model_residuals^2)
#ACF and PACF of the residuals
tsdisplay(lags_model_residuals)

#Forecast from lags_model
Forecast_lags_model <- predict(lags_model, (as.data.frame(dataset_test)))
plot(Forecast_lags_model)
Forecast_lags_model

#Error measures for lags_model
lags_model_errors_dataset = dataset_test - Forecast_lags_model
lags_model_ME_dataset <- mean(lags_model_errors_dataset) #Mean error
lags_model_MSE_dataset <- mean(lags_model_errors_dataset^2) #Mean squared error
lags_model_MAE_dataset <- mean(abs(lags_model_errors_dataset)) #Mean absolute error
lags_model_MAPE_dataset <- 100 * mean(abs(lags_model_errors_dataset)/dataset_test) #Mean absolute percentage error
lags_model_RMSE_dataset <- sqrt(mean(lags_model_errors_dataset^2)) #Root mean squared error


# Regression model with seasonal dummies ----------------------------------


# when you consider the whole dataset in below lm function as at first index and you apply seasonal dummies to whole dataset, data here is dataset_train.after adding this you need to split the dataset into train and test and then you will get the result.

#Use the Seasonal Dummies
seasonaldummies <- lm(Transactions ~  Mon  + Tue +  Wed + Thu + Fri +  Sat , data=dataset_train)
summary(seasonaldummies)
dataset_train

#Extract Residuals
seasonaldummies_residuals <- residuals(seasonaldummies)
#We will also need fitted values for our analysis, which can be extracted using fitted():
#Extract Residuals
seasonaldummies_fitted <- fitted(seasonaldummies)
#Plot Histogram
hist(seasonaldummies_residuals)
#QQ-Plot
qqnorm(seasonaldummies_residuals)
qqline(seasonaldummies_residuals)
#Jarque-Bera test
jarque.bera.test(seasonaldummies_residuals)
#Shapiro-Wilk test
shapiro.test(seasonaldummies_residuals)
#Kolmogorov-Smirnov test
ks.test(seasonaldummies_residuals,y="rnorm")
#Plot Residuals against Fitted Values
plot(seasonaldummies_fitted, seasonaldummies_residuals)
#Plot Residuals against Fitted Values
plot(seasonaldummies_fitted, seasonaldummies_residuals^2)
#ACF and PACF of the residuals
tsdisplay(seasonaldummies_residuals)

#Forecast from seasonal dummies
Forecast_seasonaldummies <- predict(seasonaldummies, dataset_test)
plot(Forecast_seasonaldummies)
Forecast_seasonaldummies


#Error measures for seasonal dummies
seasonaldummies_errors_dataset = dataset_test - Forecast_seasonaldummies$mean
seasonaldummies_ME_dataset <- mean(seasonaldummies_errors_dataset) #Mean error
seasonaldummies_MSE_dataset <- mean(seasonaldummies_errors_dataset^2) #Mean squared error
seasonaldummies_MAE_dataset <- mean(abs(seasonaldummies_errors_dataset)) #Mean absolute error
seasonaldummies_MAPE_dataset <- 100 * mean(abs(seasonaldummies_errors_dataset)/dataset_test) #Mean absolute percentage error
seasonaldummies_RMSE_dataset <- sqrt(mean(seasonaldummies_errors_dataset^2)) #Root mean squared error

seasonaldummies_errors_dataset
seasonaldummies_MAPE_dataset

# Auto regressive model with lag , seasonal dummies variable --------------

Lag_seasonal <- lm(Transactions ~ L1_dataset + L2_dataset  + L4_dataset  + L7_dataset + Mon + Tue + Wed  + Thu + Fri + Sat , data=dataset_train)

summary(Lag_seasonal)

tsdisplay(residuals(Lag_seasonal)) 

# Check for multi collinearity
#VIF for fit4

VIF(Lag_seasonal)

#Extract Residuals
Lag_seasonal_residuals <- residuals(Lag_seasonal)
#We will also need fitted values for our analysis, which can be extracted using fitted():
#Extract Residuals
Lag_seasonal_fitted <- fitted(Lag_seasonal)
#Plot Histogram
hist(Lag_seasonal_residuals)
#QQ-Plot
qqnorm(Lag_seasonal_residuals)
qqline(Lag_seasonal_residuals)
#Jarque-Bera test
jarque.bera.test(Lag_seasonal_residuals)
#Shapiro-Wilk test
shapiro.test(Lag_seasonal_residuals)
#Kolmogorov-Smirnov test
ks.test(Lag_seasonal_residuals,y="rnorm")
#Plot Residuals against Fitted Values
plot(Lag_seasonal_fitted, Lag_seasonal_residuals)
#Plot Residuals against Fitted Values
plot(Lag_seasonal_fitted, Lag_seasonal_residuals^2)
#ACF and PACF of the residuals
tsdisplay(Lag_seasonal_residuals)

#Forecast from Lag Seasonal
Forecast_Lag_seasonal <- predict(Lag_seasonal, (as.data.frame(dataset_test)))
plot(Forecast_Lag_seasonal)
Forecast_Lag_seasonal

#Error measures for Lag Seasonal
Lag_seasonal_errors_dataset = dataset_test - Forecast_Lag_seasonal
Lag_seasonal_ME_dataset <- mean(Lag_seasonal_errors_dataset) #Mean error
Lag_seasonal_MSE_dataset <- mean(Lag_seasonal_errors_dataset^2) #Mean squared error
Lag_seasonal_MAE_dataset <- mean(abs(Lag_seasonal_errors_dataset)) #Mean absolute error
Lag_seasonal_MAPE_dataset <- 100 * mean(abs(Lag_seasonal_errors_dataset)/dataset_test) #Mean absolute percentage error
Lag_seasonal_RMSE_dataset <- sqrt(mean(Lag_seasonal_errors_dataset^2)) #Root mean squared error

Lag_seasonal_errors_dataset
Lag_seasonal_MAPE_dataset

# Auto regressive model with lag , seasonal dummies and trend variables --------

Lag_seasonal_trend <- lm(Transactions ~ L1_dataset + L2_dataset + L4_dataset + L7_dataset +   Wed  + Thu + Fri + Sat + dataset_trend , data=dataset_train)

summary(Lag_seasonal_trend)

tsdisplay(residuals(Lag_seasonal_trend)) 

# Check for multi collinearity
#VIF for fit4

VIF(Lag_seasonal_trend)

#Extract Residuals
Lag_seasonal_trend_residuals <- residuals(Lag_seasonal_trend)
#We will also need fitted values for our analysis, which can be extracted using fitted():
#Extract Residuals
Lag_seasonal_trend_fitted <- fitted(Lag_seasonal_trend)
#Plot Histogram
hist(Lag_seasonal_trend_residuals)
#QQ-Plot
qqnorm(Lag_seasonal_trend_residuals)
qqline(Lag_seasonal_trend_residuals)
#Jarque-Bera test
jarque.bera.test(Lag_seasonal_trend_residuals)
#Shapiro-Wilk test
shapiro.test(Lag_seasonal_trend_residuals)
#Kolmogorov-Smirnov test
ks.test(Lag_seasonal_trend_residuals,y="rnorm")
#Plot Residuals against Fitted Values
plot(Lag_seasonal_trend_fitted, Lag_seasonal_trend_residuals)
#Plot Residuals against Fitted Values
plot(Lag_seasonal_trend_fitted, Lag_seasonal_trend_residuals^2)
#ACF and PACF of the residuals
tsdisplay(Lag_seasonal_trend_residuals)



#Forecast from Lag Seasonal Trend
Forecast_Lag_seasonal_trend <- predict(Lag_seasonal_trend, (as.data.frame(dataset_test)))

plot(Forecast_Lag_seasonal_trend)
Forecast_Lag_seasonal_trend

#Error measures for Lag Seasonal Trend
Lag_seasonal_trend_errors_dataset = dataset_test - Forecast_Lag_seasonal_trend
Lag_seasonal_trend_ME_dataset <- mean(Lag_seasonal_trend_errors_dataset) #Mean error
Lag_seasonal_trend_MSE_dataset <- mean(Lag_seasonal_trend_errors_dataset^2) #Mean squared error
Lag_seasonal_trend_MAE_dataset <- mean(abs(Lag_seasonal_trend_errors_dataset)) #Mean absolute error
Lag_seasonal_trend_MAPE_dataset <- 100 * mean(abs(Lag_seasonal_trend_errors_dataset)/dataset_test) #Mean absolute percentage error
Lag_seasonal_trend_RMSE_dataset <- sqrt(mean(Lag_seasonal_trend_errors_dataset^2)) #Root mean squared error

Lag_seasonal_trend_errors_dataset
Lag_seasonal_trend_MAPE_dataset
Lag_seasonal_trend_RMSE_dataset
Lag_seasonal_trend_RMSE_dataset

# Forecast using best regression ------------------------------------------

Lag_seasonal_trend <- lm(Transactions ~ L1_dataset + L2_dataset + L4_dataset + L7_dataset +   Wed  + Thu + Fri + Sat + dataset_trend , data=dataset)

summary(Lag_seasonal_trend)

#Forecast from Lag Seasonal Trend
Forecast_Lag_seasonal_trend <- predict(Lag_seasonal_trend, (as.data.frame(dataset_test)))
plot(Forecast_Lag_seasonal_trend)
Forecast_Lag_seasonal_trend

# Model with AIC both selection -------------------------------------------

all_variable <- lm(Transactions ~ . , data=dataset_train)

AICSelection_directionmodel <- step (all_variable, direction = "both")

summary(AICSelection_directionmodel)

tsdisplay(residuals(AICSelection_directionmodel)) 

# Check for multi collinearity
#VIF for fit4

VIF(AICSelection_directionmodel)

#Extract Residuals
AICSelection_directionmodel_residuals <- residuals(AICSelection_directionmodel)
#We will also need fitted values for our analysis, which can be extracted using fitted():
#Extract Residuals
AICSelection_directionmodel_fitted <- fitted(AICSelection_directionmodel)
#Plot Histogram
hist(AICSelection_directionmodel_residuals)
#QQ-Plot
qqnorm(AICSelection_directionmodel_residuals)
qqline(AICSelection_directionmodel_residuals)
#Jarque-Bera test
jarque.bera.test(AICSelection_directionmodel_residuals)
#Shapiro-Wilk test
shapiro.test(AICSelection_directionmodel_residuals)
#Kolmogorov-Smirnov test
ks.test(AICSelection_directionmodel_residuals,y="rnorm")
#Plot Residuals against Fitted Values
plot(AICSelection_directionmodel_fitted, AICSelection_directionmodel_residuals)
#Plot Residuals against Fitted Values
plot(AICSelection_directionmodel_fitted, AICSelection_directionmodel_residuals^2)
#ACF and PACF of the residuals
tsdisplay(AICSelection_directionmodel_residuals)

# Forecast using AIC selection model
Forecast_AICSelection_directionmodel <- predict(AICSelection_directionmodel, dataset_test)

plot(Forecast_AICSelection_directionmodel)
Forecast_AICSelection_directionmodel

#Error measures for Lag Seasonal Trend
AICSelection_directionmodel_errors_dataset = dataset_test - Forecast_AICSelection_directionmodel
AICSelection_directionmodel_ME_dataset <- mean(AICSelection_directionmodel_errors_dataset) #Mean error
AICSelection_directionmodel_MSE_dataset <- mean(AICSelection_directionmodel_errors_dataset^2) #Mean squared error
AICSelection_directionmodel_MAE_dataset <- mean(abs(AICSelection_directionmodel_errors_dataset)) #Mean absolute error
AICSelection_directionmodel_MAPE_dataset <- 100 * mean(abs(AICSelection_directionmodel_errors_dataset)/dataset_test) #Mean absolute percentage error
AICSelection_directionmodel_RMSE_dataset <- sqrt(mean(AICSelection_directionmodel_errors_dataset^2)) #Root mean squared error

AICSelection_directionmodel_errors_dataset
AICSelection_directionmodel_MAPE_dataset
AICSelection_directionmodel_RMSE_dataset
## Regression validation


# Checking Accuracy -------------------------------------------------------

#Arithmetic mean method
accuracy(Forecast_Arithmetic_mean_dataset,dataset_test)
#Check the accuracy for simple moving average
accuracy(Forecast_SMA_dataset,dataset_test)
#Naive method
accuracy(Naive_method_dataset,dataset_test)
#ES method Forecast_ES_ANA_opt_dataset
accuracy(Forecast_ES_ANA_opt_dataset,dataset_test)
#ES method Forecast_ES_ANA_opt_dataset
accuracy(Forecast_ES_ANA_opt_dataset,dataset_test)
#ES method Forecast_ES_AAA_opt_dataset
accuracy(Forecast_ES_AAA_opt_dataset,dataset_test)

# Accuracy of all ARIMA Models

accuracy(FC_Arima711_Seasoanl013_dataset, dataset_test)
accuracy(FC_Arima202_Seasoanl012_dataset, dataset_test)
accuracy(FC_Arima202_Seasoanl111_dataset, dataset_test)
accuracy(FC_Arima113_Seasoanl212_dataset, dataset_test)
#accuracy(FC_Arima212_Seasoanl102_dataset, dataset_test)
accuracy(FC_Arima302_Seasoanl112_dataset, dataset_test)
accuracy(FC_Arima402_Seasoanl311_dataset, dataset_test)

#Check the accuracy for Sarima model
accuracy(Forecast_Sarima_dataset,dataset_test)
#Check the accuracy for auto arima model
accuracy(Forecast_auto_fit_dataset,dataset_test)
#Check the accuracy for auto arima model
accuracy(Forecast_autosarima_fit_dataset,dataset_test)
#Check the accuracy for NN model
accuracy(accnfcst,dataset_test)

# Creating a summary table for comparison of error measures ---------------

Summary_stats <- matrix(c(Arithmetic_ME_dataset,Arithmetic_MSE_dataset,Arithmetic_MAE_dataset,Arithmetic_MAPE_dataset,Arithmetic_RMSE_dataset), ncol=5, byrow=TRUE)
Summary_stats <- rbind(Summary_stats,c(SMA_ME_dataset,SMA_MSE_dataset,SMA_MAE_dataset,SMA_MAPE_dataset,SMA_RMSE_dataaset3))
Summary_stats<- rbind(Summary_stats,c(Naive_ME_dataset,Naive_MSE_dataset,Naive_MAE_dataset,Naive_MAPE_dataset,Naive_RMSE_dataset))
Summary_stats<- rbind(Summary_stats,c(ES_ANA_ME_dataset,ES_ANA_MSE_dataset,ES_ANA_MAE_dataset,ES_ANA_RMSE_dataset,ES_ANA_MAPE_dataset))
Summary_stats<- rbind(Summary_stats,c(ES_AAA_ME_dataset,ES_AAA_MSE_dataset,ES_AAA_MAE_dataset,ES_AAA_RMSE_dataset,ES_AAA_MAPE_dataset))
Summary_stats<- rbind(Summary_stats,c(auto_ME_dataset,auto_MSE_dataset,auto_MAE_dataset,auto_MAPE_dataset,auto_RMSE_dataset))
Summary_stats<- rbind(Summary_stats,c(Sarima_ME_dataset,Sarima_MSE_dataset,Sarima_MAE_dataset,Sarima_MAPE_dataset,Sarima_RMSE_dataset))
colnames(Summary_stats) <- c('ME','MSE','MAE','MAPE','RMSE')
rownames(Summary_stats) <- c('Arithmetic Mean','SMA','Naive approach','ES ANA','ES AAA','Auto Arima','SARIMA')
Summary_stats <- as.table(Summary_stats)
Summary_stats
View(Summary_stats)
