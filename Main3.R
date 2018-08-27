# Feature Information ----------------------------------------------------

# • ip: ip address of click.
# • app: app id for marketing.
# • device: device type id of user mobile phone (e.g., iphone 6 plus, iphone 7, huawei mate 7, etc.)
# • os: os version id of user mobile phone
# • channel: channel id of mobile ad publisher
# • click_time: timestamp of click (UTC)
# • attributed_time: if user download the app for after clicking an ad, this is the time of the app download
# • is_attributed: the target that is to be predicted, indicating the app was downloaded

# Install packages --------------------------------------------------------

# install.packages("readr")
# install.packages("data.table")
# install.packages("tibble")
# install.packages("fasttime")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("ISLR")
# install.packages("ggplot2")
# install.packages("purrr")
# install.packages("e1071")
# install.packages("MLmetrics")
# install.packages("pROC")
# install.packages("caret")

# 1) Data Import ---------------------------------------------------------------

#install.packages("readr")
library(readr)

setwd("C:\\Users\\nilsb\\sciebo\\Master\\2. Semester\\Seminar\\Projekt\\Data")

#Load initial dataset
# install.packages("data.table")
# install.packages("tibble")
library(data.table)
library(tibble)
back_up = as.tibble(fread("train-all.csv", na.strings = ""))
dataset = as.tibble(fread("train-all.csv", na.strings = ""))  # dataset is synonymous with training set

# Convert the variables "ip", "app", "device", "os", "channel" & the target variable "is_attributed" into factors 
convert_features <- c("ip", "app", "device", "os", "channel", "is_attributed")
dataset[convert_features] <- lapply(dataset[convert_features], factor)

# Convert the variables "click_time" & "attributed_time" into POSIXct format
# install.packages("fasttime")
library(fasttime)
dataset$click_time = fastPOSIXct(dataset$click_time)
dataset$attributed_time = fastPOSIXct(dataset$attributed_time)

# Load data which should be forecasted
data_forecast = as.tibble(fread("test-all.csv", na.strings = ""))

# Convert the variables "click_id", "ip", "app", "device", "os" and "channel" into factors
convert_features_fc <- c("ip", "app", "device", "os", "channel")
data_forecast[convert_features_fc] <- lapply(data_forecast[convert_features_fc], factor)

# Convert "click_time" variable into POSIXct format
data_forecast$click_time = fastPOSIXct(data_forecast$click_time)

# Control if transformation of variables was successful 
head(data_forecast)
head(dataset)


# 2) Data Preparation --------------------------------------------------------
# install.packages("dplyr")
library(dplyr)


# a. Apply binning --------------------------------------------------------

# Separate date and time information into two variables and apply binning on them separately to prepare for Naive Bayes

# install.packages("tidyr")
library(tidyr)

# i. Separation of "click_time"
# Separate blick_time variable into "click_date" and "click_exact_time"
dataset = separate(dataset, col=click_time, into = c("click_date", "click_exact_time"), sep= " ")
str(dataset)

# Convert variables "click_date" and "click_exact_time" into right formats -> Date and Time 
# install.packages("lubridate")
library(lubridate)
dataset$click_date = ymd(dataset$click_date) #choose this for date

# Apply binning on "click_date" and "click_exact_time"

# Binning on click_data
dataset$bins_click_date = NA
dataset$bins_click_date = cut(dataset$click_date, breaks="1 day", labels=FALSE) #works fine if previously converted
# Convert bin variable into factor
dataset$bins_click_date = as.factor(dataset$bins_click_date)
dataset$bins_click_date = factor(dataset$bins_click_date, levels=c("1","2","3","4"))

# Binning on click_exact_time - 12 intervals
dataset$bins_click_exact_time = NA
dataset$bins_click_exact_time <- cut(strptime(dataset$click_exact_time, format = "%H:%M:%S"), breaks=strptime(c("00:00:00","02:00:00","04:00:00","06:00:00","08:00:00","10:00:00","12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00"), format= "%H:%M:%S"), labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22"))
dataset$bins_click_exact_time = as.character(dataset$bins_click_exact_time)
ind = which(is.na(dataset$bins_click_exact_time))
dataset$bins_click_exact_time[ind] = "22-24"
# Convert bin variable into factor
dataset$bins_click_exact_time = as.factor(dataset$bins_click_exact_time)
dataset$bins_click_exact_time = factor(dataset$bins_click_exact_time, levels=c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24"))

str(dataset)

# Binning on click_exact_time - 24 intervals (instead of 12 as default)
dataset$bins_click_exact_time_24 = NA
dataset$bins_click_exact_time_24 <- cut(strptime(dataset$click_exact_time, format = "%H:%M:%S"),
                                        breaks=strptime(c("00:00:00","01:00:00","02:00:00", "03:00:00", "04:00:00", "05:00:00","06:00:00", "07:00:00", "08:00:00","09:00:00", "10:00:00", "11:00:00","12:00:00", "13:00:00", "14:00:00","15:00:00", "16:00:00", "17:00:00","18:00:00", "19:00:00", "20:00:00","21:00:00", "22:00:00", "23:00:00"), format= "%H:%M:%S"),
                                        labels = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22", "22-23"))

dataset$bins_click_exact_time_24 = as.character(dataset$bins_click_exact_time_24)
ind = which(is.na(dataset$bins_click_exact_time_24))
dataset$bins_click_exact_time_24[ind] = "23-24"
# # Convert bin variable into factor
dataset$bins_click_exact_time_24 = as.factor(dataset$bins_click_exact_time_24)
dataset$bins_click_exact_time_24 = factor(dataset$bins_click_exact_time_24, levels=c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22", "22-23", "23-24"))

str(dataset)


# b. Split into Training-/Testset ------------------------------------

# install.packages("ISLR")
library(ISLR)
attach(dataset)
smp_siz = floor(0.90*nrow(dataset))  # creates a value for dividing the data into train and test. In this case the value is defined as 90% of the number of rows in the dataset
smp_siz  # shows the value of the sample size

set.seed(456)   # set seed to ensure we always have same random numbers generated
train_ind = sample(seq_len(nrow(dataset)),size = smp_siz)  # Randomly identifies the rows equal to sample size ( defined in previous instruction) from  all the rows of dataset and stores the row number in train_ind
training_set=dataset[train_ind,] #creates the training dataset with row numbers stored in train_ind
test_set=dataset[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind

# Check whether transformations have worked out and control the size of the training- & testset
str(training_set)
str(test_set)


# 3) Data Exploration Full Dataset --------------------------------------------------------
#install.packages("purrr")
library(purrr)

# a. Check NA's -----------------------------------------------------------

#Check if NA's occur
any(is.na(dataset))
sum(is.na(dataset))
which(is.na(dataset))

#Part of dataset without NA's
dim(dataset[complete.cases(dataset), ])
# alt: na.omit(dataset)

#Part of dataset with NA's
dim(dataset[!complete.cases(dataset), ])

# Check percentage of NA's for each feature for the whole dataset
na_per_feature = apply(dataset, 2, function(col) sum(is.na(col))/length(col))
na_per_feature
is.vector(na_per_feature)

#Check percentage of observations WITHOUT Click Fraud
percent_attributed_dataset = nrow(subset(dataset, dataset$is_attributed==1))/nrow(dataset)
percent_attributed_dataset
#Validate whether the sum the percent of the number of observations WITHOUT Click Fraud and the number of NA's per feature "attributed_time" (where is_attributed = 0) adds up to 1 and thus suggest the conclusion that NA's occur only when 

sum(percent_attributed_dataset, na_per_feature[7])

# Conclusion: NA's only occur in the feature "attributed_time" when "is_attributed"=0, meaning that the app was not downloaded and Click Fraud has happened. This observation does make sense intuitively, since "attributed_time" tells us when an app was downloaded. When an app was not downloaded (is_attributed=0), there can be no information about the download time ("attributed_time").
# We face systematic NA (not MAR). The missingness fully depends on "is_attributed".


# b. Get Basic Statistics -------------------------------------------------------------

# install.packages("dplyr")
library(dplyr)

# Get different datasets based on fraudulent and natural clicks
ds_is_attributed_1 = subset(dataset, subset=dataset$is_attributed==1)
ds_is_attributed_0 = subset(dataset, subset=dataset$is_attributed==0)

#Overview of the count of the factor levels and min/max values of the features (relevant for the time variables)
summary(dataset)
str(dataset)

#Get number of days considered in dataset based on bins created
number_of_days = max(dataset$click_date)-min(dataset$click_date)

#Check distribution of fraudulent and real clicks
plot(dataset$is_attributed)

#Check percentage of NON-FRAUDULENT clicks based on the dataset
percent_natural = nrow(subset(dataset, dataset$is_attributed==1))/nrow(dataset)
percent_natural
#Check absolute number of NON-FRAUDULENT clicks
nrow(subset(dataset, dataset$is_attributed==1))

#Check percentage of FRAUDULENT clicks based on the dataset
percent_fraud = nrow(subset(dataset, dataset$is_attributed==0))/nrow(dataset)
percent_fraud
#Check absolute number of FRAUDULENT clicks
number_frauds = nrow(subset(dataset, dataset$is_attributed==0))

#Get number of unique values per feature
str(dataset)
unique_feature_levels = list(number_ip = length(unique(dataset$ip)),
number_apps = length(unique(dataset$app)),
number_devices = length(unique(dataset$device)),
number_os = length(unique(dataset$os)),
number_channels = length(unique(dataset$channel)))

print(unique_feature_levels)
  
# c. Explore each Feature --------------------------------------------------

# install.packages("ggplot2")
library(ggplot2)

# I. ip
#Visualize the distribution
ggplot(data=dataset, aes(x=ip)) + geom_bar()

#Check how often certain ip-addresses appeared in the dataset 
dataset %>%
  group_by(ip) %>%
  count(sort=T)

# Further analyze the most frequqnt occuring ip 5348
ip_5348 = subset(dataset, subset= dataset$ip == "5348")
nrow(ip_5348)
str(ip_5348)
summary(ip_5348)


# II. app
#Visualize the distribution
ggplot(data=dataset, aes(x=app)) + geom_bar()

#Check how often certain app id's appeared in the dataset
dataset %>%
  group_by(app) %>%
  count(sort=T)


# III. device
#Visualize the distribution
ggplot(data=dataset, aes(x=device)) + geom_bar()

#Check how often certain devices appeared in the dataset
dataset %>%
  group_by(device) %>%
  count(sort=T)

# Analyze the most frequent occuring device 1 
device_1 = subset(dataset, subset= dataset$device == "1")
nrow(device_1)
str(device_1)
summary(device_1)
1-(306495/(4245510+306495))

# IV. os
#Visualize the distribution
ggplot(data=dataset, aes(x=os)) + geom_bar()

#Check how often certain os appeared in the dataset
dataset %>%
  group_by(os) %>%
  count(sort=T)

# V. channel
#Visualize the distribution
ggplot(data=dataset, aes(x=channel)) + geom_bar()

#Check how often certain channels appeared in the dataset
dataset %>%
  group_by(channel) %>%
  count(sort=T)

# VI. click_time

# Compare time distribution based on the click_time of fraudulent and natural clicks
ggplot(data=ds_is_attributed_1, aes(x=ds_is_attributed_1$bins_click_exact_time)) + geom_bar() + ggtitle("Distribution of Natural Clicks based on Time") + xlab("Time Bins - 2 hour intervals") + ylab("Frequency")
ggplot(data=ds_is_attributed_0, aes(x=ds_is_attributed_0$bins_click_exact_time)) + geom_bar() + ggtitle("Distribution of Fraudulent Clicks based on Time") + xlab("Time Bins - 2 hour intervals") + ylab("Frequency")


max(dataset$click_date) 
min(dataset$click_date)

#Dataset represents 4 days - Monday until Thursday

# VII. attributed_time

#Not considered because NA's appear in this variable when is_attributed = 0, therefore it is not a suitable predictor, since it does not help to differentiate between is_attributed = 0 and = 1. 

# d. Check Relationships between Variables ------------------------------------------------------

# I. OS & Device

ct_os_device = table(dataset$os, dataset$device)

# Get other statistics
mean(ct_os_device)
sd(ct_os_device)

# Visualize 
ggplot(data=as.data.frame(ct_os_device), aes(y=Freq)) + geom_boxplot() #We can see that there are certain combinations, which appear very frequently, whereas others not that much

#Apply Chi-Square test to check independence of features device and os
chisq.test(table(dataset$os, dataset$device))

# Interpreation Results Chi-Squared Test: 
# If p-value here is lower than 0.05 (which is the case), then this means that both variables are dependent (null-hypothesis would test whether they are independent).
# The null hypothesis for this test is that there is no relationship between OS  and Device.  The alternative hypothesis is that there is a relationship between OS and Device.
# We get a significant result here. This is no surprise, however, since the sample is very large. 


# Next: Explore interesting specific combinations because os and device 
# Reasons for doing so:
# Check whether a certain combination of os and device can be clearly assigned to a bot.
# Check whether os and device could be independent. Intuitively, one would consider that those features are not independent from each other (e.g. an iPhone always has a certain os, therefore there is a dependency) naturally seem to be dependent features although Naive Bayes assumes independence of features.  

# Get how often of the most frequently occuring combination in the dataset 
max(ct_os_device)

df_ct_os_device=as.data.frame(ct_os_device)
str(df_ct_os_device)

#Figure out the Index of the most frequently occuring combination (with a frequency of 1128878)
df_ct_os_device[df_ct_os_device$Freq == 1128878L,]
# which(df_ct_os_device$Freq == 1128878L)
# High number of occurences for os = 19 and device = 1

#Check whether os = 19 and device = 1 always result in is_attributed = 0
specific_fraud_combination = subset(dataset, subset= dataset$os == 19 & dataset$device == 1)

#Check is_attributed here
summary(specific_fraud_combination)

#Interesting: 1049716 click frauds came from the combination (os = 19 and device = 1), but also 79162 natural

#Calculate the perccentage of (is_attributed = 0 aka Click Fraud) came from os = 19 and device = 1
percentage_fraudulent_specific_combination = 1-(79162/(1049716+79162))
print(percentage_fraudulent_specific_combination)

# This specific combination is 92.9% fraudulent while in the main dataset we have 90.7%

#For comparison, print the percentage of Fraud Clicks in the whole dataset
print(percent_fraud)

#Interpretation: Almost 93% of the observations with os=19 and device=1 are Click Frauds. However, also in the whole dataset we have a percentage of Click Fraud of 90.7 %. Therefore, this result might be not that special. 


# II. Channel & Device

ct_channel_device = table(dataset$channel, dataset$device)

# Get how often of the most frequently occuring combination in the dataset
max(ct_channel_device)

# Get other statistics 
mean(ct_channel_device)
sd(ct_channel_device)

# Visualize
ggplot(data=as.data.frame(ct_channel_device), aes(y=Freq)) + geom_boxplot()
#We can see that there are certain combinations, which appear very frequent, whereas others not that much

#Apply Chi-Square test to check independence 
chisq.test(table(dataset$channel, dataset$device))

# III. App & Channel

ct_app_channel = table(dataset$app, dataset$channel)

# Get how often of the most frequently occuring combination in the dataset 
max(ct_app_channel)

# Get other statistics 
mean(ct_app_channel)
sd(ct_app_channel)

# Visualize
ggplot(data=as.data.frame(ct_app_channel), aes(y=Freq)) + geom_boxplot()

#Apply Chi-Square test to check independence 
chisq.test(table(dataset$app, dataset$channel))

check = as.data.frame(ct_app_channel)
View(check)
str(dataset)

# 4) Modeling --------------------------------------------------------------

# install.packages('e1071')
library(e1071)
# install.packages("MLmetrics")
library(MLmetrics)
# install.packages("pROC")
library(pROC)

## CLASSIFIER: Try Naive Bayes classifiers with different inputs and compare performance to determine the best model specification

# Hyperparameter: Laplace is set to 1
# Why? The small probability added to every outcome ensures that they are all possible even if never previously observed
# Add 1 to all the event to get rid off the problem that multiplying with 0 causes whole probability to get to 0.

# 1) Complete 12
classifier_1 = naiveBayes(is_attributed ~ ip + app + device + os + channel +  bins_click_exact_time, data = training_set, laplace=1)
y_pred_1 = predict(classifier_1, newdata = test_set[-9])
# Get Precision
precision_1 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_1)
print(precision_1)

# 2) Complete 24
classifier_2 = naiveBayes(is_attributed ~ ip + app + device + os + channel + bins_click_exact_time_24, data = training_set, laplace=1)
y_pred_2 = predict(classifier_2, newdata = test_set[-9])
# Get Precision
precision_2 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_2)
print(precision_2)

## --- Test for 12  --- 

# 3) Only IP excluded 12
classifier_3 = naiveBayes(is_attributed ~ app + device + os + channel +   bins_click_exact_time, data= training_set, laplace=1)
y_pred_3 = predict(classifier_3, newdata = test_set[-9])
# Get Precision
precision_3 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_3)
print(precision_3)

# 4)  Excluding OS and IP 12
classifier_4 = naiveBayes(is_attributed ~  app + device + channel +   bins_click_exact_time, data= training_set, laplace=1)
# test_set$y_pred = predict(classifier, newdata = test_set)
y_pred_4 = predict(classifier_4, newdata = test_set[-9])
# Get Precision
precision_4 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_4)
print(precision_4)

# 5) Excluding device 12

classifier_5 = naiveBayes(is_attributed ~ ip + app + os + channel +   bins_click_exact_time, data = training_set, laplace=1)
y_pred_5 = predict(classifier_5, newdata = test_set[-9])
# Get Precision
precision_5 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_5)
print(precision_5)

# 6) Excluding OS 12h
classifier_6 = naiveBayes(is_attributed ~ ip + app + device + channel +   bins_click_exact_time, data = training_set, laplace=1)
y_pred_6 = predict(classifier_6, newdata = test_set[-9])
# Get Precision
precision_6 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_6)
print(precision_6)

# --- Test for 24 ---

# 7) Only IP excluded 24
classifier_7 = naiveBayes(is_attributed ~ app + device + os + channel +   bins_click_exact_time_24, data= training_set, laplace=1)
y_pred_7 = predict(classifier_7, newdata = test_set[-9])
# Get Precision
precision_7 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_7)
print(precision_7)

# 8)  Excluding OS and IP 24
classifier_8 = naiveBayes(is_attributed ~  app + device + channel +   bins_click_exact_time_24, data= training_set, laplace=1)
# test_set$y_pred = predict(classifier, newdata = test_set)
y_pred_8 = predict(classifier_8, newdata = test_set[-9])
# Get Precision
precision_8 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_8)
print(precision_8)

# 9) Excluding device 24
classifier_9 = naiveBayes(is_attributed ~ ip + app + os + channel +   bins_click_exact_time_24, data = training_set, laplace=1)
y_pred_9 = predict(classifier_9, newdata = test_set[-9])
# Get Precision
precision_9 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_9)
print(precision_9)

# 10) Excluding OS 24
classifier_10 = naiveBayes(is_attributed ~ ip + app + device + channel +   bins_click_exact_time_24, data = training_set, laplace=1)
y_pred_10 = predict(classifier_10, newdata = test_set[-9])
# Get Precision
precision_10 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_10)
print(precision_10)

# Two additional models

# 11) Excluding device and ip 12
classifier_11 = naiveBayes(is_attributed ~ ip + app + os + channel + bins_click_exact_time, data = training_set, laplace=1)
y_pred_11 = predict(classifier_11, newdata = test_set[-9])
# Get Precision
precision_11 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_11)
print(precision_11)

# 12) Excluding device and ip 24
classifier_12 = naiveBayes(is_attributed ~ ip + app + os + channel + bins_click_exact_time_24, data = training_set, laplace=1)
y_pred_12 = predict(classifier_12, newdata = test_set[-9])
# Get Precision
precision_12 = Precision(y_true=test_set$is_attributed, y_pred=y_pred_12)
print(precision_12)



# 5) Model Performance -------------------------------------------------------

# CREATE the Confusion Matrix
# install.packages("caret")
library(caret)

confusionMatrix(data = y_pred, reference = test_set$is_attributed)

get_model_performance = function (predict_variable, y_reference = test_set$is_attributed){
  library(MLmetrics)
  library(caret)
  require(MLmetrics)
  require(caret)
  
  Table_Predictions = table(predict_variable)
  Confusion_Matrix = confusionMatrix(data = predict_variable, reference = y_reference)
  Precision_Score = Precision(y_true = y_reference, y_pred = predict_variable)
  Recall_Score = Recall(y_true = y_reference, y_pred = predict_variable)
  F1 = F1_Score(y_true = y_reference, y_pred = predict_variable)
  Accuracy_Score = Accuracy(y_true = y_reference, y_pred = predict_variable)
  
  print(list(Table_Predictions=Table_Predictions, Confusion_Matrix=Confusion_Matrix, Precision_Score=Precision_Score, Recall_Score=Recall_Score, F1=F1, Accuracy_Score=Accuracy_Score))
}

#Get detailled Performance of different models
model_performance_1 = get_model_performance(predict_variable = y_pred_1, y_reference = test_set$is_attributed)
model_performance_2 = get_model_performance(predict_variable = y_pred_2, y_reference = test_set$is_attributed)
model_performance_3 = get_model_performance(predict_variable = y_pred_3, y_reference = test_set$is_attributed)
model_performance_4 = get_model_performance(predict_variable = y_pred_4, y_reference = test_set$is_attributed)
model_performance_5 = get_model_performance(predict_variable = y_pred_5, y_reference = test_set$is_attributed)
model_performance_6 = get_model_performance(predict_variable = y_pred_6, y_reference = test_set$is_attributed)
model_performance_7 = get_model_performance(predict_variable = y_pred_7, y_reference = test_set$is_attributed)
model_performance_8 = get_model_performance(predict_variable = y_pred_8, y_reference = test_set$is_attributed)
model_performance_9 = get_model_performance(predict_variable = y_pred_9, y_reference = test_set$is_attributed)
model_performance_10 = get_model_performance(predict_variable = y_pred_10, y_reference = test_set$is_attributed)

#Percentage of predicted is_attributed=0 and 1 in chosen model
predicted_values_dataset = table(y_pred_2)

# For comparison later, calculate the prediction
percent_predicted_is_attributed_0= predicted_values_dataset[1]/(predicted_values_dataset[1]+predicted_values_dataset[2])
percent_predicted_is_attributed_0
percent_predicted_is_attributed_1 = 1-percent_predicted_is_attributed_0
percent_predicted_is_attributed_1

# 7) Forecast -------------------------------------------------------------

#Apply same Data Preparation on "data_forecast" as on the basic "dataset"

# a. Apply binning --------------------------------------------------------

# i. Separation of "click_time"
#Separate blick_time variable into "click_date" and "click_exact_time"
data_forecast = separate(data_forecast, col=click_time, into = c("click_date", "click_exact_time"), sep= " ")
str(data_forecast)

# Convert variables "click_date" and "click_exact_time" into right formats -> Date and Time 
# install.packages("lubridate")
library(lubridate)
data_forecast$click_date = ymd(data_forecast$click_date) #choose this for date

#Apply binning on "click_date" and "click_exact_time"

# Binning on click_data
data_forecast$bins_click_date = NA
data_forecast$bins_click_date = cut(data_forecast$click_date, breaks="1 day", labels=FALSE) #works fine if previously converted
# Convert bin variable into factor
data_forecast$bins_click_date = as.factor(data_forecast$bins_click_date)
data_forecast$bins_click_date = factor(data_forecast$bins_click_date, levels=c("1","2","3","4"))

# Binning on click_exact_time - 12 intervals
data_forecast$bins_click_exact_time = NA
data_forecast$bins_click_exact_time <- cut(strptime(data_forecast$click_exact_time, format = "%H:%M:%S"), breaks=strptime(c("00:00:00","02:00:00","04:00:00","06:00:00","08:00:00","10:00:00","12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00"), format= "%H:%M:%S"), labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22"))
data_forecast$bins_click_exact_time = as.character(data_forecast$bins_click_exact_time)
ind = which(is.na(data_forecast$bins_click_exact_time))
data_forecast$bins_click_exact_time[ind] = "22-24"
# Convert bin variable into factor
data_forecast$bins_click_exact_time = as.factor(data_forecast$bins_click_exact_time)
data_forecast$bins_click_exact_time = factor(data_forecast$bins_click_exact_time, levels=c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24"))

# b. Get Basic Statistics -------------------------------------------------

# Get different data_forecast subsets based on fraudulent and natural clicks for further analysis
ds_forecast_is_attributed_1 = subset(data_forecast, subset=data_forecast$is_attributed==1)
ds_forecast_is_attributed_0 = subset(data_forecast, subset=data_forecast$is_attributed==0)

#Overview of the count of the factor levels and min/max values of the features (relevant for the time variables)
summary(data_forecast)
str(data_forecast)

#Get number of days considered in data_forecast based on bins created
number_of_days = max(data_forecast$click_date)-min(data_forecast$click_date)

#Check distribution of fraudulent and real clicks
plot(data_forecast$is_attributed)

#Get number of unique values per feature
str(data_forecast)
unique_feature_levels = list(number_ip = length(unique(data_forecast$ip)),
                             number_apps = length(unique(data_forecast$app)),
                             number_devices = length(unique(data_forecast$device)),
                             number_os = length(unique(data_forecast$os)),
                             number_channels = length(unique(data_forecast$channel)))

print(unique_feature_levels)


# c. Explore each Feature -------------------------------------------------

# I. ip
#Visualize the distribution
ggplot(data=data_forecast, aes(x=ip)) + geom_bar()

#Check how often certain ip-addresses appeared in the data_forecast 
data_forecast %>%
  group_by(ip) %>%
  count(sort=T)

# II. app
#Visualize the distribution
ggplot(data=data_forecast, aes(x=app)) + geom_bar()

#Check how often certain app id's appeared in the data_forecast
data_forecast %>%
  group_by(app) %>%
  count(sort=T)

# III. device
#Visualize the distribution
ggplot(data=data_forecast, aes(x=device)) + geom_bar()

#Check how often certain devices appeared in the data_forecast
data_forecast %>%
  group_by(device) %>%
  count(sort=T)

# IV. os
#Visualize the distribution
ggplot(data=data_forecast, aes(x=os)) + geom_bar()

#Check how often certain os appeared in the data_forecast
data_forecast %>%
  group_by(os) %>%
  count(sort=T)

# V. channel
#Visualize the distribution
ggplot(data=data_forecast, aes(x=channel)) + geom_bar()

#Check how often certain channels appeared in the data_forecast
data_forecast %>%
  group_by(channel) %>%
  count(sort=T)

# VI. click_time

# Compare time distribution based on the click_time of fraudulent and natural clicks
ggplot(data=ds_is_attributed_1, aes(x=ds_forecast_is_attributed_1$bins_click_exact_time)) + geom_bar() + ggtitle("Distribution of Natural Clicks based on Time")
ggplot(data=ds_is_attributed_0, aes(x=ds_forecast_is_attributed_0$bins_click_exact_time)) + geom_bar() + ggtitle("Distribution of Fraudulent Clicks based on Time")

max(data_forecast$click_date) 
min(data_forecast$click_date)


# VII. attributed_time

#Not considered because NA's appear in this variable when is_attributed = 0, therefore it is not a suitable predictor, since it does not help to differentiate between is_attributed = 0 and = 1. 


# d. Prediction -----------------------------------------------------------

# Predict is_attributed based on the model trained on the training data
data_forecast$is_attributed_predicted = predict(classifier_2, newdata = data_forecast)

str(data_forecast)
summary(data_forecast)

#FORECAST SET: Check percentage of NON-FRAUDULENT clicks predicted by our model based on the forecast_dataset
percent_natural_forecast = nrow(subset(data_forecast, data_forecast$is_attributed_predicted==1))/nrow(data_forecast)
print(percent_natural_forecast)

percent_fraud_forecast = 1-percent_natural_forecast
print(percent_fraud_forecast)

#TRAINING SET: Compare with percentage of predicted natural clicks (is_attributed = 1) in trained model
percent_natural_dataset = nrow(subset(dataset, dataset$is_attributed==1))/nrow(dataset)
print(percent_natural_dataset)

percent_fraud_dataset = 1-percent_natural_dataset
print(percent_fraud_dataset)


#Compare with percent natural in dataset used for training and testset
percent_natural
#Check absolute number of NON-FRAUDULENT clicks
nrow(subset(data_forecast, data_forecast$is_attributed_predicted==1))

#Check percentage of FRAUDULENT clicks predicted by our model based on the forecast_dataset
percent_fraud_forecast = nrow(subset(data_forecast, data_forecast$is_attributed_predicted==0))/nrow(data_forecast)
percent_fraud_forecast
#Compare with percentage of predicted click frauds (is_attributed = 0) in trained model
percent_predicted_is_attributed_0


#Compare with percent fraud in dataset used for training and testset
percent_fraud
#Check absolute number of FRAUDULENT clicks
number_frauds = nrow(subset(data_forecast, data_forecast$is_attributed_predicted==0))
