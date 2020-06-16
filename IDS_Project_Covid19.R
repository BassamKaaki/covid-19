
#-----------------------------------------------------------------------------------
#Introduction to Data Science Project (Bassam Kaaki)
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#Installing excel package in R
#-----------------------------------------------------------------------------------

install.packages("readxl")

#-----------------------------------------------------------------------------------
#Loading the excel package in R
#-----------------------------------------------------------------------------------

library(readxl)

#-----------------------------------------------------------------------------------
#Importing the dataset (excel sheet) called corona_data
#-----------------------------------------------------------------------------------

#Situation report 113 - May 12, 2020 data
corona_data <- read_excel("/USERS/bassamalkaaki/Library/Mobile Documents/com~apple~CloudDocs/UCLA/UCLA-CLASSES/Intro_Data_Science/IDS_project/corona_data.xlsx")

#Situation report 95 (for comparison purposes - April 24, 2020)

corona_data_proj <- read_excel("/USERS/bassamalkaaki/Library/Mobile Documents/com~apple~CloudDocs/UCLA/UCLA-CLASSES/Intro_Data_Science/IDS_project/corona_data_proj.xlsx")


#-----------------------------------------------------------------------------------
#Coverting the tibble to dataframe
#-----------------------------------------------------------------------------------

#May 12, 2020 data (Situation report 113)
corona_data <- as.data.frame(corona_data) 

#April 24, 2020 data (Situation report 95)
corona_data_project <- as.data.frame(corona_data_proj)

#-----------------------------------------------------------------------------------
#Removal of NA's in dataset and replacing them with the means of their columns
#May 12, 2020 data (Situation report 113)
#-----------------------------------------------------------------------------------

## Removal of 13 NAs from the total_recovered column & replacing with column mean

corona_data$total_recovered[is.na(corona_data$total_recovered)] <- mean(corona_data$total_recovered, na.rm = TRUE)

## Removal of 10 NAs from the active cases column and replacing with column mean

corona_data$active_cases[is.na(corona_data$active_cases)] <- mean(corona_data$active_cases, na.rm = TRUE)

## Removal of 81 NAs from the critical cases column and replacing with column mean

corona_data$critical_cases[is.na(corona_data$critical_cases)] <- mean(corona_data$critical_cases, na.rm = TRUE)

## Removal of 33 NAs from the total_tests column and replacing it with column mean

corona_data$total_tests[is.na(corona_data$total_tests)] <- mean(corona_data$total_tests, na.rm = TRUE)

## Removal of 34 NAs from the tests_1M column and replacing it with column mean

corona_data$tests_1M[is.na(corona_data$tests_1M)] <- mean(corona_data$tests_1M, na.rm = TRUE)

#-----------------------------------------------------------------------------------
#Turn character variables to factor & numeric variables for use in analysis
#May 12, 2020 data (Situation report 113)
#-----------------------------------------------------------------------------------

## Factoring the country variable

corona_data$country <- as.factor(corona_data$country)

## Factoring the region variable

corona_data$region <- as.factor(corona_data$region)

## Factoring the transmission_classification variable

corona_data$transmission_classification <- as.factor(corona_data$transmission_classification)

## Changing tests_1M variable from character to numeric

corona_data$tests_1M <- as.numeric(corona_data$tests_1M)

#-----------------------------------------------------------------------------------
#Turn character variables to factor & numeric variables for use in analysis
#April 24, 2020 data
#-----------------------------------------------------------------------------------

## Factoring the country variable

corona_data_proj$Country <- as.factor(corona_data_proj$Country)

## Factoring the country variable

corona_data_proj$region <- as.factor(corona_data_proj$region)

## Factoring the transmission_classification variable

corona_data_proj$transmission_classification <- as.factor(corona_data_proj$transmission_classification)

#-----------------------------------------------------------------------------------
#Column name change - Situation report 113 code incase it was changed in R
#-----------------------------------------------------------------------------------

#names(corona_data)[1] <- "country"
#names(corona_data)[2] <- "region"
#names(corona_data)[3] <- "total_confirmed_cases"
#names(corona_data)[4] <- "total_confirmed_new_cases"
#names(corona_data)[5] <- "total_deaths"
#names(corona_data)[6] <- "total_new_deaths"
#names(corona_data)[7] <- "total_recovered"
#names(corona_data)[8] <- "active_cases"
#names(corona_data)[9] <- "critical_cases"
#names(corona_data)[10] <- "total_tests"
#names(corona_data)[11] <- "tests_1M"
#names(corona_data)[12] <- "transmission_classification"
#names(corona_data)[13] <- "days_since_last_reported_case"

#-----------------------------------------------------------------------------------
#Column name change - Situation report 95 code incase it was changed in R
#-----------------------------------------------------------------------------------
#names(corona_data_proj)[1] <- "Country"
#names(corona_data_proj)[2] <- "region"
#names(corona_data_proj)[3] <- "total_confirmed_cases"
#names(corona_data_proj)[4] <- "total_confirmed_new_cases"
#names(corona_data_proj)[5] <- "total_deaths"
#names(corona_data-proj)[6] <- "total_new_deaths"
#names(corona_data_proj)[7] <- "total_recovered"
#names(corona_data_proj)[8] <- "active_cases"
#names(corona_data_proj)[9] <- "critical_cases"
#names(corona_data_proj)[10] <- "total_tests"
#names(corona_data_proj)[11] <- "tests_1M"
#names(corona_data_proj)[12] <- "transmission_classification"
#names(corona_data_proj)[13] <- "days_since_last_reported_case"

#-----------------------------------------------------------------------------------
#Exploratory & Expository Visualizations
#-----------------------------------------------------------------------------------

par(mfrow = c(1,2))
boxplot(sqrt(tests_1M)~region, boxwex = 0.5, data = corona_data_proj)
boxplot(log(tests_1M)~region, boxwex = 0.5, data = corona_data)

mean(corona_data_proj$tests_1M)
#[1] 7939.906                 #mean of tests/million on 24 April, 2020
mean(corona_data$tests_1M)
[1] 19912.91                  #mean of tests/million increased on May 12, 2020

##########Finding the mean of tests/1M in different regions

#Situation Report (95)

mean_corona_data_proj <- aggregate(corona_data_proj$tests_1M, list(corona_data_proj$region), mean)

mean_table_corona_data_proj <- (mean_corona_data_proj)

plot(mean_corona_data_proj$Group.1, mean_corona_data_proj$x, ylab = "Mean of tests/1M", xlab = "Regions", main = "Mean of tests/1M in different regions/April 24, 2020", cex.axis = 0.45)

#Situation Report (113)

mean_corona_data <- aggregate(corona_data$tests_1M, list(corona_data$region), mean)
mean_table_corona_data <- (mean_corona_data)

plot(mean_corona_data$Group.1, mean_corona_data$x, ylab = "Mean of tests/1M", xlab = "Regions", main = "Mean of tests/1M in different regions/May 12, 2020", cex.axis = 0.45)

##########Plot active_cases against total_tests

#Situation report 95
plot(corona_data_proj$total_tests, corona_data_proj$active_cases, pch = 5, col = "red", xlab = "Total Tests", ylab = "Active Cases", main = "Total tests made confirming active cases")

#Situation report 113
points(corona_data$total_tests, corona_data$active_cases, pch = 7, col = "blue")

#Legend for graph
legend("center", c("Situation Report - April 24, 2020", "Situation Report - May 12, 2020"), col = c("red", "blue"), cex = 0.7, pch = c(5, 7))

##Installing and loading ggplo2

install.packages("ggplot2")
library("ggplot2")

########## Total tests made in each region

## Situation report - April 24 (total tests in all countries)

total_tests95 <- ggplot(data = corona_data_proj, aes(x = region, y = total_tests)) +
      geom_bar(stat = "identity", fill = "purple") + 
      labs(title = "Total Tests made in Regions - April Report",
           subtitle = "All Regions",
           x = "Regions", y = "Total Tests")
           
## Situation report - May 12

total_tests113 <- ggplot(data = corona_data, aes(x = region, y = total_tests)) +
      geom_bar(stat = "identity", fill = "green") + 
      labs(title = "Total Tests made in Regions - May Report",
           subtitle = "All Regions",
           x = "Regions", y = "Total Tests")

##########Plot tests_1M / total_conirmed_new_cases / region / transmission_ classification

##Situation report 95

tcnc_ct <- ggplot(data = corona_data_proj, aes(x = total_confirmed_new_cases, y = tests_1M, color = transmission_classification, group = transmission_classification)) +
       geom_point(size = 3, shape = 5) +
      labs(title = "Total confirmed new cases tests classification in different regions - April 24, 2020",
           subtitle = "All Regions",
           x = "Total confirmed new cases", y = "Tests/Million") + facet_wrap(~ region)
           
           
 ##Situation report 113
 
 tcnc_ct_113 <- ggplot(data = corona_data, aes(x = total_confirmed_new_cases, y = tests_1M, color = transmission_classification, group = transmission_classification)) +
       geom_point(size = 3, shape = 3) +
      labs(title = "Total confirmed new cases tests classification in different regions, May 12, 2020",
           subtitle = "All Regions",
           x = "Total confirmed new cases", y = "Tests/Million") + facet_wrap(~ region)          

##########Plot total_tests / active cases / region 

##Situation report 95

tt_ac_95 <- ggplot(data = corona_data_proj, aes(x = active_cases, y = total_tests, color = region)) +
      geom_point(size = 2, shape = 7) +
      labs(title = "Total tests vs active cases by region - April 24, 2020",
           subtitle = "All regions",
           x = "Active cases", y = "Total tests") + facet_wrap(~ region)

active_cases_quant95 <- quantile(corona_data_proj$active_cases)

##Situation report 113

tt_ac_113 <- ggplot(data = corona_data, aes(x = active_cases, y = total_tests, color = region)) +
      geom_point(size = 2, shape = 1) +
      labs(title = "Total tests vs active cases by region - May 12, 2020",
           subtitle = "All regions",
           x = "Active cases", y = "Total tests") + facet_wrap(~ region)


active_cases_quant113 <- quantile(corona_data$active_cases)

##########Plot total_tests / death cases / transmission / region

##Situation report 95

death_transmission <- ggplot(data = corona_data_proj, aes(x = total_deaths, y = tests_1M, color = transmission_classification, group = transmission_classification)) +
       geom_point(size = 5, shape = 1) +
      labs(title = "Total deaths vs transmission type",
           subtitle = "All regions - April 24",
           x = "Total deaths", y = "Tests/million") + facet_wrap(~ region)

##Situation report 113

death_transmission_113 <- ggplot(data = corona_data, aes(x = total_deaths, y = tests_1M, color = transmission_classification, group = transmission_classification)) +
       geom_point(size = 5, shape = 2) +
      labs(title = "Total deaths vs transmission type",
           subtitle = "All regions - May 12",
           x = "Total deaths", y = "Tests/million") + facet_wrap(~ region)


##########Plot total_tests / country / recovered / transmission

##Situation report 95

recovered_tests_tran <- ggplot(data = corona_data_proj, aes(x = total_recovered, y = tests_1M, color = transmission_classification, group = transmission_classification)) +
       geom_point(size = 5, shape = 9) +
      labs(title = "Total recovery cases vs transmission type",
           subtitle = "All Regions - April 24",
           x = "Total recoveries", y = "Tests/million") + facet_wrap(~ region)

##Situation report 113

recovered_tests_tran_113 <- ggplot(data = corona_data, aes(x = total_recovered, y = tests_1M, color = transmission_classification, group = transmission_classification)) +
       geom_point(size = 5, shape = 9) +
      labs(title = "Total recovery cases vs transmission type",
           subtitle = "All Regions - May 12",
           x = "Total recoveries", y = "Tests/million") + facet_wrap(~ region)


##########Plot days since last reported cases

##Situation report 95

days_reported95 <- ggplot(data = corona_data_proj, aes(x = active_cases, y = days_since_last_reported_case, color = region, group = region)) +
       geom_point(stat = "identity", fill = "orange") + 
       labs(title = "Days since last reported case in Regions - April Report",
            subtitle = "All Regions",
            x = "Active Cases", y = "Days since last reported case")

##Situation report 113

days_reported113 <- ggplot(data = corona_data, aes(x = active_cases, y = days_since_last_reported_case, color = region, group = region)) +
       geom_point(stat = "identity", fill = "orange") + 
       labs(title = "Days since last reported case in Regions - May Report",
            subtitle = "All Regions",
            x = "Active Cases", y = "Days since last reported case")


#-----------------------------------------------------------------------------------
#Plotting total_tests variable vs total confirmed cases in different regions
#-----------------------------------------------------------------------------------

##Disable scientific notation for graphing
options(scipen = 999)

########## Situation report 95

par(mfrow = c(2,1))

plot(log(corona_data_proj$total_tests[corona_data_proj$region == "Africa"]), corona_data_proj$total_confirmed_cases[corona_data_proj$region == "Africa"],
	main = "Total confirmed cases vs total tests in different regions/April 24 Report", 
	xlim = c(0,5000000), ylim = c(0,100000), xlab = "Total tests", 
	ylab = "Total confirmed cases", pch = 17, col = "red")

points(corona_data_proj$total_tests[corona_data_proj$region == "Americas"], corona_data_proj$total_confirmed_cases[corona_data_proj$region == "Americas"],
	pch = 3, col = "green")
	
points(corona_data_proj$total_tests[corona_data_proj$region == "Eastern_Mediterranean"], corona_data_proj$total_confirmed_cases[corona_data_proj$region == "Eastern_Mediterranean"],
	pch = 5, col = "blue")
	
points(corona_data_proj$total_tests[corona_data_proj$region == "Europe"], corona_data_proj$total_confirmed_cases[corona_data_proj$region == "Europe"],
	pch = 8, col = "orange")
	
points(corona_data_proj$total_tests[corona_data_proj$region == "South_East_Asia"], corona_data_proj$total_confirmed_cases[corona_data_proj$region == "South_East_Asia"],
	pch = 10, col = "black")
	
points(corona_data_proj$total_tests[corona_data_proj$region == "Western_Pacific"], corona_data_proj$total_confirmed_cases[corona_data_proj$region == "Western_Pacific"],
	pch = 16, col = "purple")
	
legend("topright", c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South East Asia", "Western Pacific"),
	col = c("red", "green", "blue", "orange", "black", "purple"), cex = 0.6, pch = c(17,3,5,8,10,16))


########## Situation report 113

plot(log(corona_data$total_tests[corona_data$region == "Africa"]), corona_data$total_confirmed_cases[corona_data$region == "Africa"],
	main = "Total confirmed cases vs total tests in different regions/May 12 Report", 
	xlim = c(0,2000000), ylim = c(0,79000), xlab = "Total tests", 
	ylab = "Total confirmed cases", pch = 17, col = "red")
	
points(corona_data$total_tests[corona_data$region == "Americas"], corona_data$total_confirmed_cases[corona_data$region == "Americas"],
	pch = 3, col = "green")
	
points(corona_data$total_tests[corona_data$region == "Eastern_Mediterranean"], corona_data$total_confirmed_cases[corona_data$region == "Eastern_Mediterranean"],
	pch = 5, col = "blue")
	
points(corona_data$total_tests[corona_data$region == "Europe"], corona_data$total_confirmed_cases[corona_data$region == "Europe"],
	pch = 8, col = "orange")
	
points(corona_data$total_tests[corona_data$region == "South_East_Asia"], corona_data$total_confirmed_cases[corona_data$region == "South_East_Asia"],
	pch = 10, col = "black")
	
points(corona_data$total_tests[corona_data$region == "Western_Pacific"], corona_data$total_confirmed_cases[corona_data$region == "Western_Pacific"],
	pch = 16, col = "purple")
	
## Adding a legend 

legend("topright", c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South East Asia", "Western Pacific"),
	col = c("red", "green", "blue", "orange", "black", "purple"), cex = 0.6, pch = c(17,3,5,8,10,16))


#-----------------------------------------------------------------------------------
#Pie chart creation (Average Total tests vs Region)
#-----------------------------------------------------------------------------------

#Install and load package called "plotrix"
install.packages("plotrix")
library(plotrix)

par(mfrow = c(2,1))

## Pie chart creation with average total test by region April 24 Report

########## Situation report 95

avg_total_tests_95 <-aggregate(total_tests~region, data=corona_data_proj, FUN = mean)
avg_total_tests_95

## Rounding of avg_total_deaths$total_deaths

avg_total_tests_95$total_tests
#[1] 7620.83 129695.02  91276.23 219927.43  83080.40 140875.58

r_total_tests_95 <- round(avg_total_tests_95$total_tests, digits = 0)

r_total_tests_95
#[1]   7621 129695  91276 219927  83080 140876

## Region label

avg_total_tests_tab_95 <- table(avg_total_tests_95$region)
avg_total_tests_tab_95

new_tests_pie_label_95 <- names(avg_total_tests_tab_95)

## Final label to use in pie
new_pie_region_lab_95 <- paste(new_tests_pie_label_95, "\n", r_total_tests_95)
new_pie_region_lab_95

## Using the label to form a 3D pie created for total tests in different regions

pie3D(avg_total_tests_95$total_tests,labels = new_pie_region_lab_95, explode = 0.1,
	labelcex = 0.50, labelrad = 0.5, main = "Average total tests in different regions, April 24 report")

########## Situation report 113

## Pie chart creation with average total test by region May 12 Report

## Average total tests made by region

avg_total_tests <-aggregate(total_tests~region, data=corona_data, FUN = mean)
avg_total_tests

## Rounding of avg_total_deaths$total_deaths

avg_total_tests$total_tests
#[1]  99652.61 317418.87 229327.67 466284.35 243978.90 199458.39

r_total_tests <- round(avg_total_tests$total_tests, digits = 0)

r_total_tests
#[1]  99653 317419 229328 466284 243979 199458

## Region label

avg_total_tests_tab <- table(avg_total_tests$region)
avg_total_tests_tab

new_tests_pie_label <- names(avg_total_tests_tab)

## Final label to use in pie
new_pie_region_lab <- paste(new_tests_pie_label, "\n", r_total_tests)
new_pie_region_lab


## Using the label to form a 3D pie created for total tests in different regions

pie3D(avg_total_tests$total_tests,labels = new_pie_region_lab, explode = 0.1,
	labelcex = 0.50, labelrad = 0.5, main = "Average total tests in different regions, May 12 report")

#-----------------------------------------------------------------------------------
#Boxplot Critical cases in different regions
#-----------------------------------------------------------------------------------

par(mfrow = c(2,1))

#Situation Report April 24 - 95

boxplot(log(corona_data_proj$critical_cases), main = "Critical Cases in different Regions April 24", ylab = "Number of critical cases")

boxplot(log(critical_cases)~region, data = corona_data_proj, col = c("red", "yellow", "cyan", "blue", "green", "purple"), 
main = "Critical cases in different regions (April 24)", ylab = "Number of critical cases", cex = 0.45, cex.axis = 0.7, horizontal = FALSE)

critical_cases_quant_95 <- quantile(corona_data_proj$critical_cases)

#      0%      25%      50%      75%     100% 
#    0.00     0.00     2.00    36.25 14932.00 

#Situation Report May 12 - 113
	
boxplot(log(corona_data$critical_cases), main = "Critical Cases in different Regions", ylab = "Number of critical cases", notch = TRUE)

boxplot(log(critical_cases)~region, data = corona_data, col = c("red", "yellow", "cyan", "blue", "green", "purple"), 
main = "Critical cases in different regions (May 12)", ylab = "Number of critical cases", cex = 0.45, cex.axis = 0.7, notch = FALSE, horizontal = FALSE)

## Find quartiles in boxplot

critical_cases_quant_113 <- quantile(corona_data$critical_cases)
#       0%        25%        50%        75%       100% 
#   1.0000     7.0000   175.5000   435.2093 16473.0000

#-----------------------------------------------------------------------------------
#Barplot - Transmission case types 
#-----------------------------------------------------------------------------------

par(mfrow = c(2,1))
##Finding count for the transmission class variable - April 24 report

transmission_classification_tab <- table(corona_data$transmission_classification)
transmission_classification_tab

## Creating a bar plot for transmission case types

barplot(transmission_classification_tab , ylab = "Count", col = c("green", "yellow", "orange", "purple"), xlab = "Transmission type", ylim =c(0,100), main = "Case Transmission type - April 24 report")

## Get numbers of transmission cases to place in legend
table(corona_data$transmission_classification_tab)

legend("topright", c("85", "56", "20", "49"), col = c("green", "yellow", "orange", "purple"), cex = 0.70, lwd=2)

##Finding count for the transmission class variable - May 12 report

transmission_classification_tab95 <- table(corona_data_proj$transmission_classification)
transmission_classification_tab95

## Creating a bar plot for transmission case types

barplot(transmission_classification_tab95 , ylab = "Count", col = c("green", "yellow", "orange", "purple"), xlab = "Transmission type", ylim =c(0,100), main = "Case Transmission type - May 12 report")

##Forming a box and grid over the barplot
box()
grid()

#-----------------------------------------------------------------------------------
#Supervised Learning - Multiple Linear Regression
#-----------------------------------------------------------------------------------

##Using May 12 Report (latest dataset)

# Split data set into training set and test set
n <- nrow(corona_data)  # Number of observations = 210
ntrain <- round(n*0.6)    # 60% for training set
set.seed(123)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

train_corona_data <- corona_data[tindex,]  # Create training set - 126 objects
test_corona_data <- corona_data[-tindex,]  # Create test set - 84 objects

# Exploratory data analysis - # check for trend or no trend
plot(train_corona_data$tests_1M, train_corona_data$total_confirmed_new_cases) #No Trend
plot(train_corona_data$tests_1M, train_corona_data$active_cases) #No Trend
plot(train_corona_data$tests_1M, train_corona_data$critical_cases) #No Trend
plot(train_corona_data$total_tests, train_corona_data$total_confirmed_new_cases) #Trend
plot(train_corona_data$total_tests, train_corona_data$active_cases) #Trend
plot(train_corona_data$total_tests, train_corona_data$critical_cases) #Trend


plot(train_corona_data$total_tests, train_corona_data$active_cases, xlab = "Total Tests", ylab = "Active Cases", main = "Total tests vs. Active Cases", col = "blue")

# Train linear model to predict 

lm2 <- lm(total_tests~ total_confirmed_cases + total_confirmed_new_cases + total_deaths + total_new_deaths + total_recovered + active_cases + critical_cases + tests_1M, data=train_corona_data)

summary(lm2)

#Call:
#lm(formula = total_tests ~ total_confirmed_cases + total_confirmed_new_cases + 
#    total_deaths + total_new_deaths + total_recovered + active_cases + 
#    critical_cases + tests_1M, data = train_corona_data)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-745125 -115306   -5194   72754  919461 

#Coefficients:
                            Estimate Std. Error t value       Pr(>|t|)    
#(Intercept)               96690.1824 28790.7084   3.358       0.001059 ** 
#total_confirmed_cases       -10.1942     3.4447  -2.959       0.003732 ** 
#total_confirmed_new_cases   443.6722    63.7154   6.963 0.000000000207 ***
#total_deaths                 36.5803    11.7361   3.117       0.002300 ** 
#total_new_deaths          -3167.4028  1365.2456  -2.320       0.022073 *  
#total_recovered              24.7037     3.5620   6.935 0.000000000238 ***
#active_cases                 11.5160     3.1759   3.626       0.000428 ***
#critical_cases             -312.6989    49.4485  -6.324 0.000000004833 ***
#tests_1M                      1.9597     0.8009   2.447       0.015904 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 254100 on 117 degrees of freedom
#Multiple R-squared:  0.9498,	Adjusted R-squared:  0.9464 
#F-statistic: 276.8 on 8 and 117 DF,  p-value: < 0.00000000000000022

#y = 96690.1824 - 10.1942(total_confirmed_cases) + 443.6722(total_confirmed_new_cases) + 36.5803(total_deaths) - 3167.4028(total_new_deaths) + 24.7037(total_recovered) + 11.5160(active_cases) - 312.6989(ctitical_cases) + 1.9597(tests_1M)

set.seed(123)

#total_tests and total_recovered prediction
coef(lm2)[1] + coef(lm2)[6]*1000                #121393
#total_tests and active cases prediction
coef(lm2)[1] + coef(lm2)[7]*500                 #102448
coef(lm2)[1] + coef(lm2)[7]*1000                #1082061
#total_tests and critical_cases predictions
coef(lm2)[1] + coef(lm2)[8]*1000.               #-216008


# for every active case in all regions there are 96,701 tests made

# The predicted vs. residual plot confirm a good distribution
plot(lm2$fitted, lm2$residuals)

# Using the trained model to predict the output of test set
predict2 <- predict(lm2, newdata=test_corona_data) 

# Checking correlation between predicted and actual test set response values 
cor(predict2, test_corona_data$total_tests)

[1] 0.7122038      # Moderate correlation

#Checking if residuals are normally distributed

rs2 <- residuals(lm2)
qqnorm(rs2)
qqline(rs2, col = "red")

par(mfrow = c(2,2))
plot(lm2)

#Measuring the model performance using RMSE (root mean squared error)
#We are going to predict the number of tests, the RMSE will indicate how
#far off the predictions will be in terms of cases

#install Metrics package
install.packages("Metrics")

#Loading Metrics package

# Calculate RMSE for training set
rmse_train <- rmse(predict(lm2),train_corona_data$total_tests)
rmse_train

#[1] 244903.8 
(244903.8/9935720)*100
#[1] 2.464882            reported RMSE values in training set

#So for total tests an RMSE of 244903 would be a good determination of cases

# Calculate RMSE for test set
rmse_test <- rmse(predict(lm2, newdata=test_corona_data), 
                  test_corona_data$total_tests)
rmse_test

#[1] 235034.2 
(235034.2/9935720)*100
#[1] 2.365548            reported RMSE values in testing set


#-----------------------------------------------------------------------------------
#Clustering using Eucledian, Dendogram - Viewing options and understanding data
#-----------------------------------------------------------------------------------

#View of the number of tests made and active cases by country
plot(active_cases ~ total_tests, xlab = "Total Tests", ylab = "Active Cases", corona_data)
with(corona_data, text(active_cases ~ total_tests, labels=country, pos = 4, cex = 0.6))

#View of the number of tests made and active cases by region
plot(active_cases ~ total_tests, xlab = "Total Tests", ylab = "Active Cases", corona_data)
with(corona_data, text(active_cases ~ total_tests, labels=region, pos = 4, cex = 0.5))

#Normalization of data
#remove column 1, 2, 12 (factor variables)

remove_factor_columns <- corona_data[, -c(1,1,2,2,12,12)]
summary(remove_factor_columns)

#Preventing domination of low data points to high data points and vice versa (we normalize)
#Average for each variable becomes 0 and standard deviation becomes 1

#calculate mean for all variables in columns
m <- apply(remove_factor_columns, 2, mean)

#calculate standard deviations for all variables in columns
s <- apply(remove_factor_columns, 2, sd)

#Calculate normalized dataset remove_factor_columns created

remove_factor_columns <- scale(remove_factor_columns, m, s)

#Calculating Euclidean distance
distance <- dist(remove_factor_columns)

#Get a compact picture of eucledian distance, how far each one is compared to the list

#Cluster dendrogram with complete linkage
hc.c <- hclust(distance)
plot(hc.c, labels = corona_data$country)

#Cluster dendrogram with average
hc.a <- hclust(distance, method = "average")
plot(hc.a, labels = corona_data$country, hang = -1)

#Cluster membership using 3 clusters
member.c <- cutree(hc.c, 3)

#Cluster membership based on average linkage
member.a <- cutree(hc.a, 3)

table(member.c, member.a)
plot(member.c, member.a)

#        member.a
#member.c   1   2   3
#       1   9   1   0
#       2 199   0   0
#       3   0   0   1


#Explanation: Using average linkage method: 
# 9 + 199 countries belong in cluster 1 
# 1 country belongs to cluster 2
# 1 country belongs in cluster 3

#Explanation: Using complete linkage method:

# 9 + 1 countries in cluster 1
# 199 countries belong to cluster 2
# 1 country belong to cluster 3 

#If we look at both average and complete linkage
# Both methods placed 9 countries in cluster 1 
# Both methods placed 1 country in cluster 3

#-----------------------------------------------------------------------------------
#Kmeans clustering (May 12, dataset)
#-----------------------------------------------------------------------------------
#Normalization of data
#remove column 1, 2, 12 (factor variables)

remove_factor_columns <- corona_data[, -c(1,1,2,2,12,12)]
summary(remove_factor_columns)

#Preventing domination of low data points to high data points and vice versa (we normalize)
#Average for each variable becomes 0 and standard deviation becomes 1

#calculate mean for all variables in columns
m <- apply(remove_factor_columns, 2, mean)

#calculate standard deviations for all variables in columns
s <- apply(remove_factor_columns, 2, sd)

#Calculate normalized dataset remove_factor_columns created

remove_factor_columns <- scale(remove_factor_columns, m, s)

set.seed(855)
kc <- kmeans(remove_factor_columns, centers = 3, iter.max = 10)

#K-means clustering with 3 clusters of sizes 1, 9, 200 (countries)

#Cluster means:
#  total_confirmed_cases total_confirmed_new_cases total_deaths total_new_deaths #total_recovered
#1            13.2292346                12.6847276   11.3805229       13.4047805       #9.8038163
#2             1.6664970                 1.4008130    2.2147359        1.2272729       #2.7664225
#3            -0.1411385                -0.1264602   -0.1565657       -0.1222512      #-0.1735081
#  active_cases critical_cases total_tests    tests_1M days_since_last_reported_case
#1   13.7438392     11.6591821  11.0329811  0.35953015                   -0.44283943
#2    0.9561076      1.8940393   2.1973941  0.27321850                   -0.44283943
#3   -0.1117440     -0.1435277  -0.1540476 -0.01409248                    0.02214197

Within cluster sum of squares by cluster:
[1]   0.0000 146.3867 467.2590
 (between_SS / total_SS =  70.6 %)

#Variability in cluster 1 is lower = 0.0000 (very close distance - USA)
#Variability in cluster 2 is medium = 146.3867 and countries are closer in terms of distance
#Variability in cluster 3 is 467.2590 and country members are not close in terms of distance

# Cluster - 1 (USA)

# Cluster - 2 (Iran, Peru, Brazil, France, UK, Italy, Spain, Germany, Russia) A mix of Americas, Eastern Mediterranean and European countries.).

# Cluster - 3 (All other countries). 

kc$cluster # (countries placed in clusters)

#[1] 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[46] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[91] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[136] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[181] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

kc$centers # (averages)

##### Review and print centers component of kmeans object

kc$centers

#total_confirmed_cases total_confirmed_new_cases total_deaths total_new_deaths #total_recovered
#1            -0.1411385                -0.1264602   -0.1565657       -0.1222512      #-0.1735081
#2            13.2292346                12.6847276   11.3805229       13.4047805       #9.8038163
#3             1.6664970                 1.4008130    2.2147359        1.2272729       #2.7664225
#  active_cases critical_cases total_tests    tests_1M days_since_last_reported_case
#1   -0.1117440     -0.1435277  -0.1540476 -0.01409248                    0.02214197
#2   13.7438392     11.6591821  11.0329811  0.35953015                   -0.44283943
#3    0.9561076      1.8940393   2.1973941  0.27321850                   -0.44283943


#5 Plotting results of clusters of data points and their centers

plot(active_cases ~ total_tests, corona_data, col = kc$cluster, xlab = "Total Tests", ylab = "Active Cases", main = "Kmeans Country Clusters", pch = 16)

with(corona_data, col = kc$cluster, text(active_cases ~ total_tests, labels=country, pos = 4, cex = 0.5, pch = 16)) 

# Centroid of each cluster
points(kc$centers,col=1:10, pch=9,cex=3,lwd=3)

#clustering is good when between cluster distance is high and within cluster 
#distance is low

plot(remove_factor_columns,col = kc$cluster, xlab = "Total Tests", ylab = "Active Cases")
points(kc$centers,col=1:10, pch=9,cex=3,lwd=3)

###Shapes of clusters for better view and seperation

install.packages("cluster")
library(cluster)

clusplot(corona_data, kc$cluster, main = "Representation of Clusters", shade = TRUE, labels = 2, lines = 0)


#-----------------------------------------------------------------------------------
#Notes and experimental data to help with project work
#-----------------------------------------------------------------------------------
# Removal of total_tests1M and total_new_deaths variables from regression (least significant)

#lm3 <- lm(total_tests~ total_confirmed_cases + total_confirmed_new_cases + #total_deaths + total_recovered + active_cases + critical_cases, #data=train_corona_data)

# The predicted vs. residual plot confirm a good distribution
#plot(lm3$fitted, lm3$residuals)

# Using the trained model to predict the output of test set
#predict3 <- predict(lm3, newdata=test_corona_data)

# Checking correlation between predicted and actual test set response values 
#cor(predict3, test_corona_data$total_tests)

#[1] 0.5835482

# addition of days since last reported case

#lm4 <- lm(total_tests~ total_confirmed_cases + total_confirmed_new_cases + #total_deaths + total_new_deaths + total_recovered + active_cases + critical_cases #+ tests_1M + days_since_last_reported_case, data=train_corona_data)

#plot(lm4$fitted, lm4$residuals)

# Using the trained model to predict the output of test set
#predict4 <- predict(lm4, newdata=test_corona_data)

#cor(predict4, test_corona_data$total_tests)

#[1] 0.7008562

#-----------------------------------------------------------------------------------
#Make copy of corona_data for testing code on it and preventing changes to corona_data dataset

#corona_data_testing <- corona_data

#breaks = seq(0, 3000000, 500000)

#Choosing the best plots to work with

#plot(corona_data[, c(1,3,4,5,6,7,8,9,10,11,13)],)