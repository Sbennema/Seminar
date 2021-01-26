## Github Seminar
rm(list=ls())

library(bsts)
library(hms)
library(stringr)

# 
#     Script
#
#

path_data <- "C:/Users/Steef/Documents/Uni/Seminar BA&QM/R code/"
path_functions <- "C:/Users/Steef/Documents/Uni/Seminar BA&QM/R code/Seminar/"

source(paste(path_functions,"Functions.R", sep = ""))       #Load in the functions

#Read in the data
#Make the visit_index when there are ads equal to missing for baseline

data_be_app <- read.csv(paste(path_data,file="be_app_adj.csv", sep = ""), header = T)
data_be_app_bayes <- data_be_app
data_be_app_bayes$visits_index[data_be_app_bayes$ads==1] <- NaN

data_be_web <- read.csv(paste(path_data,file="be_web_adj.csv", sep = ""), header = T)
data_be_web_bayes <- data_be_web
data_be_app_bayes$visits_index[data_be_app_bayes$ads==1] <- NaN

data_nl_app <- read.csv(paste(path_data,file="nl_app_adj.csv", sep = ""), header = T)
data_nl_app_bayes <- data_nl_app
data_nl_app_bayes$visits_index[data_nl_app_bayes$ads==1] <- NaN

data_nl_web <- read.csv(paste(path_data,file="nl_web_adj.csv", sep = ""), header = T)
data_nl_web_bayes <- data_nl_web
data_nl_web_bayes$visits_index[data_nl_web_bayes$ads==1] <- NaN


#Create the regressors

Regressors_be_app <- XVarsData(data_be_app_bayes)
Regressors_be_web <- XVarsData(data_be_web_bayes)
Regressors_nl_app <- XVarsData(data_nl_app_bayes)
Regressors_nl_web <- XVarsData(data_nl_web_bayes)

#Find the number of days
numberOfDays <- (dim(data_nl_web)[1]) / (dim(Regressors_nl_web)[1])

#Duplicate the regressors such that every observation in the data has these variables
Regressors_be_app <- Regressors_be_app[rep(rownames(Regressors_be_app),numberOfDays),]
rownames(Regressors_be_app) <- 1:NROW(Regressors_be_app)
Regressors_be_web <- Regressors_be_web[rep(rownames(Regressors_be_web),numberOfDays),]
rownames(Regressors_be_web) <- 1:NROW(Regressors_be_web)
Regressors_nl_app <- Regressors_nl_app[rep(rownames(Regressors_nl_app),numberOfDays),] 
rownames(Regressors_nl_app) <- 1:NROW(Regressors_nl_app)
Regressors_nl_web <- Regressors_nl_web[rep(rownames(Regressors_nl_web),numberOfDays),] 
rownames(Regressors_nl_web) <- 1:NROW(Regressors_nl_web)

#Find the timestamps when ads happen
ad_locations_be <- Ad_locations(data_be_app)
ad_locations_nl <- Ad_locations(data_nl_app)

#Useful considerations
day <- 24*60
week <- day*6
month <- week*4

Numb_ads_be <- dim(ad_locations_be)[1]
Numb_ads_nl <- dim(ad_locations_nl)[1]


#
#         FIRST BAYESIAN BASELINE ESTIMATION
#

end_train_nl <- ad_locations_nl$start[1] - 1
start_train_nl <- 1
Regressors_model_nl_web <- as.matrix(Regressors_nl_web[start_train_nl:end_train_nl,2:5])

y_nl_web <- data_nl_web_bayes$visits_index[start_train_nl:end_train_nl]
#ss_nl <- AddLocalLinearTrend(list(),y_nl)
ss_nl <- AddAr(list(),y_nl_web)
ss_nl <- AddSeasonal(ss_nl,y_nl_web,nseasons=7,season.duration=day)
model <- bsts(y_nl_web~Regressors_model_nl_web,state.specification = ss_nl,niter=300)


Regressors_pred_nl_web <- as.matrix(Regressors_nl_web[ad_locations_nl$start[1]:ad_locations_nl$end[1],2:5])
f_horizon <- ad_locations_nl$end[1] - end_train_nl
pred <- predict(model,horizon = f_horizon,newdata = Regressors_pred_nl_web)
lift <- data_nl_web$visits_index[ad_locations_nl$start[1]:ad_locations_nl$end[1]] - pred$mean 
lift_tot <- sum(lift)
print(lift_tot)


#ss <- AddLocalLinearTrend(list(), y)
#ss <- AddSeasonal(ss, y)
#model <- bsts(y,state.specification=ss,niter=300)