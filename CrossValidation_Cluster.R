################################################################################
### PERFORM CROSS-VALIDATION ON A HORIZON OF 20 ################################
################################################################################

### Perform the analysis described in Section 4.3 of the paper

# Load libraries 
library(forecast)
library(lubridate)

library(caretForecast)
library(caret)
library(xgboost)
library(ranger)
library(matrixStats)


# Load data
load('Verdichtet.RData')
load('best_k.RData')

data <- data_V_MZ


### Time series cross-validation functions for the different models

## always: only return the window for time series cross-validation
## The first and smallest training set contains the years 2020 and 2021
## The last and largest training set is the one that still has all h predictions
## within the training data set

# Naive forecast
ts_CV_naive <- function(ts, fcast_win, initialTrain, best_k){
  residuals <- tsCV(ts, function(x,h){forecast(naive(x, h = h))}, 
                    h = fcast_win, initial = initialTrain)
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}


# Random Forest on detrended data
ts_CV_rf_detrend <- function(ts, initialTrain, fcast_win, best_k){
  rf_detrend <- function(x,h){
    # Detrending with LM
    LMod <- lm(x ~ time(x))
    x_detrend <- ts(as.numeric(LMod$residuals), start = start(x), 
                    frequency = frequency(x))
    
    # Train model
    model <- ARml(x_detrend, caret_method = 'ranger', max_lag = 20, cv = TRUE, 
                  cv_horizon = 4, K = best_k,
                  tune_grid = expand.grid(mtry = c(4,7,10), splitrule = 'variance', min.node.size = c(1,5,10)),
                  num.threads = 1)
    
    # Generate forecasts and add the trend back
    fc_detrend <- forecast(model, h=h)$mean
    FC <- fc_detrend + LMod$coefficients[1] + time(fc_detrend) * LMod$coefficients[2]
    return(list(mean = FC))
  }
  
  # Prevent caret from using parallelism
  if ('doParallel' %in% loadedNamespaces()) {
    doParallel::stopImplicitCluster()
  }
  
  if ('foreach' %in% loadedNamespaces()) {
    foreach::registerDoSEQ()
  }
  
  residuals <- tsCV(ts, rf_detrend, h = fcast_win, initial = initialTrain)
  
  # Only return the window for time series cross-validation
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}


# TBATS
ts_CV_TBATS <- function(ts, initialTrain, fcast_win, best_k){
  residuals <- tsCV(ts, function(x,h){forecast(tbats(x, seasonal.periods = 251), h = h)}, 
                    h = fcast_win, initial = initialTrain)
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}


# ARIMA
ts_CV_ARIMA <- function(ts, initialTrain, fcast_win, best_k){
  residuals <- tsCV(ts, function(x,h){forecast(auto.arima(x), h = h)}, 
                    h = fcast_win, initial = initialTrain)
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}

# Dynamic harmonic regression (DHR)
ts_CV_DHR <- function(ts, initialTrain, fcast_win, best_k){
  residuals <- tsCV(ts, function(x,h){forecast(auto.arima(x, xreg = fourier(x, K = best_k), seasonal = FALSE), 
                                               h = h, xreg = fourier(x, K = best_k, h = h))}, 
                    h = fcast_win, initial = initialTrain)
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}

# XGBoost on detrended Data
ts_CV_xgb_detrend <- function(ts, initialTrain, fcast_win, best_k){
  xgb_detrend <- function(x,h){
    # Detrending with LM
    LMod <- lm(x ~ time(x))
    x_detrend <- ts(as.numeric(LMod$residuals), start = start(x), 
                    frequency = frequency(x))
    
    # Train model
    model <- ARml(x_detrend, caret_method = 'xgbTree', max_lag = 20, cv = TRUE,
                  cv_horizon = 4, K = best_k,
                  tune_grid = expand.grid(nrounds = c(100, 1000), max_depth=c(1,5), 
                                          eta=c(0.01,0.3), gamma=0,
                                          colsample_bytree = 1, min_child_weight = 1,
                                          subsample = c(0.5, 0.8)),
                  nthread = 1)
    
    # Generate Forecasts and add the trend back
    fc_detrend <- forecast(model, h=h)$mean
    FC <- fc_detrend + LMod$coefficients[1] + time(fc_detrend) * LMod$coefficients[2]
    return(list(mean = FC))
  }
  
  # Prevent caret from using parallelism
  if ('doParallel' %in% loadedNamespaces()) {
    doParallel::stopImplicitCluster()
  }
  
  if ('foreach' %in% loadedNamespaces()) {
    foreach::registerDoSEQ()
  }
  
  residuals <- tsCV(ts, xgb_detrend, h = fcast_win, initial = initialTrain)
  
  # Only return the window for time series cross-validation
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}

### Function for performing the cross-validation

cross_validate <- function(targetVariable, material, model_function, 
                          fcast_win, initialTrain, best_k){
  
  data_material <- data[data$MATERIALART == material, c('ZAEHLDATUM', targetVariable)]
  ts_material <- ts(data_material[order(data_material$ZAEHLDATUM), targetVariable], frequency = 251)
  
  CV_res <- model_function(ts_material, initialTrain = initialTrain, 
                          fcast_win = fcast_win, best_k = best_k)
  return(CV_res)
}


# List of the different forecasting functions
model_functions <- list(ts_CV_naive = ts_CV_naive, 
                        ts_CV_TBATS = ts_CV_TBATS, 
                        ts_CV_ARIMA = ts_CV_ARIMA,
                        ts_CV_rf_detrend = ts_CV_rf_detrend,
                        ts_CV_xgb_detrend = ts_CV_xgb_detrend,
                        ts_CV_DHR = ts_CV_DHR)

# The different materials
materials <- levels(data$MATERIALART)

# The different forecast windows
fcast_wins <- 1:20


## All combinations
settings <- expand.grid(material = materials, 
                        model_function = names(model_functions), 
                        fcast_win = fcast_wins)

### Perform the cross-validation

Sys.info()
i <- as.integer(Sys.getenv('PBS_ARRAYID'))


print(settings[i,]$model_function)
print(settings[i,]$material)
print(settings[i,]$fcast_win)


set.seed(1512)

residuals <- cross_validate('ANZAHL_GEBINDE_NMZ',
                           material = settings[i,]$material, 
                           model_function = model_functions[[settings[i,]$model_function]], 
                           fcast_win = settings[i,]$fcast_win,
                           initialTrain = 502,
                           best_k = best_k[settings[i,]$material])

save(residuals, file = paste0('cv_results/residuals/res_', 
                              gsub('ts_CV_', '', settings[i,]$model_function), 
                              '_', settings[i,]$material, '_',
                              settings[i,]$fcast_win, '.RData'))

Sys.info()
gc()