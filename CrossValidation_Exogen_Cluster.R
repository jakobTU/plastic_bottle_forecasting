################################################################################
### PERFORM CROSS-VALIDATION ON A HORIZON OF 20 WITH EXTERNAL COVARIATES #######
################################################################################

### Perform the analysis described in Section 4.4 of the paper

# Load libraries
library(forecast)
library(lubridate)

library(caretForecast)
library(caret)
library(xgboost)
library(ranger)
library(matrixStats)


# Load data
load("Verdichtet.RData")
load("best_k.RData")
load("exo_cov.RData")

data <- data_V_MZ

### Time series cross-validation functions for the different models

# Dynamic harmonic regression (DHR)
ts_CV_DHR_exo <- function(ts, initialTrain, fcast_win, lag, best_k, exo, data){
  
  fc_function <- function(x, h, exo, data, best_k){
    n <- length(x)
    tended  <- ts(c(x, rep(NA, h)), start = start(x), frequency = frequency(x))
    
    # Incorporate exogenous covariates
    exo_lagged <- exo
    
    exo_lagged$precipation <- c(rep(NA, lag), exo$precipation[1:(nrow(exo) - lag)])
    exo_lagged$temperature <- c(rep(NA, lag), exo$temperature[1:(nrow(exo) - lag)])
    exo_lagged$sunshine    <- c(rep(NA, lag), exo$sunshine[1:(nrow(exo) - lag)])
    
    # Add Fourier terms
    xreg <- cbind(fourier(tended, K = best_k), 
                  exo_lagged[exo_lagged$Datum %in% head(sort(data$ZAEHLDATUM), n+h),-1])
    
    # Add dummy variables
    dummys <- model.matrix(~wday + season, data = xreg)[,-1]
    dummys <- subset(dummys, select = -c(wday_sat, wday_sun))
    
    xreg <- cbind(subset(xreg, select = -c(wday, season)), dummys)
    
    # Fit model and calculate forecasts
    model <- auto.arima(x, xreg = as.matrix(xreg)[1:n,], seasonal = FALSE)
    
    if (h==1) {
      return(forecast(model, xreg = t(as.matrix(xreg)[(n+1):(n+h),])))
    }
    else {
      return(forecast(model, xreg = as.matrix(xreg)[(n+1):(n+h),]))
    }
    
  }
  
  residuals <- tsCV(ts, fc_function, 
                    h = fcast_win, exo = exo, data = data, best_k = best_k, 
                    initial = initialTrain)
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}

# Random Forest on detrended Data
ts_CV_rf_detrended_exo <- function(ts, initialTrain, fcast_win, lag, best_k, exo, data){
  
  fc_function <- function(x, h, exo, data, best_k){
    
    # Detrending with LM
    LMod <- lm(x ~ time(x))
    x_detrend <- ts(as.numeric(LMod$residuals), start = start(x), 
                    frequency = frequency(x))
    
    n <- length(x_detrend)
    tended  <- ts(c(x_detrend, rep(NA, h)), start = start(x_detrend), frequency = frequency(x_detrend))
    
    # Incorporate exogenous covariates
    exo_lagged <- exo
    
    exo_lagged$precipation <- c(rep(NA, lag), exo$precipation[1:(nrow(exo) - lag)])
    exo_lagged$temperature <- c(rep(NA, lag), exo$temperature[1:(nrow(exo) - lag)])
    exo_lagged$sunshine    <- c(rep(NA, lag), exo$sunshine[1:(nrow(exo) - lag)])
    
    # Restrict to observed dates
    xreg <- exo_lagged[exo_lagged$Datum %in% head(sort(data$ZAEHLDATUM), n+h),-1]
    
    # Dummy variables for weekday and season
    dummys <- model.matrix(~wday + season, data = xreg)[,-1]
    dummys <- subset(dummys, select = -c(wday_sat, wday_sun))
    
    xreg <- cbind(subset(xreg, select = -c(wday, season)), dummys)
    
    # Fit model
    model <- ARml(x_detrend, caret_method = "ranger", max_lag = 20, cv = TRUE, 
                  cv_horizon = 4, K = best_k, xreg = xreg[1:n,],
                  tune_grid = expand.grid(mtry = c(4,7,10), splitrule = "variance", min.node.size = c(1,5,10)),
                  num.threads = 1)
    
    # Generate forecasts and add the trend back
    if (h==1) {
      fc_detrend <- forecast(model, xreg = t(as.matrix(xreg)[(n+1):(n+h),]))$mean
    }
    else {
      fc_detrend <- forecast(model, xreg = as.matrix(xreg)[(n+1):(n+h),])$mean
    }
    
    FC <- fc_detrend + LMod$coefficients[1] + time(fc_detrend) * LMod$coefficients[2]
    return(list(mean = FC))
  }
  
  ## Prevent caret from using parallelism
  if ("doParallel" %in% loadedNamespaces()) {
    doParallel::stopImplicitCluster()
  }
  
  if ("foreach" %in% loadedNamespaces()) {
    foreach::registerDoSEQ()
  }
  
  residuals <- tsCV(ts, fc_function, 
                    h = fcast_win, exo = exo, data = data, best_k = best_k, 
                    initial = initialTrain)
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}

# XGBoost on detrended data
ts_CV_xgb_detrended_exo <- function(ts, initialTrain, fcast_win, lag, best_k, exo, data){
  
  fc_function <- function(x, h, exo, data, best_k){
    
    # Detrending with LM
    LMod <- lm(x ~ time(x))
    x_detrend <- ts(as.numeric(LMod$residuals), start = start(x), 
                    frequency = frequency(x))
    
    n <- length(x_detrend)
    tended  <- ts(c(x_detrend, rep(NA, h)), start = start(x_detrend), frequency = frequency(x_detrend))
    
    # Incorporate exogenous covariates
    exo_lagged <- exo
    
    exo_lagged$precipation <- c(rep(NA, lag), exo$precipation[1:(nrow(exo) - lag)])
    exo_lagged$temperature <- c(rep(NA, lag), exo$temperature[1:(nrow(exo) - lag)])
    exo_lagged$sunshine    <- c(rep(NA, lag), exo$sunshine[1:(nrow(exo) - lag)])
    
    # Restrict to observed days
    xreg <- exo_lagged[exo_lagged$Datum %in% head(sort(data$ZAEHLDATUM), n+h),-1]
    
    # Dummy variables for weekday and season
    dummys <- model.matrix(~wday + season, data = xreg)[,-1]
    dummys <- subset(dummys, select = -c(wday_sat, wday_sun))
    
    xreg <- cbind(subset(xreg, select = -c(wday, season)), dummys)
    
    # Fit model
    model <- ARml(x_detrend, caret_method = "xgbTree", max_lag = 20, cv = TRUE,
                  cv_horizon = 4, K = best_k, xreg = xreg[1:n,],
                  tune_grid = expand.grid(nrounds = c(100, 1000), max_depth=c(1,5), 
                                          eta=c(0.01,0.3), gamma=0,
                                          colsample_bytree = 1, min_child_weight = 1,
                                          subsample = c(0.5, 0.8)),
                  nthread = 1)
    
    # Generate forecasts and add the trend back
    if (h==1) {
      fc_detrend <- forecast(model, xreg = t(as.matrix(xreg)[(n+1):(n+h),]))$mean
    }
    else {
      fc_detrend <- forecast(model, xreg = as.matrix(xreg)[(n+1):(n+h),])$mean
    }
    
    FC <- fc_detrend + LMod$coefficients[1] + time(fc_detrend) * LMod$coefficients[2]
    return(list(mean = FC))
  }
  
  # Prevent caret from using parallelism
  if ("doParallel" %in% loadedNamespaces()) {
    doParallel::stopImplicitCluster()
  }
  
  if ("foreach" %in% loadedNamespaces()) {
    foreach::registerDoSEQ()
  }
  
  residuals <- tsCV(ts, fc_function, 
                    h = fcast_win, exo = exo, data = data, best_k = best_k, 
                    initial = initialTrain)
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}

# Function for performing the cross-validation

cross_validate <- function(targetVariable, material, model_function, 
                          fcast_win, lag, initialTrain, best_k, 
                          data, exo){
  
  data_material <- data[data$MATERIALART == material, c("ZAEHLDATUM", targetVariable)]
  ts_material <- ts(data_material[order(data_material$ZAEHLDATUM), targetVariable], frequency = 251)
  
  CV_res <- model_function(ts_material, initialTrain = initialTrain, 
                          fcast_win = fcast_win,
                          lag = lag,
                          best_k = best_k,
                          exo = exo, data = data_material)
  return(CV_res)
}

# List of the different forecasting functions
model_functions <- list(ts_CV_DHR_exo = ts_CV_DHR_exo,
                        ts_CV_rf_detrended_exo = ts_CV_rf_detrended_exo,
                        ts_CV_xgb_detrended_exo = ts_CV_xgb_detrended_exo)

# The different materials
materials <- levels(data$MATERIALART)

# The used lag of the external covariates
lag <- 20

# The different forecast windows
fcast_wins <- 1:20


# All combinations
settings <- expand.grid(material = materials, 
                        model_function = names(model_functions),
                        fcast_win = fcast_wins,
                        lag = lag)

### Perform the cross-validation

Sys.info()
i <- as.integer(Sys.getenv("PBS_ARRAYID"))

print(settings[i,]$model_function)
print(settings[i,]$material)
print(settings[i,]$lag)

set.seed(1512)

residuals <- cross_validate(targetVariable = "ANZAHL_GEBINDE_NMZ",
                           material = settings[i,]$material, 
                           model_function = model_functions[[settings[i,]$model_function]], 
                           fcast_win = settings[i,]$fcast_win,
                           lag = settings[i,]$lag,
                           initialTrain = 502,
                           best_k = best_k[settings[i,]$material],
                           data = data,
                           exo = ExogenCovariates)

save(residuals, file = paste0("cv_results/residuals_x/res_", 
                              gsub("ts_CV_", "", settings[i,]$model_function), 
                              "_", settings[i,]$material,"_", 
                              settings[i,]$fcast_win, ".RData"))

Sys.info()
gc()