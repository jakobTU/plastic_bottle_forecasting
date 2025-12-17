################################################################################
### PERFORM CROSS-VALIDATION FOR LSTM ON A HORIZON OF 20 #######################
################################################################################

### Perform the cross-correlation analysis for the LSTM described in Section 4.3
### of the paper

# Load libraries 
library(forecast)
library(caretForecast)
library(caret)
library(xgboost)
library(keras)
library(tensorflow)
library(ranger)
library(matrixStats)

# Load data
load('Verdichtet.RData')
load('best_k.RData')

data <- data_V_MZ

# LSTM
forecast_LSTM <- function(x, h, BS=1, lag = 20, epoch=100){

  x_data <- t(sapply(1:(length(x) - lag - h + 1),
                     function(i) x[i:(i + lag - 1)]))
  
  y_data <- t(sapply((1 + lag):(length(x) - h + 1),
                     function(i) x[i:(i + h - 1)]))
  
  
  x_train <- array(x_data, dim = c(nrow(x_data), lag, 1))
  if (h == 1) {
    y_train <- array(y_data, dim = c(length(y_data), h, 1))
  } else {
    y_train <- array(y_data, dim = c(nrow(y_data), h, 1))
  }
  
  
  # Define model
  model <- keras_model_sequential()
  
  model <- layer_lstm(model, units = 50, input_shape = c(lag, 1), return_sequences = TRUE)
  model <- layer_dropout(model, rate = 0.2)
  model <- layer_lstm(model, units = 50, return_sequences = FALSE)
  model <- layer_dropout(model, rate = 0.2)
  model <- layer_dense(model, units = h)
  
  compile(model, loss = 'mse', optimizer = 'adam')
  
  # Fit model
  fit(model, x = x_train, y = y_train, epochs = epoch, batchsize = BS)
  
  
  x_test <- x[(length(x) - lag + 1): length(x)]
  x_test_arr <- array(x_test, dim = c(1, lag, 1))
  
  # Forecast
  predicted <- predict(model, x_test_arr, batch_size = BS)
  predicted_ts <- ts(as.numeric(predicted), end = end(x), frequency = 251)
  
  result <- list()
  result$mean <- rep(mean(x), h)
  result$mean <- predicted_ts
  class(result) <- 'forecast'
  return(result)
}

ts_CV_LSTM <- function(ts, fcast_win, initialTrain, best_k){

  residuals <- tsCV(ts, forecast_LSTM, 
                    h = fcast_win, initial = initialTrain)
  
  residuals <- as.matrix(residuals)
  return(residuals[(initialTrain+1):(nrow(residuals)-ncol(residuals)),])
}

# Function for performing the cross-validation
cross_validate <- function(targetVariable, material, model_function, 
                          fcast_win, initialTrain, best_k){
  
  data_material <- data[data$MATERIALART == material, c('ZAEHLDATUM', targetVariable)]
  ts_material <- ts(data_material[order(data_material$ZAEHLDATUM), targetVariable], frequency = 251)
  
  CV_res <- model_function(ts_material, initialTrain = initialTrain, 
                          fcast_win = fcast_win, best_k = best_k)
  return(CV_res)
}


# List of the different forecasting functions
model_functions <- list(ts_CV_LSTM = ts_CV_LSTM)

# The different materials
materials <- levels(data$MATERIALART)

# The different forecast windows
fcast_wins <- 1:20

# All combinations
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

residuals <- cross_validate(targetVariable = 'ANZAHL_GEBINDE_NMZ',
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