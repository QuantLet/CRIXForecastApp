[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CRIXLRML** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: 'CRIXLRML'

Published in: 'Master Thesis'

Description: 'R-Script with Machine Learning model training used on CRIX Forecasting App'

Keywords: 'CRIX, Bitcoin, Ethereum, Forecast, ML, LSTM, GRU'

Author: 'Gonzalo Garcia'

See also: 'optional: listing related Quantlets or Github code'

Submitted:  '16. Jun 2021'

Datafile: 'CRIXLRML.R'

Input: 'optional'

Output:  'optional'
```

### R Code
```r

source("CRIXdataloader/CRIXdataloader.R")
library("lubridate")
library("dygraphs")
library("forecast")
library("xts")
library("zoo")
library("dplyr")
library("reshape2")
library("keras")
library("reticulate")
library("timetk")


obs_on_train_set = 1680 # ~70%. val_set ~15%. test_set ~15%. Originally, proportions have drifted a bit with time.


# ML Models

# Daily log returns

coin <- get_crypto()
coin$date <- ymd(coin$date)

log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
log_returns <- xts(log_returns, order.by = coin$date)
log_returns <- na.omit(log_returns)


# For training

train_data <- as.matrix(log_returns[1:obs_on_train_set])
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data_1 <- scale(log_returns, center = mean, scale = std)
data <- t(data_1) #Transpose matrix

# For prediction

last_30_observations <- tail(log_returns,30)
pred_data <- as.matrix(last_30_observations)
pred_data_1 <- scale(pred_data, center = mean, scale = std)
pdata <- t(pred_data_1) #Transpose matrix
dim(pdata) <- c(dim(pdata), 1)


generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 1) {
  if (is.null(max_index)) {
    max_index <- nrow(data) - delay - 1
  }
  i <- min_index + lookback
  function() {
    if (shuffle) {
      # Doesn't draw samples in order, but maintains order within sample
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index) {
        i <<- min_index + lookback
      }
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), # Batch_size = 40
                                lookback / step, # Timesteps = 30
                                1)) # Only one feature
    
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices]
      targets[[j]] <- data[rows[[j]] + delay]
    }           
    list(samples, targets)
  }
}

# Past days needed for prediction
lookback <- 30
# One observation per day
step <- 1
# Predicting only one step into the future
delay <- 1
# How many obs in each sample
batch_size <- 40

# Generator instances

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = obs_on_train_set,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = obs_on_train_set + 1,
  max_index = 2040,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 2040 + 1,
  max_index = ncol(data),
  step = step,
  batch_size = batch_size
)

# This defines how many steps I have to draw from val_gen to see the entire validation set
val_steps <- (2040 - obs_on_train_set + 1 - lookback) / batch_size

# Same as last line for the test set
test_steps <- (ncol(data) - 2041 - lookback) / batch_size

# Necessary for known bug avoidance.
train_gen_py <- keras:::as_generator.function(train_gen)
val_gen_py <- keras:::as_generator.function(val_gen)
test_gen_py <- keras:::as_generator.function(test_gen)


# callback_model_checkpoint(
#   filepath,
#   monitor = "val_loss",
#   verbose = 0,
#   save_best_only = FALSE,
#   save_weights_only = FALSE,
#   mode = c("auto", "min", "max"),
#   period = NULL,
#   save_freq = "epoch"
# )

#Simple ML model (val_loss = 0.6894)

model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(lookback / step, 1)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen_py,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen_py,
  validation_steps = val_steps,
  callbacks = list(
    callback_model_checkpoint(
      "models/ML_baseline",
      monitor = "val_loss",
      verbose = 1,
      save_best_only = TRUE,
      save_weights_only = FALSE,
      save_freq = "epoch"
    )
  )
)

model %>% save_model_tf("models/ML_baseline")

ml_baseline_model <- load_model_tf("models/ML_baseline")


# GRU (val_loss = 0.65288)

model <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = list(NULL, 1)) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen_py,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen_py,
  validation_steps = val_steps,
  callbacks = list(
    callback_model_checkpoint(
      "models/GRU",
      monitor = "val_loss",
      verbose = 1,
      save_best_only = TRUE,
      save_weights_only = FALSE,
      save_freq = "epoch"
    ))
)

model %>% save_model_tf("models/GRU")

gru_model <- load_model_tf("models/GRU")

gru_model %>% predict(
  pdata ,
  batch_size = 40)

# GRU+DO
# Incorporate Drop-Out (val_loss = 0.64636)

model <- keras_model_sequential() %>%
  layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.4,
            input_shape = list(NULL, 1)) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen_py,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen_py,
  validation_steps = val_steps,
  callbacks = list(
    callback_model_checkpoint(
      "models/GRU_DO",
      monitor = "val_loss",
      verbose = 1,
      save_best_only = TRUE,
      save_weights_only = FALSE,
      save_freq = "epoch"
    ))
)

model %>% save_model_tf("models/GRU_DO")

gru_do_model <- load_model_tf("models/GRU_DO")

# LSTM + DO (val_loss = 0.64543)

model <- keras_model_sequential() %>%
  layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.6,
             input_shape = list(NULL, 1)) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen_py,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen_py,
  validation_steps = val_steps,
  callbacks = list(
    callback_model_checkpoint(
      "models/LSTM_BEST_new_version", 
      monitor = "val_loss",
      verbose = 1,
      save_best_only = TRUE,
      save_weights_only = FALSE,
      save_freq = "epoch"
    ))
)


lstm_do <- load_model_tf("models/LSTM_BEST")


##### From here only for chosen model #####

# loss = 0.6306038.
lstm_do %>% evaluate_generator(
  test_gen_py,
  steps = test_steps)

# For prediction

last_30 <- as.matrix(last_30_observations)
last_30 <- scale(last_30, center = mean, scale = std)
last_30 <- t(last_30) #Transpose matrix
dim(last_30) <- c(dim(last_30),1)

# Prediction
lstm_do %>% predict(
  pdata ,
  batch_size = 40)

##### Up to here only for chosen model #####

# GRU stacked (0.64749) -> Overfits

model <- keras_model_sequential() %>%
  layer_gru(units = 32,
            return_sequences = TRUE,
            input_shape = list(NULL, 1)) %>%
  layer_gru(units = 32, activation = "relu",
            dropout = 0.3,
            recurrent_dropout = 0.7) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen_py,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen_py,
  validation_steps = val_steps,
  callbacks = list(
    callback_model_checkpoint(
      "models/GRU_stacked_2",
      monitor = "val_loss",
      verbose = 1,
      save_best_only = TRUE,
      save_weights_only = FALSE,
      save_freq = "epoch"
    ))
)

gru_stacked_2_model <- load_model_tf("models/GRU_stacked_2")

# LSTM stacked (0.64745) <- Overfits

model <- keras_model_sequential() %>%
  layer_lstm(units = 32,
             return_sequences = TRUE,
             input_shape = list(NULL, 1)) %>%
  layer_lstm(units = 32, activation = "relu",
             dropout = 0.5,
             recurrent_dropout = 0.7) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen_py,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen_py,
  validation_steps = val_steps,
  callbacks = list(
    callback_model_checkpoint(
      "models/LSTM_stacked",
      monitor = "val_loss",
      verbose = 1,
      save_best_only = TRUE,
      save_weights_only = FALSE,
      save_freq = "epoch"
    ))
)

lstm_stacked_model <- load_model_tf("models/LSTM_stacked")

# LSTM + Stacked + Drop-Out (Overfits)
# (val_loss = 0.64631)

model <- keras_model_sequential() %>%
  layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.6, return_sequences = TRUE,
             input_shape = list(NULL, 1)) %>%
  layer_lstm(units = 64, dropout = 0.4, recurrent_dropout = 0.6) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen_py,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen_py,
  validation_steps = val_steps,
  callbacks = list(
    callback_model_checkpoint(
      "models/LSTM_DO_Stacked",
      monitor = "val_loss",
      verbose = 1,
      save_best_only = TRUE,
      save_weights_only = FALSE,
      save_freq = "epoch"
    ))
)
```

automatically created on 2021-06-18