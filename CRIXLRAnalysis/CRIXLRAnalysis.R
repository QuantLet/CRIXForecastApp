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


# Daily log returns

coin <- get_crypto()
coin$date <- ymd(coin$date)

log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
log_returns <- xts(log_returns, order.by = coin$date)
log_returns <- na.omit(log_returns)
train <- log_returns[1:obs_on_train_set]

##Analysis

# ACF

Acf(log_returns, lag.max = 33)

# PACF

Pacf(log_returns, lag.max = 33)

# Decomposition

stl <- stl(log_returns)

dyList <- list(
  dygraph(stl$time.series[,1], main = "seasonal", group = "mdecomp") %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 40, strokeColor = "Black", fillColor = "Black", retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")),
  
  dygraph(stl$time.series[,2], main = "trend", group = "mdecomp") %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 40, strokeColor = "Black", fillColor = "Black", retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")),
  
  dygraph(stl$time.series[,3], main = "remainder", group = "mdecomp") %>%
    dyLegend(labelsSeparateLines=TRUE) %>%
    dyRangeSelector(height = 40, strokeColor = "Black", fillColor = "Black", retainDateWindow = TRUE) %>%
    dyOptions(digitsAfterDecimal = 4) %>%
    dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")))

dyList[1]
dyList[2] 
dyList[3]
