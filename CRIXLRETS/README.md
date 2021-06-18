[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CRIXLRETS** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: 'CRIXLRETS'

Published in: 'Master Thesis'

Description: 'R-Script with ETS model training used on CRIX Forecasting App'

Keywords: 'CRIX, Bitcoin, Ethereum, Forecast, ETS'

Author: 'Gonzalo Garcia'

See also: 'optional: listing related Quantlets or Github code'

Submitted:  '16. Jun 2021'

Datafile: 'CRIXLRETS.R'

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


# Daily log returns

coin <- get_crypto()
coin$date <- ymd(coin$date)

log_returns <- diff.xts(coin[, 2], lag = 1, log = T)
log_returns <- xts(log_returns, order.by = coin$date)
log_returns <- na.omit(log_returns)
train <- log_returns[1:obs_on_train_set]

## Time Series Cross Validation for ETS

# Naive

model = naive

# compute CV MAE

e_naive = tsCV(train, model, initial = 12, h = 14)
mae_naive = colMeans(abs(e_naive), na.rm = T)

# Mean Forecast 

model = meanf

# compute CV MAE

e_meanf = tsCV(train, model, initial = 12, h = 14)
mae_meanf = colMeans(abs(e_meanf), na.rm = T)

# ETS 

model = function(y, h) {
  forecast(ets(y), h = h)
}

# compute CV MAE

e_ets = tsCV(train, model, h = 14)
mae_ets = mean(abs(e_ets), na.rm = T)


# Plotting MAE against the forecast horizon
mae <-
  cbind(mae_naive, mae_ets, mae_meanf)

mae %>% ts() %>%
  dygraph(main = 'Cross-Validation', ylab = "MAE") %>%
  dyAxis("y") %>%
  dySeries("mae_naive", label = 'Naive Model') %>%
  dySeries("mae_ets", label = 'ETS Model') %>%
  dySeries("mae_meanf", label = 'Mean Model') %>%
  dyLegend(labelsSeparateLines = TRUE) %>%
  dyOptions(digitsAfterDecimal = 6) %>%
  dyCSS(
    textConnection(
      ".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }"
    )
  )

# ETS
ets <- train %>%
  ets()

saveRDS(ets, file = "models/ets.rds")
```

automatically created on 2021-06-18