[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CRIXdataloader** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: 'CRIXdataloader'

Published in: 'Master Thesis'

Description: 'R-Script to load CRIX Data from the web'

Keywords: 'CRIX, Bitcoin, Ethereum, Forecast, Data'

Author: 'Gonzalo Garcia'

See also: 'optional: listing related Quantlets or Github code'

Submitted:  '16. Jun 2021'

Datafile: 'CRIXdataloader.R'

Input: 'optional'

Output:  'optional'
```

### R Code
```r

get_crypto <- function(coin = 'crix') {
  if (coin == 'crix') {
    read.csv(url("http://data.thecrix.de/data/new_crix.csv")) 
  } 
  
  # This was only used to generate graphs for the paper itself. Can still be used to expand the App so I
  # left it as a placeholder.
  else if (coin == 'btc') {
    read.csv("Data\\btc.csv") 
  } else if (coin == 'eth') {
    read.csv("Data\\eth.csv") 
  } else if (coin == 'xrp') {
    read.csv("Data\\xrp.csv") 
  } else if (coin == 'usdt') {
    read.csv("Data\\usdt.csv") 
  } else if (coin == 'link') {
    read.csv("Data\\link.csv") 
  }  else if (coin == 'ltc') {
    read.csv("Data\\ltc.csv") 
  } else if (coin == 'ada') {
    read.csv("Data\\ada.csv") 
  } else if (coin == 'dot') {
    read.csv("Data\\dot.csv") 
  } else if (coin == 'bnb') {
    read.csv("Data\\bnb.csv") 
  } else if (coin == 'doge') {
    read.csv("Data\\doge.csv") 
  } 
  
}


```

automatically created on 2021-06-18