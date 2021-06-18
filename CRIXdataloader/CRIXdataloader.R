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

