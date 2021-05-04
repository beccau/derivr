library(derivr)
library(dplyr)
library(plotly)

# Plot PnL for three options:

spot_price <- seq(100, 200)
call1 <-  10 * bs_call(180, spot_price, 7, 0.25, 0.001)
call2 <- -10 * bs_call(170, spot_price, 7, 0.30, 0.001)
put1 <-   10 * bs_put(140, spot_price, 7, 0.40, 0.001)

sum <- call1 + call2 + put1
data <- data.frame(spot_price)
atm <- 150
max_val <- max(call1, call2, put1, sum)
min_val <- min(call1, call2, put1, sum)

plotly::plot_ly(data, x= ~spot_price, y = ~sum, name='Sum', type='scatter', mode='lines') %>%
  plotly::add_trace(y = ~call1, name='CALL1', line = list(dash = 'dash')) %>%
  plotly::add_trace(y = ~call2, name='CALL2', line = list(dash = 'dash')) %>%
  plotly::add_trace(y = ~put1, name='PUT1', line = list(dash = 'dash')) %>%
  plotly::add_segments(x = atm, xend = atm, y = max_val, yend = min_val, name='ATM', line=list(dash = 'dot'))


