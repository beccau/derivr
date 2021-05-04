library(derivr)
library(dplyr)
library(plotly)

# Greeks

# Option
strike <- 180
days <- 10
vol <- 0.25
r <- 0.001
spot_price <- seq(100, 200)

price <- bs_call(strike, spot_price, days, vol, r)
delta <- delta_call(strike, spot_price, days, vol, r)
vega  <- vega_call_put(strike, spot_price, days, vol, r)
gamma <- gamma_call_put(strike, spot_price, days, vol, r)
theta <- theta_call(strike, spot_price, days, vol, r)
rho   <- rho_call(strike, spot_price, days, vol, r)


data <- data.frame(spot_price)
plotly::plot_ly(data, x= ~spot_price, y = ~price, name='Value', type='scatter', mode='lines') %>%
  plotly::add_trace(y = ~delta, name='delta', line = list(dash = 'dash')) %>%
  plotly::add_trace(y = ~vega, name='vega', line = list(dash = 'dash'))  %>%
  plotly::add_trace(y = ~gamma, name='gamma', line = list(dash = 'dash'))  %>%
  plotly::add_trace(y = ~theta, name='theta', line = list(dash = 'dash'))  %>%
  plotly::add_trace(y = ~rho, name='rho', line = list(dash = 'dash'))
  


df <- data.frame(spot_price, price, delta, vega, gamma, theta, rho)
View(df)

