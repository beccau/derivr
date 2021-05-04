
one_day = 0.0027397260273972603


#' Value of a put option
#'
#' @export 
bs_put <- function(strike, spot, days_to_exp, vol, interest, dividends=0) {
  r <- interest
  d <- dividends
  years_to_exp <- one_day * days_to_exp
  
  return (bs("put", strike, spot, years_to_exp, vol, r, d))
}

#' Value of a call option
#'
#' @export 
bs_call <- function(strike, spot, days_to_exp, vol, interest, dividends=0) {
  r <- interest
  d <- dividends
  years_to_exp <- one_day * days_to_exp
  
  return (bs("call", strike, spot, years_to_exp, vol, r, d))
}

#' Baisc bs value option
#'
#' @export 
bs <- function(type, strike, spot, years_to_exp, vol, interest, dividends=0) {
  S <- spot
  K <- strike
  t <- years_to_exp
  v <- vol
  r <- interest
  d <- dividends
  
  d1 <- (log((S) / K) + ((r - d) + v * v / 2.0) * t) / (v * sqrt(t))
  d2 <- d1 - v * sqrt(t)
  
  if (type == "call") {
    return (S * exp(-d * t) * stats::pnorm(d1) - K * exp(-r * t) * stats::pnorm(d2))
  } else if (type == "put") {
    return (K * exp(-r * t) * stats::pnorm(-d2) - S * exp(-d * t) * stats::pnorm(-d1))
  }
}

d1 <- function(S, K, r, v, t) {
  (log(S/K) + (r + v^2 / 2) * t) / (v * sqrt(t))
}

d2 <- function(d1, v, t) {
  d1 - v * sqrt(t)
}

# --- The Greeks

#' Delta of a call option
#'
#' @export 
delta_call <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest

  stats::pnorm(d1(S, K, r, v, t))
}

#' Delta of a put option
#'
#' @export 
delta_put <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest
  
  stats::pnorm(d1(S, K, r, v, t)) - 1
}

#' Gamma of an option
#'
#' @export 
gamma_call_put <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest
  
  stats::dnorm(d1(S, K, r, v, t)) / (S * v * sqrt(t))
}

#' Vega of an option
#'
#' @export 
vega_call_put <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest
  
  S * stats::dnorm(d1(S, K, r, v, t)) * sqrt(t)
}

#' Theta of a call option
#'
#' @export 
theta_call <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest
  
  d_1 <- d1(S, K, r, v, t)
  - S * stats::dnorm(d_1) * v / (2 * sqrt(t)) - (r * K * exp(-r * t) * stats::pnorm(d2(d_1, v, t)))
}

#' Theta of a put option
#'
#' @export 
theta_put <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest

    d_1 <- d1(S, K, r, v, t)
  - S * stats::dnorm(d_1) * v / (2 * sqrt(t)) + r * K * exp(-r * t) * stats::pnorm(-d2(d_1, v, t))
}

#' Rho of a call option
#'
#' @export 
rho_call <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest

  K * t * exp(-r * t) * stats::pnorm(d2(d1(S, K, r, v, t), v, t)) 
}

#' Rho of a put option
#'
#' @export 
rho_put <- function(strike, spot, days_to_exp, vol, interest) {
  S <- spot
  K <- strike
  t <- one_day * days_to_exp
  v <- vol
  r <- interest
  
  - K * t * exp(-r * t) * stats::pnorm(-d2(d1(S, K, r, v, t), v, t)) 
}
