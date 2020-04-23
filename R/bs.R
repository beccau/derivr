
one_day = 0.0027397260273972603
interest_rate = 0.001


#' Value of a put option
#'
#' @export 
bs_put <- function(strike, spot, vol, days_to_exp) {
  r = interest_rate  # ränta 0.1%
  d = 0  # utdelning = 0
  years_to_exp = one_day * days_to_exp
  
  return (bs("put", strike, spot, years_to_exp, vol, r))
}

#' Value of a call option
#'
#' @export 
bs_call <- function(strike, spot, vol, days_to_exp) {
  r = interest_rate  # ränta 0.1%
  d = 0  # utdelning = 0
  years_to_exp = one_day * days_to_exp
  
  return (bs("call", strike, spot, years_to_exp, vol, r))
}

#' Baisc bs value option
#'
#' @export 
bs <- function(type, strike, spot, years_to_exp, vol, interest) {
  S = spot
  K = strike
  t = years_to_exp
  v = vol
  r = interest
  d = 0 # dividends = 0
  
  d1 = (log((S) / K) + ((r - d) + v * v / 2.0) * t) / (v * sqrt(t))
  d2 = d1 - v * sqrt(t)
  
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
delta_call <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%

  stats::pnorm(d1(S, K, r, v, t))
}

#' Delta of a put option
#'
#' @export 
delta_put <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%
  
  stats::pnorm(d1(S, K, r, v, t)) - 1
}

#' Gamma of an option
#'
#' @export 
gamma_call_put <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%
  
  stats::dnorm(d1(S, K, r, v, t)) / (S * v * sqrt(t))
}

#' Vega of an option
#'
#' @export 
vega_call_put <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%
  
  S * stats::dnorm(d1(S, K, r, v, t)) * sqrt(t)
}

#' Theta of a call option
#'
#' @export 
theta_call <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%
  
  d_1 = d1(S, K, r, v, t)
  - S * stats::dnorm(d_1) * v / (2 * sqrt(t)) - (r * K * exp(-r * t) * stats::pnorm(d2(d_1, v, t)))
}

#' Theta of a put option
#'
#' @export 
theta_put <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%

    d_1 = d1(S, K, r, v, t)
  - S * stats::dnorm(d_1) * v / (2 * sqrt(t)) + r * K * exp(-r * t) * stats::pnorm(-d2(d_1, v, t))
}

#' Rho of a call option
#'
#' @export 
rho_call <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%

  K * t * exp(-r * t) * stats::pnorm(d2(d1(S, K, r, v, t), v, t)) 
}

#' Rho of a put option
#'
#' @export 
rho_put <- function(strike, spot, days_to_exp, vol) {
  S = spot
  K = strike
  t = one_day * days_to_exp
  v = vol
  r = interest_rate  # ränta 0.1%
  
  - K * t * exp(-r * t) * stats::pnorm(-d2(d1(S, K, r, v, t), v, t)) 
}
