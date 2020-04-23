#' Find impvol of an option
#'
#' @export 
bsimpv <- function(strike, spot, days_to_exp, value, type) {
  if (type == "call") {
    return (bsimpv_call(strike, spot, days_to_exp, value))
  } else {
    return (bsimpv_put(strike, spot, days_to_exp, value))
  }
}

#' Find impvol of a call option
#'
#' @export 
bsimpv_call <- function(strike, spot, days_to_exp, call_value) {
  bs1 = function(v1) {
    b = bs_call(strike = strike, spot = spot, days_to_exp = days_to_exp, vol=v1)
    return (b - call_value)
  }
  
  tryCatch(
    pracma::brent(bs1, 0, 1)$root, 
    error=function(cond) {
      return(NA)
    }
  )
}

#' Find impvol of a put option
#'
#' @export 
bsimpv_put <- function(strike, spot, days_to_exp, put_value) {
  bs1 = function(v1) {
    b = bs_put(strike = strike, spot = spot, days_to_exp = days_to_exp, vol=v1)
    return (b - put_value)
  }
  
  tryCatch(
    pracma::brent(bs1, 0, 1)$root, 
    error=function(cond) {
      return(NA)
    }
  )
}
