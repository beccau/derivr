library(testthat)
library(derivr)

test_that("BS Put option", {
  expect_equal(bs_put(100, 110, 90, 0.5, 0.001), 6.11, tolerance=0.001)
})

test_that("BS Call option", {
  expect_equal(bs_call(100, 110, 90, 0.5, 0.001), 16.14, tolerance=0.001)
})

# delta call
test_that("BS Delta Call option", {
  expect_equal(delta_call(100, 110, 90, 0.5, 0.001), 0.694, tolerance=0.001)
})

# delta put
test_that("BS Delta Put option", {
  expect_equal(delta_put(100, 110, 90, 0.5, 0.001), -0.306, tolerance=0.001)
})

# gamma_call_put
test_that("BS Gamma Call/Put option", {
  expect_equal(gamma_call_put(100, 110, 90, 0.5, 0.001), 0.013, tolerance=0.001)
})

# vega call_put
test_that("BS Vega Call/Put option", {
  expect_equal(vega_call_put(100, 110, 90, 0.5, 0.001), 19.153, tolerance=0.001)
})

# theta_call
test_that("BS Theta Call option", {
  expect_equal(theta_call(100, 110, 90, 0.5, 0.001), -19.469, tolerance=0.001)
})

# theta_put
test_that("BS Theta Put option", {
  expect_equal(theta_put(100, 110, 90, 0.5, 0.001), -19.369, tolerance=0.001)
})

# rho_call
test_that("BS Rho Call option", {
  expect_equal(rho_call(100, 110, 90, 0.5, 0.001), 14.855, tolerance=0.001)
})

# rho_put
test_that("BS Rho Put option", {
  expect_equal(rho_put(100, 110, 90, 0.5, 0.001), -9.790, tolerance=0.001)
})
