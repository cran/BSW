## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, results='markup', comment=""---------------------------------
library(testthat)
library(BSW)
df <- data.frame(y = rep(c(0, 1), each = 250), 
                 x = rep(c(0, 1, 0, 1), times = c(200, 50, 50, 200))
                 )
RR <- (200 * 250) / (50 * 250)
SE <- sqrt((1/200 + 1/50) - (1/250 + 1/250))
fit <- bsw(y ~ x, df)
out <- summary(fit)

## ---- echo=TRUE, results='markup', comment=""---------------------------------
test_that(desc = "Estimated relative risk is equal to 4",
          code = {
                  expect_equal(object = unname(exp(coef(fit)[2])),
                               expected = RR)
            }
          )

## ---- echo=TRUE, results='markup', comment=""---------------------------------
test_that(desc = "Estimated standard error is equal to 0.1303840",
          code = {
                  expect_equal(object = unname(out$std.err[2]), 
                               expected = SE)
            }
          )

## ---- echo=TRUE, results='markup', comment=""---------------------------------
test_that(desc = "Estimated z-value is equal to 10.63239",
          code = {
                  expect_equal(object = unname(out$z.value[2]), 
                               expected = log(RR) / SE)
            }
          )

## ---- echo=TRUE, results='markup', comment=""---------------------------------
test_that(desc = "Estimated 95% confidence interval limits are equal to 3.097968 and 5.164676",
          code = {
                  expect_equal(object = unname(exp(confint(fit)[2,])), 
                               expected = exp(log(RR) + SE * qnorm(c(0.025, 0.975))))
            }
          )

