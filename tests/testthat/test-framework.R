context("test framework.R")

test_that("test trading_dates function", {
  date <- '2016-01-01'
  res <- trading_dates(date, 0)
  expect_that(is.null(res), is_true())
  res <- trading_dates(date, 1)
  expect_that(res, equals(as.Date('2016-01-04')))
  res <- trading_dates(date, 2)
  expect_that(res, equals(as.Date(c('2016-01-04', '2016-01-05'))))

  date <- '2016-01-04'
  res <- trading_dates(date, -1)
  expect_that(res, equals(as.Date('2016-01-04')))
  res <- trading_dates(date, -2)
  expect_that(res, equals(as.Date(c('2015-12-31', '2016-01-04'))))
})
