# See https://github.com/yihui/testit
assert('Tautology is true', TRUE, TRUE)

date <- '2016-01-01'
res <- trading_dates(date, 0)
assert(is.null(res))
res <- trading_dates(date, 1)
assert(res == as.Date('2016-01-04'))
res <- trading_dates(date, 2)
assert(res == as.Date(c('2016-01-04', '2016-01-05')))

date <- '2016-01-04'
res <- trading_dates(date, -1)
assert(res == as.Date('2016-01-04'))
res <- trading_dates(date, -2)
assert(res == as.Date(c('2015-12-31', '2016-01-04')))

