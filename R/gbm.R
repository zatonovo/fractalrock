# Wiener process
# http://www.cscamm.umd.edu/lectures/evandenlectures_final.pdf
# Ornstein-Uhlenbeck process
# http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/
# http://www.ms.unimelb.edu.au/publications/RampertshammerStefan.pdf


# http://www.columbia.edu/~mh2078/MCS04/MCS_framework_FEegs.pdf
gbm(n, s0=10, mu=0.01, sigma=0.03) %as% {
  cumprod(c(s0, exp((mu - sigma^2/2) / 252 + sigma*rnorm(n-1) / sqrt(252))))
}

wiener(n, s=10, mu=0.01, sigma=0.03) %as% {
  wiener(n, s, mu, sigma, s)
}

wiener(0, s, mu=0.01, sigma=0.03, acc) %as% acc

wiener(n, s, mu=0.01, sigma=0.03, acc) %as% {
  #s1 <- s * mu / 252 + sigma * rnorm(1) / sqrt(252)
  s1 <- s * (1 + mu / 252) + sigma * rnorm(1) / sqrt(252)
  wiener(n-1, s1, mu, sigma, c(acc, s1))
}





#rprocess('gbm', s) %as% 

