#===========================
# Week 4
#===========================

### Central Limit Theorem

# N(1,4)
set.seed(1)
clt.norm = c()
for(i in 1:10000)
{
  x = rnorm(n = 1000, mean = 1, sd = 2)
  n = length(x)
  bar.x = mean(x)
  clt.norm[i] = sqrt(n) * (bar.x - 1)
}
hist(clt.norm)
mean(clt.norm)
var(clt.norm)

# Exp(1/2)
set.seed(1)
clt.exp = c()
for(i in 1:10000)
{
  x = rexp(n = 1000, rate = 1/2)
  n = length(x)
  bar.x = mean(x)
  clt.exp[i] = sqrt(n) * (bar.x - 2)
}
hist(clt.exp)
mean(clt.exp)
var(clt.exp)


# AR(1) : X_t = 0.3 *X_{t-1} + e_t, e_t ~ i.i.d. N(0,1)
set.seed(1)
clt.ar = c()
for(i in 1:10000)
{
  xt = arima.sim(model = list(ar = 0.3), n = 1000)
  n = length(x)
  mu.hat = mean(xt)
  clt.ar[i] = sqrt(n) * (mu.hat - 0)
}
hist(clt.ar)
mean(clt.ar)
var(clt.ar); 1/(1-0.3)^2



### SACF, SPCACF

# covariance
library(mvtnorm)
set.seed(1)
sig = matrix(data = c(1,0.5, 0.5, 2), nrow = 2, ncol = 2, byrow = TRUE)
x = rmvnorm(n = 10000, mean = rep(0, 2), sigma = sig)
x1 = x[,1]
x2 = x[,2]
cov(x1, x2)


# AR(1) : phi_{1} = 0.5
# Autocovariance
set.seed(123)
ar1 = arima.sim(model = list(ar = 0.5), n= 10000)

0.5^1 / (1-0.5^2)
ar1.lag1 = ar1[-1]

cov(ar1.lag1, ar1[-length(ar1)])
acf.ar1 = acf(x = ar1, type = "covariance")
acf.ar1[1]

acf.ar1[2] ; 0.5^2 / (1-0.5^2) 


# consistency
set.seed(123)
x1 = arima.sim(model = list(ar = 0.7), n = 100)
x2 = arima.sim(model = list(ar = 0.7), n = 100000)
acf1 = acf(x1, type = "covariance" , plot = FALSE); acf1[1]
acf2 = acf(x2,type = "covariance", plot = FALSE); acf2[1]
0.7/(1-0.7^2)

acf1[2];acf2[2]
0.7^2/(1-0.7^2)


#Autocorrelation / Partial autocorrelation
par(mfrow = c(2,2))
acf(ar1, type = "covariance")
acf(ar1, type = "correlation")
acf(ar1, type = "partial")
pacf(ar1)



### Yule-Walker estimator

set.seed(123)
ar3 = arima.sim(model = list(ar = c(0.5)), n = 1000)

#install.packages("itsmr")
#library(itsmr)
ywe1 = itsmr::yw(ar3, 1)
ywe1$phi
a = acf(ar3, type = "correlation")
a$acf[2]


set.seed(123)
ar4 = arima.sim(model = list(ar = c(0.3, 0.2)), n = 1000)
ywe2 = itsmr::yw(ar4, 2)
b = acf(ar4)

rho1 = b$acf[2]
rho2 = b$acf[3]

phi1 = rho1 * (1-rho2) / (1-rho1^2)
phi2 = (rho2 - rho1^2) / (1-rho1^2)

ywe2$phi
c(phi1, phi2)


ywe3 = itsmr::yw(ar4, 1)
ywe3$phi
