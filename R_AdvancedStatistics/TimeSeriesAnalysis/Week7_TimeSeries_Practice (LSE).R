#===========================
# Week 7
#===========================

### Least square estimator : AR(1)

xt = arima.sim(model = list(ar = 0.3), n = 1000)

lm(formula = xt[-length(xt)] ~ xt[-1])

phi.hat = sum(xt[-1] * xt[-length(xt)]) / sum(xt[-1]^2)
phi.hat

lm(formula = xt[-length(xt)] ~ xt[-1] + 0)


# Asymptotic property

num.vec = c()
denom.vec = c()

set.seed(123)

for(i in 1:1000)
{
  len = 5000
  et = rnorm(n = len, mean = 0, sd = 2)
  xt = et[1]
  for(j in 2:len){
    xt[j] = 0.3 * xt[(j-1)] + et[j]
  }
  
  num.vec[i] = sum(xt[-(len-1)] * et[-1]) / sqrt(len)
  denom.vec[i]= sum(xt[-1]^2)/len
}

hist(num.vec) 
mean(num.vec) 
Ex.sq = 4/(1-0.3^2) # E(x_t^2) = s^2/(1-phi^2)
var(num.vec); 4*Ex.sq

mean(denom.vec); Ex.sq
var(denom.vec)

z.phi = num.vec/denom.vec
hist(z.phi, breaks = 20)
mean(z.phi)
var(z.phi); 4/Ex.sq




### Least square estimator : AR(p)

set.seed(123)
xt = arima.sim(model = list(ar = c(0.3, 0.2, 0.1)), n = 500)

p = 3
n = length(xt)

xtx.inv = 0
for(t in (p+1):n)
{
  window = (t-1):(t-p)
  x.lag = xt[window]
  xtx.inv = xtx.inv + x.lag %*% t(x.lag)
  if(t == n){xtx.inv = solve(xtx.inv)}
}

xtx = 0
for(t in (p+1):n)
{
  window = (t-1):(t-p)
  x.lag = xt[window]
  xtx = xtx + xt[t] * x.lag
}

phi.hat = xtx.inv %*% xtx; phi.hat

lm(formula = xt[(p+1):n] ~ xt[p:(n-1)] + xt[(p-1):(n-2)] + xt[(p-2):(n-3)] + 0)


# Asymptotic property

phi.hat = matrix(data = NA, nrow = 1000, ncol = 3)

set.seed(123)
for(i in 1:1000)
{
  x = arima.sim(model = list(ar = c(0.3, 0.2, 0.1)), n = 5000)
  p = 3
  n = length(x)
  phi.hat[i,] = as.vector((lm(x[(p+1):n] ~ x[p:(n-1)] + x[(p-1):(n-2)] + x[(p-2):(n-3)] + 0)$coefficients))
}

z.phi = sqrt(5000) * t(t(phi.hat) - c(0.3,0.2,0.1))

colMeans(z.phi)
cov(z.phi)

# acf
params = c(0.3, 0.2, 0.1)
acf.ar3 = ARMAacf(ar = params, lag.max = 2)
acf.ar3 = acf.ar3 * 1.298
gamma = matrix(data = c(acf.ar3, acf.ar3[2], acf.ar3[1], acf.ar3[2], acf.ar3[3:1]), 
                        nrow = 3, ncol = 3, byrow = T)
solve(gamma)




## quantmod
library(quantmod)
library(jsonlite)
usd_krw <- getSymbols(Symbols="KRW=X", 
                      src = "yahoo", 
                      from=  Sys.Date() - 180, 
                      to =  Sys.Date(), auto.assign = FALSE)
plot(usd_krw$`KRW=X.Close`)

kospi = getSymbols(Symbols="^KS11", 
                   src = "yahoo", 
                   from= "2021-01-01", 
                   to = Sys.Date(), auto.assign = FALSE)
plot(kospi$KS11.Close)

kosdaq = getSymbols("^kq11",
                    src = "yahoo", 
                    from= Sys.Date() - 1500, 
                    to = Sys.Date(), auto.assign = FALSE)
plot(kosdaq$KQ11.Close)

tesla = getSymbols(Symbols="TSLA",
                   src = "yahoo", 
                   from=  Sys.Date() - 365, 
                   to =  Sys.Date(), auto.assign = FALSE)
plot(tesla$TSLA.Close)

apple = getSymbols(Symbols="AAPL",
                   src = "yahoo", 
                   from=  Sys.Date() - 365, 
                   to =  Sys.Date(), auto.assign = FALSE)
plot(apple$AAPL.Close)

samsung = getSymbols("005930.KS",
                     src = "yahoo", 
                     from= Sys.Date() - 365, 
                     to = Sys.Date(), auto.assign = FALSE)
plot(samsung$`005930.KS.Close`)

jyp = getSymbols("035900.KQ", 
                 src = "yahoo", 
                 from= Sys.Date() - 365, 
                 to = Sys.Date(), auto.assign = FALSE) # error
