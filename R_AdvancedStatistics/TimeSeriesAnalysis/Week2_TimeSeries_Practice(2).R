#===========================
# Week 2
#===========================

### MA(1) process

set.seed(1)
eps = rnorm(n = 1000, mean = 0, sd = 1)
n = length(eps)

theta = 0.3
X.t = c()

for(i in 2:n){
  X.t[i] = eps[i] + theta * eps[(i-1)]
  if(i == n){
    X.t = X.t[-1]
  }
}

X.mean = mean(X.t); X.mean
var(X.t); (1+theta^2)*(1)

plot(X.t, type = "l")
abline(h = X.mean, col = "red", lwd = 2)



### AR(1) process when phi = 1 (random walk)
set.seed(2)
ar.eps = rnorm(n = 1000, mean = 0, sd = 1)
random.walk = cumsum(ar.eps)
rw.mean = mean(random.walk)

plot(random.walk, type = "l", xlab = "Time", ylab = "X.t", main = "Time series plot of Random walk")
abline(h = rw.mean, col = "red", lwd =2)



### arima.sim
?arima.sim

#====== order ========

set.seed(1)
ar.ex = arima.sim(model = list(order = c(1,0,0), ar = 0.5), n = 100, n.start = 1)
plot(ar.ex, main = "Time series plot for AR(1) model (phi = 0.5)")
abline(h = mean(ar.ex), col = grey(0.5), lty = 2)

set.seed(1)
ar.ex = arima.sim(model = list(ar = 0.5), n = 100, n.start = 1)
plot(ar.ex, main = "Time series plot for AR(1) model (phi = 0.5)")
abline(h = mean(ar.ex), col = grey(0.5), lty = 2)
abline(v = 30, col = "red")

#=========== burn in =============

set.seed(1)
ar.ex = arima.sim(model = list(ar = 0.5), n = 100, n.start = 30)
plot(ar.ex, main = "Time series plot for AR(1) model (phi = 0.5)")
abline(h = mean(ar.ex), col = grey(0.5), lty = 2)
abline(v = 70, col = "red", lty = 2)

set.seed(1)
x.t = 15
eps3 = rnorm(n = 130, mean = 0, sd = 1)
for(i in 2:130){
  x.t[i] = 0.5 * x.t[(i-1)] + eps3[i]
}
plot(x.t, type = "l", main = "Time series plot for AR(1) model without burn in")

burn.in.xt = x.t[-c(1:30)]
plot(burn.in.xt, type = "l", main = "Time series plot for AR(1) model using burn in")


#==========rand.gen==============
set.seed(1)
ar.ex = arima.sim(model = list(ar = 0.5), n = 300, rand.gen = function(n) rt(n, df = 5))
plot(ar.ex, main = "Time series plot for AR(1) with t(5) distributed errors")
abline(h = 0, col = grey(0.5), lty  = 2)


#======= second order=============
set.seed(1)
ar.ex2 = arima.sim(model = list(ar = c(0.5, 0.3)), n = 300, n.start = 30)
plot(ar.ex2, main = "Time series plot for AR(2)")

