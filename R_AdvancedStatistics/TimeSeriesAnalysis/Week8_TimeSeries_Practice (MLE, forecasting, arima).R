#==========================
# Week 8
#==========================

### Maximum Likelihood Estimation

#arima()

?arima
set.seed(123)
ar.dat = arima.sim(model = list(ar = c(0.5, 0.2)), n = 1000)
ar.fit = arima(x = ar.dat, 
               order = c(2,0,0), 
               include.mean = F,
               method = "CSS-ML")
ar.fit

ar.fit$coef     #추정된 계수
coef(ar.fit)
ar.fit$loglik   #likelihood
ar.fit$residuals


# AR(p) 모형에서 CSS와 lm() 결과 비교
set.seed(123)
ar1 = arima.sim(model = list(ar = c(0.3)), n = 1000)
ar1.css = arima(x = ar1, order = c(1,0,0), include.mean = F, method = "CSS"); coef(ar1.css)
ar1.lm = lm(formula = ar1[2:1000] ~ ar1[1:999] +0); coef(ar1.lm)
coef(arima(x = ar1, order = c(1,0,0), include.mean = F))

# likelihood 비교하기(CSS vs ML)
set.seed(123)
ar1.ml = arima(x = ar1, order = c(1,0,0),include.mean = F, method = "ML")
ar1.ml$loglik
ar1.css$loglik

# ARMA(p,q)모형 추정
set.seed(123)
arma.dat = arima.sim(model = list(ar = c(0.5, 0.3), ma = 0.1), n = 1000)
arma.fit = arima(x = arma.dat,
                 order = c(2,0,1),
                 include.mean = F,
                 method = "CSS-ML")
arma.fit

arma.ar.fit = arima(x = arma.dat,
                    order = c(1,0,0),
                    include.mean = F,
                    method = "CSS-ML")
arma.ar.fit

fitted.arma = arma.dat - arma.fit$residuals
fitted.ar = arma.dat - arma.ar.fit$residuals

plot(x = arma.dat, type = "l")
lines(x = fitted.arma, type = "l", col = "red", lwd = 2)
lines(x = fitted.ar, type = "l", col = "green")


# xreg : exogenous variables
set.seed(123)
x = arima.sim(model = list(ar = 0.7), n = 500)
z = rnorm(500, mean = 3, sd = 1)  # 외생 변수
y = x + 0.5 * z  # 시계열에 외생 변수 영향 추가

fit.y = arima(y, order = c(1, 0, 0), xreg = z, method = "ML")
fit.y


#Arima
library(forecast)
arma.fit.arima = Arima(y = arma.dat,
                       order = c(2,0,1),
                       include.mean = F)

arma.fit.arima
fitted.arma.arima = fitted(arma.fit.arima)
fitted.arma - fitted.arma.arima


### Forecasting
?forecast
set.seed(12345)
arma.dat2 = arima.sim(model = list(ar = c(0.5, 0.2)), n = 600)
train = arma.dat2[1:500]
test = arma.dat2[501:600]

arma.fit2 = arima(x = train,
                  order = c(2,0,0),
                  include.mean = F)
pred.arma = forecast(arma.fit2, h = 100)
pred.arma
pred.arma$mean
plot(pred.arma)

a = pred.arma$fitted
b = pred.arma$residuals
c = pred.arma$x
c -(a+b)

plot(test, type = "l")
lines(as.vector(pred.arma$mean), col = "red")

rmse1 = sqrt(mean((test - pred.arma$mean)^2)); rmse1

# one step ahead forecast using new observations without re-estimation
pred.arma2 = Arima(test, model = arma.fit2)
pred.arma2$fitted

plot(test, type = "l")
lines(pred.arma2$fitted, col = "red")

rmse2 = sqrt(mean(pred.arma2$residuals^2)); rmse2
accuracy(pred.arma2)


# one step ahead forecast using new observations with re-estimation
pred.arma3 = c()
for(i in 1:100)
{
  dat = arma.dat2[i:(499+i)]
  arima.fitting = arima(dat, order = c(2,0,0))
  pred.arma3[i] = forecast(arima.fitting, h = 1)$mean[1]
}
plot(test, type = "l")
lines(pred.arma3, col = "red")

residuals = test - pred.arma3
rmse3 = sqrt(mean(residuals^2))

rmse1;rmse2;rmse3

plot(pred.arma)
points(arma.dat2, type = "l")
points(501:600, fitted(pred.arma2), col = "red", type ="l")
points(501:600, pred.arma3, col = "green", type = "l")



### Variance Stabilizing Transformation
## power transformation; (y_t)^a
## Box-Cox transformation; {(y_t)^a - 1}/a, a!=0
## log transformation; log(y_t), a==0

### diff
## difference; y_t-y_{t-1} = (y_t-y_{t-1})/(t-(t-1))
## return; (y_t-y_{t-1})/y_{t-1} = (y_t / y_{t-1}) - 1
## logarithmic return; log(y_t)-log(y_{t-1}) = log(y_t / y_{t-1}) ~ return, when (y_t / y_{t-1}) ~ 1

(200-100); (50-100)
(200-100)/100; (50-100)/100
log(200)-log(100); log(50)-log(100)

### example
x = 50:200;x
plot(x, (x-100)/100, type='l') # return
points(x, log(x)-log(100), col=3, type='l') # logarithmic return

### apple stock price
library(quantmod)

apple = getSymbols(Symbols="AAPL",
                   src = "yahoo", 
                   from=  "2010-01-01", 
                   to =  Sys.Date(), auto.assign = FALSE)
head(apple)
plot(apple$AAPL.Close)
apple_close = as.numeric(apple$AAPL.Close)
n=length(apple_close)

## Variance Stabilizing Transformation
boxcox.y_t = BoxCox(apple_close, 0.1)
log.y_t = log(apple_close)

plot(boxcox.y_t, type="l")
plot(log.y_t, type="l")

## diff
difference.y_t = diff(apple_close)
return.y_t = (difference.y_t)/apple_close[-n]
log_return.y_t = diff(log(apple_close))

plot(difference.y_t, type="l")
plot(return.y_t, type="l")
plot(log_return.y_t, type="l")



### Autoregressive Integrated Movingaverage (ARIMA)

### data generating; ARIMA(1,1,1)
set.seed(11)
arma_sample <- arima.sim(model=list(ar=c(0.3), ma=c(0.1)), n=500)
plot(arma_sample)

# X_t : ARIMA
# Z_t = X_t - X_{t-1} : ARMA
# Z_1 = X_1
# Z_2 = X_2 - X_1 => X_2 = Z_1 + Z_2
# Z_3 = X_3 - X_2 => X_3 = Z_1 + Z_2 + Z_3

arima_sample <- cumsum(arma_sample) # integrated
plot(arima_sample, type='l')

set.seed(11)
arima_sample2 <- arima.sim(model=list(order=c(1,1,1), ar=c(0.3), ma=c(0.1)), n=500)
points(arima_sample2, col=2, type='l')

?arima.sim
arma_sample[1:4]
arima_sample[1:4]
arima_sample2[1:4]


## data fitting
arima(arima_sample, order = c(1,1,1))
