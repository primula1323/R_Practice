# Multivariate analysis lab 
# Written by S. Jung 

## Lab 3: Wishart distribution and inference on multivariate location 

# In this lab, we will explore 

# (1) Distribution of the Hoteling's T^2 and its variants. 
# (2-3) Wishart distribution, and an inference problem related to the Wishart parameter
# (4) Exercises for inference on multivariate locations 

library(mvtnorm)
 
# (II) Wishart distribution -----------------------------------------------

# A Wishart-distributed random matrices can be sampled using
# i) Random normal variates (through the definition)
# ii) by rWishart function 

p <- 3
Sigma <- matrix(c(10,3,1,
                  3,6,1,
                  1,1,6), nrow = 3)
m <- 100
X <- rmvnorm(n = m , sigma = Sigma)
M <- t(X) %*% X # just one random matrix 


# Now sample n = 100 many Wishart matrices
M.array <- rWishart(n = 500, df = m, Sigma = Sigma) #위샤트 샘플링. 
dim(M.array) #tensor 형태. 3*3 랜덤행렬 500개.

# Inspect the first two observations 
M.array[,,1]
M.array[,,2]

# Take a sample mean and (element-wise) variance and compare with their expected value. 
(sample.mean <- apply(M.array,c(1,2),mean))#마지막 인덱스 위치의 것들로만 루프 돌기.
(sample.var <- apply(M.array,c(1,2),var))

# Exercise 
# 1. Write down the population mean and population "element-wise" variance of the random
# Wishart matrix. Use the Frobenius Norm on the different between the empirical and population 
# mean matrices (and variance matrices)  to record the deviation.
# What will happen to your deviation if $m$ increases (or $n$ increases)? 

(pop.mean <- m*Sigma)
(pop.var <- m*(Sigma*Sigma+matrix(c(10,6,6),nrow=3)%*%matrix(c(10,6,6),nrow=1)))

# Frobenius norm on the difference between the empirical and population
norm((pop.mean-sample.mean)/m,type = 'f')
norm((pop.var-sample.var)/m,type = 'f')

# diverging $m$
m_lst <- c(1000,2000,3000,4000,5000)
m_mean = NULL
m_var = NULL
for (m in m_lst) {
  M.array <- rWishart(n = 1000, df = m, Sigma = Sigma)
  sample.mean <- apply(M.array,c(1,2),mean)
  sample.var <- apply(M.array,c(1,2),var)
  m_mean = cbind(m_mean,norm((pop.mean-sample.mean)/m,type = 'f'))
  m_var = cbind(m_var,norm((pop.var-sample.var)/m,type = 'f'))
}
par(mfrow=c(1,2))
plot(m_lst,m_mean,'l')
plot(m_lst,m_var,'l')
par(mfrow=c(1,1))

# diverging $n$ - LLN과 더 가까운 상황. 단 cov 있으므로 직접 관련되지도 않고, 수렴 속도 빠르지도 않음.
n_lst <- c(1000,2000,3000,4000,5000)
n_mean = NULL
n_var = NULL
m=1000
for (n in n_lst) {
  M.array <- rWishart(n = n, df = m, Sigma = Sigma)
  sample.mean <- apply(M.array,c(1,2),mean)
  sample.var <- apply(M.array,c(1,2),var)
  n_mean = cbind(n_mean,norm((pop.mean-sample.mean)/m,type = 'f'))
  n_var = cbind(n_var,norm((pop.var-sample.var)/m,type = 'f'))
}
par(mfrow=c(1,2))
plot(n_lst,n_mean,'l')
plot(n_lst,n_var,'l')
par(mfrow=c(1,1))

# (III) An inference problem for the Wishart parameter --------------------

# Let $M \sim Wishart(m, Sigma)$. Consider testing the null hypothesis of 
# $\Sigma = I$ vs $\Sigma \neq I$ based on only one observation of M. 
# We will construct a likelihood ratio test. 
# Assume $m > p$ and recall the density function of the Wishart matrix. 
# Then, i) $M/m$ is the m.l.e. of Sigma.
#       ii) The LR statistic, $-2 \log (L_0 / L_1)$ is a function of eigenvalues $\lambda_i$ of $M/m$, 
#          and is $ m \sum_{i=1}^k (\lambda_i - 1 - \log(\lambda_i))$.
#           $-2 \log (L_0 / L_1) = -m \log(|\Sigma^{-1}S|) - m*p + m*tr(\Sigma^{-1}S)$.
#       iii) Under H_0, the LR statistic follows chi^2 distribution with d.f. p(p+1)/2 (if m is large)

p <- 3
m <- 100
M  <- rWishart(n = 1, df = m, Sigma = diag(rep(1,p)))
lambdas <- eigen(M[,,1] / m )$values
m * ( sum(lambdas)  - p - sum(log(lambdas)))


# Exercise 
# 2. Verify the distribution of the LR statistic is indeed chi^2, using a qqplot. 

p <- 3
m <- 100
M  <- rWishart(n = 100, df = m, Sigma = diag(rep(1,p)))
LR <- c()

for (i in seq(1,100)) {
  sample.m=M[,,i]
  lambdas <- eigen(sample.m/m)$values
  LR <- append(LR,m*(sum(lambdas)-p-sum(log(lambdas))))
}

theo <- rchisq(100,p*(p+1)/2)
qqplot(theo,LR,main='Chisq Q-Q Plot', xlab='Theoretical Quantiles', ylab='Sample Quantiles')
abline(0,1)

        
# (IV) Exercises for inference on multivariate locations ----------------------------
# Textbook problem 5.9 (Demonstration)

mu0 <- c(100, 150, 50, 90, 17, 31)
sample.mean <- c(95.52, 164.38, 55.69, 93.39, 17.98, 31.13)
cov.mat <- matrix(c(3266.46, 1343.97, 731.54, 1175.50, 162.68, 238.37,
                    1343.97, 721.91, 324.25, 537.35, 80.17, 117.73,
                    731.54, 324.25, 179.28, 281.17, 39.15, 56.80,
                    1175.50, 537.35, 281.17, 474.98, 63.73, 94.85,
                    162.68, 80.17, 39.15, 63.73, 9.95, 13.88,
                    238.37, 117.73, 56.80, 94.85, 13.88, 21.26),nrow=6)
n=61
m=6
alpha=0.05


# (b) ellipse confidence interval for weight and girth
(x_ <- sample.mean[c(1,4)])
(s <- cov.mat[c(1,4),c(1,4)])
(coef <- qf(1-alpha,2,59))
(n-1)*2/((n-2)*n)
#{\mu: (\mu-x_).T %*% solve(s) %*% (\mu-x_) <= 0.0333426*3.153123}

# (c) Bonferroni confidence interval for each variable- 6개의 가설검정. 즉 5% 6회 하지 않을 것.
# $\bar{X}_i +- S_{ii}/\sqrt{n} t_{n-1}(\alpha/(2m))$ for each $i$.
coef <- qt(1-alpha/(2*m),n-1)
for (i in seq(1,6)) {
  print(c(sample.mean[i]-sqrt(cov.mat[i,i]/n)*coef,sample.mean[i]+sqrt(cov.mat[i,i]/n)*coef))
}

# (d) Construct the 95% Bonferroni confidence rectangle for the man weight and mean girth.
# Compare this rectangle with the confidence ellipse in Part b.
BCI.weight <- c(sample.mean[1]-sqrt(cov.mat[1,1]/n)*coef,sample.mean[1]+sqrt(cov.mat[1,1]/n)*coef)
BCI.girth <- c(sample.mean[4]-sqrt(cov.mat[4,4]/n)*coef,sample.mean[4]+sqrt(cov.mat[4,4]/n)*coef)

#install.packages("car")
#library(car)
plot(ellipse(shape=s,center=x_, radius=sqrt(0.0333426*3.153123)),type='l',xlab='weight',ylab='girth')
rect(BCI.weight[1],BCI.girth[1],BCI.weight[2],BCI.girth[2],border = 'blue') #타원 위 직사각형 그림.


# Exercise
# 3. Continued from Problem 5.9. Test whether means of the variables are (100, 150, 50, 90, 17, 31). 
# Provide a p-value. true sigma에 대한 가설검정과 true mu에 대한 가설검정은 서로 다름.

(LR = n*log(1+t(sample.mean-mu0)%*%solve((n-1)/n*cov.mat)%*%(sample.mean-mu0)))
1-pchisq(LR,6)


# (I) Distribution of the Hoteling's T^2 and its variants. -------------- : 새로운 검정통계량. 

p <- 2
mu <- c(5,10)
Sigma <- matrix(c(4,3,
                  3,6), nrow = 2)
n <- 100
X <- rmvnorm(n, mean= mu, sigma = Sigma) # A n x p normal data matrix 
Xbar <- colMeans(X)
S <- cov(X) 

# Compute Hoteling's T statistics (scaled appropriately to follow an F distribution)
T2 <- (n-p) / p * n / (n-1) *t(as.matrix(Xbar - mu)) %*% solve(S) %*% as.matrix(Xbar - mu)
Fprob <- df(T2,p,n-p) # and the tail probability of F-distribution
Fprob

# Comparison with a pre-packaged computation
library(ICSNP)
a <- HotellingsT2(X, mu = mu, test = "f") 
a$statistic
a$p.value #크게 다르지 않음.

