# Multivariate analysis lab 
# Written by S. Jung
# Version date: Oct 30, 2019

## Factor Analysis

# We will use example data from psych package. 
# epi.bfi dataset contains 13 measurements on personality
library(psych)
library(psychTools)
help(epi.bfi)

# 1. Exploration ----------------------------------------------------------
# Factor model exploits the correlations among variables. 
# Before fitting a factor model, it is a good idea to explore the correlation structure. 
# The psych package provides a couple of graphical devises for the purpose. 
pairs.panels(sat.act,pch='.')
cor.plot(epi.bfi,numbers=TRUE,main="Personality scores")


# Maximum Likelihood Factor Analysis by factanal --------------------------

# Base R distribution has the factanal function that only computes the MLE. 
fit <- factanal(epi.bfi, 1, rotation="none")
print(fit, digits=2, cutoff=.3, sort=FALSE) #무슨 의미...?
# Notice that the null hypothesis of 1-factor model is rejected. 
# Increase the number of factors (2-7) see how the p-value changes
k <- 5
fit <- factanal(epi.bfi, k, rotation="none")
fit$PVAL #보통 $로 원하는 것만 뽑아 사용.
# How many factors will you use? 

# Now set k = 6 and inspect the factor loadings
fit <- factanal(epi.bfi, 6, rotation="none")
print(fit, digits=2, cutoff=.3, sort=FALSE)

# Exercises ---------------------------------------------------------------

# 1. With p = 13, k = 6. Verify the degrees of freedom of the chi-square statistic is correct. 
p <- 13; k <- 6 ; df <- p*(p+1)/2 - p*k -p + k*(k-1)/2
df

# 2. What is the correlation between epiE and Factor1?
## Factor loadings are correlation coefficients between variables and factors.
## Thus there is strict, positive correlation between between epiE and Factor1.

#질문. 상당히 주관적. 결국 답변도 주관적일 수밖에 없음.

# 3. Verify that Factor1 is about Extraversion and Factor2 is about Depression and Anxiety. 
#    Continue interpreting Factors 3 to 6.
fit$loadings
## According to Loadings,
## Factor1 is related to epiE, epis, epiImp which are about Extraversion.
## Factor2 is related to bdi, traitanx, stateanx which are about Depression and Anxiety.
## Factor3 is related to bfagree which is about Agreeableness.
## Factor4 is related to epiImp which is about Impulsivity.
## Factor5 is related to epiNeur, bfneur, traitnax which are about Neuroticism and Anxiety.
## Factor6 is related to bfext, bfopen which are about Extraversion and Openness.

# 4. What is fit$correlation?
fit$correlation
## correlation between observable variables.

# Factor score prediction and varimax rotation ----------------------------
# For interpretability, let us resort to k = 2 factors 
# Score prediction by the regression modeling and by the WLS (option "Bartlett")
fit.reg <- factanal(epi.bfi, 2, rotation="none", scores = "regression")
fit.wls <- factanal(epi.bfi, 2, rotation="none", scores = "Bartlett") 

# Extra rotation by the varimax criterion
fit.reg.v <- factanal(epi.bfi, 2, rotation="varimax", scores = "regression")
fit.wls.v <- factanal(epi.bfi, 2, rotation="varimax", scores = "Bartlett") 

# Compare the unrotated and rotated loadings 
fit.reg$loadings
fit.reg.v$loadings
# Almost identical in this case 

# Compare the score predictions 
par(mfrow = c(2,2))
plot(fit.reg$scores, main = "regression")
plot(fit.wls$scores, main = "WLS")
plot(fit.reg.v$scores, main = "regression (Varimax)")
plot(fit.wls.v$scores, main = "WLS (Varimax)")
# Predictions from WLS and regression are almost identical in this case 


# Factor analysis using psych package -------------------------------------
fa.out <- fa(epi.bfi, nfactors = 3, fm = "ml", rotate = "varimax", scores = "regression")
# Try help(fa) and inspect various options for the fitting methods, rotation criteria, and scoring methods.
help(fa)
# We have discussed the maximum likelihood estimate ("ml") and the principal factor method ("pa")
# fa.out <- fa(epi.bfi, nfactors = 3, fm = "pa", rotate = "varimax", scores = "regression")
# The pricipal component estimates of factor models are given by function "principal". 
# fa.out <- principal(epi.bfi, nfactors = 3)
# Notice that this package contains  more options than we have discussed. 

print(fa.out,digits=2, cut=.3)

# the output displays 
# 1) factor loadings for each variable, with communality (h2) and uniqueness (u2) 
# 2) com is a measure of complexity; Larger "complexity" occurs when the variable is related to many factors
# 3) The result (p-value) of likelihood ratio test is at the line with "Likelihood Chi Square"
fa.out$communality
fa.out$uniquenesses
fa.out$loadings
fa.out$scores 
fa.out$PVAL

# A useful diagram indicating the structure of latent variables
par(mfrow = c(1,1))
fa.diagram(fa.out)


# Exercises ---------------------------------------------------------------
# Take the Olympic Decathlon data at ade4, which contains the performances 
# of 33 men's decathlon at the Olympic Games (1988).
library(ade4)
data(olympic)
help(olympic)

# We will use the portion of data at "tab"
data = olympic$tab

# Create a scree plot using psych::scree
scree(olympic$tab)


# 5. How many factors will you try first?
## two

# 6. Use the principal component estimate to fit a factor model. Use varimax rotation. 
fa.pa.out <- fa(data, nfactors =2, fm = "pa", rotate = "varimax", scores = "regression")
print(fa.pa.out, digits=2, cutoff=.3, sort=FALSE)

# 7. Plot the scatters of factor scores and inspect whether there is an outlier. 
pairs.panels(fa.pa.out$scores, pch= '.')
## There is no outlier.

# 8. Use the maximum likelihood estimate to fit a factor model, using varimax rotation. 
#    Are the results from PC estimates similar to those from ML estimates? 
#    Use a graphical method to present the structure and answer: Are the factors interpretable?
fa.ml.out <- fa(data, nfactors =2, fm = "ml", rotate = "varimax", scores = "regression")
print(fa.ml.out, digits=2, cutoff=.3, sort=FALSE)

fa.pa.out$loadings; fa.ml.out$loadings
par(mfrow = c(1,2))
fa.diagram(fa.pa.out)
fa.diagram(fa.ml.out)
## The results from PC estimates and those from ML estimates are almost identical.
## The frist factor is about speed and the second factor is about power.

# 9. Try other numbers of factors.
fa3.out <- fa(data, nfactors = 3, fm = "pa", rotate = "varimax", scores = "regression")
print(fa3.out, digits=2, cutoff=.3, sort=FALSE)
par(mfrow = c(1,1))
fa.diagram(fa3.out)

# 10. Inspect the relation between factor scores and olympic$score. 
par(mfrow = c(1,2))
plot(olympic$score, fa.ml.out$scores[1:33], ylab = 'ML2')
plot(olympic$score, fa.ml.out$scores[34:66], ylab = 'ML1')

cor(olympic$score, fa.ml.out$scores)
## Olympic$score and the first factor score have a negative correlation.
## Olympic$score and the second factor score have a positive correlation.


#결과는 not unique -> 