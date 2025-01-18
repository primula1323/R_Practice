# Multivariate analysis lab 
# Written by S. Jung
# Version date: Nov 13, 2019

## Comparisons of several populations and classification


# Example data: flea ------------------------------------------------------
library(dplyr)
library(GGally)  
?flea

# Fairly small dataset on flea beetles
ggpairs(flea)
ggscatmat(flea, columns = 2:4, color = "species")


# Comparisons of several populations  ----------------------------------------
# We compare the means of three difference populations.
# Assuming normality and the equality of covariance, MANOVA provides an answer.

# To test whether the covariances are indeed equal, one may use the likelihood-ratio
# to test the null hypothesis H0: Sigma_1 = Sigma_2 = ... = Sigma_g
# A bias-corrected version of the LRT is called Box's M test
library(MVTests)

test_varequal <- BoxM(flea[,2:7], group = flea$species) #공분산행렬이 그룹별로 서로 같은지에 대한 테스트.

# Exercises ---------------------------------------------------------------

# 1. Interpret the result of Box's M test. Can you assume equal covariance?

test_varequal$p.value


# 2. Box's M test assumes multivariate normality of each population. 
#    Is such an assumption valid? 
#    Can we assume normality for each population?  (See Lab #2)

# Now let us test whether means of these measurements differ by species.
# For this, explicitly compute the with-in and between covariances (W and B) 

n <- nrow(flea)
p <- ncol(flea) - 1
uniq.id <- unique(flea$species)


mean_vectors <- vector()
W <- matrix(0, nrow = p, ncol = p)


for (gr.id in uniq.id) {
  gData <- as.matrix( flea[flea$species == gr.id,2:7] ) 
  W <- W + t( scale( gData, scale = FALSE))  %*%  (  scale( gData, scale = FALSE) )
  mean_vectors <- cbind(mean_vectors,
                        colMeans(gData))
}
Sp <- W / (n - 3)
Tot <- cov(flea[,2:7]) * (n-1)
B <- Tot - W      

# Exercises ---------------------------------------------------------------

# 3. Manually compute the Between-variance matrix B from "mean_vectors". 
#    Check whether your result is equal to B = Tot - B above. (W를 B로부터 이론적으로 직접 계산하기.)

# In practice, we simply use the manova() function. The response variable is now 
# the collection of six variables, and a (perhaps non-intuitive) way of 
# indicating response variables is to make them as a matrix
 
out <- manova(as.matrix(flea[,2:7]) ~ species, data = flea)
out
summary(out) 

# Note that by default, Pillai's test statistic is used  
(pout <- summary(out, test = "Pillai"))
(sout <- summary(out, test = "Wilks"))
summary(out, test = "Hotelling-Lawley")
summary(out, test = "Roy")

#??? Wilks 말고 나머지 3개가 뭔가요? weeks 10-11의 나머지 3개 통계량에 대한 실습 pdf 참고.

# Now see if the B and W matrices are computed as expected
# Compare the following with B and W 
pout$SS
sout$SS
B
W


# eigenvalues of inv(W)*B
eigen( solve(W) %*% B)


# Explicitly compute Wilk's test statistic 
# |W|/|B+W| = prod {1 / (1+lambda)} 
prod(1 / (1 + sout$Eigenvalues))

# Wilk's test scales this value, then compares with an F distribution. 

# Exercises ---------------------------------------------------------------

# 4. How many positive eigenvalues should you see in the above? 


# 5. All of the MANOVA Tests above are not valid if normality is not assumed. 
#    For such cases, one may use permutation-based test. 
#    Inspect the following lines and report your conclusion. 

library(purrr)
library(modelr)
perms <- permute(flea,  100, species)
permuted_Wilk <- map(perms$perm,
              ~ summary(manova(as.matrix(flea[,2:7]) ~ species, data = .),test = "Wilks")$stats[3])
permuted_Wilk <- unlist(permuted_Wilk)
hist(permuted_Wilk)


