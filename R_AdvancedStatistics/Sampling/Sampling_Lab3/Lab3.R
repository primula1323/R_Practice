name_pkg <- c("survey", "sampling", "SDAResources")
name_pkg <- unique(name_pkg)
bool_nopkg <- !name_pkg %in% rownames(installed.packages())
if (sum(bool_nopkg) > 0) {
  install.packages(name_pkg[bool_nopkg])
}
invisible(lapply(name_pkg, library, character.only = T)) # load multiple packages


########## Ratio Estimation ##########

##### Examples 4.2 and 4.3

data(agsrs)
n<-nrow(agsrs) #300
agsrs$sampwt <- rep(3078/n,n)
agdsrs <- svydesign(id = ~1, weights=~sampwt, fpc=rep(3078,300), data = agsrs)
agdsrs
# correlatIon of acres87 and acres92
cor(agsrs$acres87,agsrs$acres92) 
# estimate the ratio acres92/acres87
sratio<-svyratio(numerator = ~acres92, denominator = ~acres87,design = agdsrs)
sratio
confint(sratio, df=degf(agdsrs))

# provide the population total of x
xpoptotal <- 964470625
# Ratio estimate of population total
predict(sratio,total=xpoptotal)
# Ratio estimate of population mean
predict(sratio,total=xpoptotal/3078)

# draw the scatterplot
par(las=1) # make tick mark labels horizontal (optional)
plot(x=agsrs$acres87/1e6,y=agsrs$acres92/1e6, 
     xlab="Millions of Acres Devoted to Farms (1987)",
     ylab = "Millions of Acres Devoted to Farms (1992)",
     main = "Acres Devoted to Farms in 1987 and 1992")
# draw line through origin with slope Bhat
abline(0,coef(sratio))

##### Example 4.5

# scatterplot and correlation of seed92 and seed94
data(santacruz)
plot(santacruz$seed92,santacruz$seed94,
     main="Number of seedlings in 1994 and 1992",
     xlab="Number of seedlings in 1992",ylab="Number of seedlings in 1994")
cor(santacruz$seed92,santacruz$seed94)

nrow(santacruz) #10
santacruz$sampwt <- rep(1,nrow(santacruz)) 
design0405 <- svydesign(ids = ~1, weights = ~sampwt, data = santacruz)
design0405
#Ratio estimation using number of seedlings of 1992 as auxiliary variable
sratio3<-svyratio(~seed94, ~seed92,design = design0405)
sratio3
confint(sratio3, df=10-1)

########## Regression Estimation ##########

##### Example 4.7

data(deadtrees)
head(deadtrees)
nrow(deadtrees) # 25
# Fit with survey regression
dtree<- svydesign(id = ~1, weight=rep(4,25), fpc=rep(100,25), data = deadtrees)
myfit1 <- svyglm(field~photo, design=dtree) 
summary(myfit1) # displays regression coefficients
confint(myfit1,df=23) # df = 25-2
# Regression estimate of population mean field trees
newdata <- data.frame(photo=11.3)
predict(myfit1, newdata)
confint(predict(myfit1, newdata),df=23)
# Estimate total field tree, add population size in total= argument
newdata2 <- data.frame(photo=1130)
predict(myfit1, newdata2, total=100)  
confint(predict(myfit1, newdata2,total=100),df=23)


########## Ratio Estimation with Stratified Sampling ##########

##### Combined ratio estimator

data(agstrat)
popsize_recode <- c('NC' = 1054, 'NE' = 220, 'S' = 1382, 'W' = 422)
agstrat$popsize <- popsize_recode[agstrat$region]
# input design information for agstrat
dstr <- svydesign(id = ~1, strata = ~region, fpc = ~popsize, weight = ~strwt, 
                  data = agstrat) 
# now compute the combined estimator of the ratio
combined<-svyratio(~ acres92,~acres87,design = dstr)
combined
# we can get the combined ratio estimator of the population total 
# with the predict function
predict(combined,total=964470625)

##### Separate ratio estimator

separate<-svyratio(~acres92,~acres87,design = dstr,separate=TRUE)
separate
#  Define the stratum totals for acres87 as a list:
stratum.xtotals <- list(NC=350474227,NE=22033421,S=280631939,W=311331038)
predict(separate,stratum.xtotals)