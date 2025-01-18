name_pkg <- c("survey", "foreign")
name_pkg <- unique(name_pkg)

bool_nopkg <- !name_pkg %in% rownames(installed.packages())
if (sum(bool_nopkg) > 0) {
  install.packages(name_pkg[bool_nopkg])
}
invisible(lapply(name_pkg, library, character.only = T))


?api
data(api)
sum(apisrs$stype == 'E'); sum(apisrs$stype == 'M'); sum(apisrs$stype == 'H')
sum(apistrat$stype == 'E'); sum(apistrat$stype == 'M'); sum(apistrat$stype == 'H')


srs_design <- svydesign(id=~1, fpc=~fpc, data=apisrs) #fpc : finite population correction
srs_design

svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)

summary(apisrs$fpc)

N = 6194; n = 200; s2 = var(apisrs$enroll)
se = sqrt((s2 / n) * ((N-n) / N)) # Note that N instead of N-1
se


nofpc <- svydesign(id=~1, weights=~pw, data=apisrs)
nofpc

svytotal(~enroll, nofpc)
svymean(~enroll, nofpc)

summary(apisrs$pw)

se_nofpc = sqrt(s2 / n)
se_nofpc


means <- svymean(~api00+api99, srs_design)
means

# The next two lines give the same result.

svycontrast(means, c(api00=1, api99=-1))
svycontrast(means, quote(api00-api99))

srs_design <- update(srs_design, apidiff=api00-api99)
srs_design <- update(srs_design, apidct=apidiff/api99)
svymean(~apidiff+apidct, srs_design)


strat_design <- svydesign(id=~1, strata=~stype, fpc=~fpc, data=apistrat)
strat_design

svytotal(~enroll, strat_design)
svymean(~enroll, strat_design)

summary(as.factor(apistrat$fpc))
svytotal(~stype, strat_design)


summary(apistrat[apistrat$stype == 'E',]$pw)
summary(apistrat[apistrat$stype == 'M',]$pw)
summary(apistrat[apistrat$stype == 'H',]$pw)

chis_adult <- read.dta('adult.dta')
summary(chis_adult$rakedw0)


strat_design_2 <- svydesign(id=~1, strata=~stype, weights=~pw, data=apistrat)
# BRR is not applicable to sampling design with fpc applied.

boot_design <- as.svrepdesign(strat_design, type='bootstrap', replicates=100)
BRR_design <- as.svrepdesign(strat_design_2, type='BRR')

boot_design
BRR_design

svymean(~enroll, strat_design)
svymean(~enroll, boot_design)
svymean(~enroll, BRR_design)


chis <- svrepdesign(variables=chis_adult[,1:418],
                    repweights=chis_adult[,420:499],
                    weights=chis_adult[,419], combined.weights=TRUE,
                    type='other', scale=1, rscales=1)


emerg_high <- subset(strat_design, emer>20)
emerg_low <- subset(strat_design, emer==0)

svymean(~api00+api99, emerg_high)
svymean(~api00+api99, emerg_low)

svytotal(~enroll, emerg_high)
svytotal(~enroll, emerg_low)


bys <- svyby(~bmi_p, ~srsex+racehpr, svymean, design=chis, keep.names=FALSE)
print(bys, digits=3)