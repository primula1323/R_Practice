name_pkg <- c("survey", "sampling", "SDAResources")
name_pkg <- unique(name_pkg)
bool_nopkg <- !name_pkg %in% rownames(installed.packages())
if (sum(bool_nopkg) > 0) {
  install.packages(name_pkg[bool_nopkg])
}
invisible(lapply(name_pkg, library, character.only = T)) # load multiple packages


########## Selecting a Sample with Unequal Probabilities ##########

##### Example 6.2

data(classes)
classes[1:2,]
N<-nrow(classes)
set.seed(78065)
# select 5 classes with probability proportional to class size and with replacement
sample_units<-sample(1:N,5,replace=TRUE,prob=classes$class_size)
sample_units
mysample<-classes[sample_units,]  
mysample  
# calculate ExpectedHits and sampling weights
mysample$ExpectedHits<-5*mysample$class_size/sum(classes$class_size)
mysample$SamplingWeight<-1/mysample$ExpectedHits
mysample$psuid<-row.names(mysample)
mysample
# check sum of sampling weights
sum(mysample$SamplingWeight)  

# sampling without replacement
set.seed(330582)
cluster(data=classes, clustername=c("class"), size=5, method="systematic",
        pik=classes$class_size,description=TRUE)

########## Selecting a Two-Stage Cluster Sample ##########

##### Example 6.11

# create data frame classeslong
data(classes)
classeslong<-classes[rep(1:nrow(classes),times=classes$class_size),]
classeslong$studentid <- sequence(classes$class_size) 
nrow(classeslong)
table(classeslong$class) # check class sizes
head(classeslong)

# select a two-stage cluster sample, psu: class, ssu: studentid 
# number of psus selected: n = 5 (pps systematic)
# number of students selected: m_i = 4 (srs without replacement)
# problist<-list(classes$class_size/647) # same results as next command
problist<-list(classes$class_size/647,4/classeslong$class_size) #selection prob
problist[[1]]  # extract the first object in the list. This is pps, size M_i/M
problist[[2]][1:5] # first 5 values in second object in list
# number of psus and ssus
n<-5
numberselect<-list(n,rep(4,n))
numberselect
# two-stage sampling
set.seed(75745)
tempid<-mstage(classeslong,stage=list("cluster","stratified"), 
               varnames=list("class","studentid"),
               size=numberselect, method=list("systematic","srswor"),pik=problist)

# get data
sample1<-getdata(classeslong,tempid)[[1]]
# sample 1 contains the ssus of the 5 psus chosen at the first stage
# Prob_ 1 _stage has the first-stage selection probabilities
head(sample1) 
nrow(sample1)
table(sample1$class) # lists the psus selected in the first stage
sample2<-getdata(classeslong,tempid)[[2]]
# sample 2 contains the final sample
# Prob_ 2 _stage has the second-stage selection probabilities
# Prob has the final selection probabilities
head(sample2)  
nrow(sample2) # sample of 20 ssus altogether
table(sample2$class) # 4 ssus selected from each psu
# calculate final weight = 1/Prob
sample2$finalweight<-1/sample2$Prob
# check that sum of final sampling weights equals population size
sum(sample2$finalweight)
sample2[,c(1,2,3,6,7)] # print variables from final sample


########## Computing Estimates from an Unequal-Probability Sample ##########

##### Example 6.4

studystat <- data.frame(class = c(12, 141, 142, 5, 1),
                        Mi = c(24, 100, 100, 76, 44), 
                        tothours=c(75,203,203,191,168))
studystat$wt<-647/(studystat$Mi*5)
sum(studystat$wt) # check weight sum, which estimates N=15 psus
# design for with-replacement sample, no fpc argument
d0604 <- svydesign(id = ~1, weights=~wt, data = studystat)
d0604
# Ratio estimation using Mi as auxiliary variable
ratio0604<-svyratio(~tothours, ~Mi,design = d0604)
ratio0604
confint(ratio0604, level=.95,df=4)
# Can also estimate total hours studied for all students in population
svytotal(~tothours,d0604)

##### Example 6.6

students <- data.frame(class = rep(studystat$class,each=5),
                       popMi = rep(studystat$Mi,each=5), 
                       sampmi=rep(5,25),
                       hours=c(2,3,2.5,3,1.5,2.5,2,3,0,0.5,3,0.5,1.5,2,3,1,2.5,3,5,2.5,4,4.5,3,2,5))
# The 'with' function allows us to calculate using variables from a data frame
# without having to type the data frame name for all of them
students$studentwt <- with(students,(647/(popMi*5)) * (popMi/sampmi))
# check the sum of the weights
sum(students$studentwt)
# create the design object
d0606 <- svydesign(id = ~class, weights=~studentwt, data = students)
d0606
# estimate mean and SE
svymean(~hours,d0606)
degf(d0606) 
confint(svymean(~hours,d0606),level=.95,df=4) #use t-approximation

# estimate total and SE
svytotal(~hours,d0606)
confint(svytotal(~hours,d0606),level=.95,df=4)