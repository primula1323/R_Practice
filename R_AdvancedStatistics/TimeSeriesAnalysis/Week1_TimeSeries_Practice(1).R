print("Hello, world!")

a <- 1+1
b = 3
c = a+b; print(c)

# Remove Objects
rm(a)
rm(list = ls())

# help
?rm
help(ls)


#=================
# Data Structure
#=================

### vector
a = c(1,2,3)                     # numeric vector
b = c("red", "white", "blue")    # character vector
c = c(TRUE, FALSE, F)            # logical vector

mode(a); mode(b); mode(c)       # storage mode
typeof(a); typeof(b); typeof(c) # low-level type
str(a); str(b); str(c)          # structure

a[1]; b[2]; c[3]
a[-1]
1:3



### factor
d = c("A", "B", "A", "C")
d.factor = factor(d); d.factor

factor(x = d, levels = c("B","A","C","D"))
factor(x = d, labels = c("First", "Second", "Third"))
factor(x = d, levels = c("B", "A", "C"),
       labels = c("First", "Second", "Third"))

mode(d.factor); typeof(d.factor); str(d.factor)



### data frame
testdata = data.frame(a,b,c) ; testdata
names(testdata) = c("ID", "Color", "Passed"); testdata

testdata[c(1,3),]
testdata[,1]
testdata$Color

mode(testdata); typeof(testdata); str(testdata)


### matrix
g = matrix(data = c(0:3), nrow = 2, ncol = 2); g   # square matrix
g1 = matrix(c(0:5), nrow = 2, ncol = 3); g1        # rectangular matrix
g2 = matrix(c(0:5), 3); g2

m1 = matrix(0:5, nrow = 3, ncol = 2, byrow = TRUE); m1

dim(g1); dim(g2)
g1 %*% g2    # matrix multiplication
g2 %*% g1
t(g1) * g2   # element-wise multiplication



### list
h = list(a, c, g); h
h[[1]]
h[[2]][1]
h[[3]][1,2]



#==============================
# R Basic Syntax and Functions
#==============================

seq(from = 1, to = 10, by = 2)
8:1
rep(x = 4, times = 3)
rep(x = c(1,2,3), times = 2)

abs(-c(1:3))
sqrt(c(4,9,16))
log(1:10)
round(x = pi, digits = 2)

factorial(5)
choose(n = 5, k = 2)

### rnorm
?rnorm
x = rnorm(n = 100, mean = 0, sd = 2)
mean(x); sd(x); var(x)
median(x)
summary(x)
stem(x)

### dnorm : get pdf value for normal distribution at ()
dnorm(x = 0)

### pnorm : get cdf value for normal distribution at ()
pnorm(q = 0)
pnorm(q = -1.96)

### qnorm : get quantile value for normal distribution at ()
qnorm(p = 0.5)
qnorm(p = 0.975)


### user-defined function
plus.minus = function(a, b){
  plus.ab = a+b
  minus.ab = a-b
  return(list("plus" = plus.ab,
              "minus" = minus.ab))
}

plus.minus(a = 1, b = 2)


### for
a = 0
b = c() 
for(i in 1:10){
  a = a+i
  b[i] = a
}
a;b



### repeat
a2 = 0
b2 = c()
i = 1
repeat{
  a2 = a2 + i
  b2[i] = a2
  i = i+1
  if(i > 10) break
}
a2; b2



### while
a3 = 0
b3 = c()
i = 1
while(i < 11){
  a3 = a3 + i
  b3[i] = i
  i = i+1
}
a3; b3


### logical operator
c(T, T, F) & c(T, F, F)
c(T, T, F) | c(T, F, F)
!c(T, F)



### comparison operator
a = c(1,2,3); b = c(2,2,2)
a==b
a!=b
a>=b
b<=a
(a>=b & a!=b)
(a>b | a==b)



### if, else
if(3>5){
  print("yes")
} else{
  print("no")
}

### else if
a = 11
if(a %% 3 == 0){
  print("remainder is 0")
} else if(a %% 3 == 1){
  print("remainder is 1")
} else{
  print("remainder is 2")
}



### ifelse
a.vec = 1:10
ifelse(test = a.vec %% 2, yes = "even", no = "odd")



### plot
e = rnorm(100); x = 1:100; y = 2*x + e
hist(e)
hist(e, breaks = 20)

boxplot(x = e, main = "box plot of e")

?lm()
regfit = lm(formula = y ~ x); regfit     # linear regression
plot(x, y, main = "scatter plot", col = "blue", pch = 1, cex = 0.8)
abline(regfit, lty = 2, lwd = 2, col = "red")

legend("topleft", legend = c("regression line", "data points"), 
       col = c("red", "blue"), lty = c(1,0), pch = c(-1, 2), cex = 0.8)




### getwd()
getwd()
setwd("./data")
dir()
setwd("..")

kospi = read.csv(file = "./data/kospi.csv", head = TRUE)
head(kospi)
tail(kospi, n = 10)
View(kospi)
fix(kospi)

kospi2 = kospi[, c(1,2)]
mean5 = c()
mean20 = c()
mean60 = c()
mean120 = c()

for(i in 1:dim(kospi2)[1]){
  if(i < 5){
    mean5[i] = NA
  } else{
    ind = (i-4):i
    mean5[i] = mean(x = kospi2[ind,2])
  }
  
  if(i < 20){
    mean20[i] = NA
  } else{
    ind = (i-19):i
    mean20[i] = mean(x = kospi2[ind,2])
  }
  
  if(i < 60){
    mean60[i] = NA
  } else{
    ind = (i-59):i
    mean60[i] = mean(x = kospi2[ind,2])
  }
  
  if(i < 120){
    mean120[i] = NA
  } else{
    ind = (i-119):i
    mean120[i] = mean(x = kospi2[ind,2])
  }
}

str(kospi2[,1])
str(as.Date(x = kospi2[,1], format = "%m/%d/%Y"))

day = as.Date(x = kospi2[,1], format = "%m/%d/%Y")

df = data.frame("Day" = day,
                "price" = kospi2[,2],
                "MA5" = mean5,
                "MA20" = mean20,
                "MA60"= mean60,
                "MA120" = mean120) 
library(ggplot2)

ggplot(data = df) +
  geom_line(aes(x = Day, y = price)) +
  geom_line(aes(x = Day, y = mean5), col = "green") +
  geom_line(aes(x = Day, y = mean20), col = "red") +
  geom_line(aes(x = Day, y = mean60), col = "orange") +
  geom_line(aes(x = Day, y = mean120), col = "purple") 




# library(lubridate)
library(ggplot2)

# read a csv file
kospi <- read.csv(file = file.choose())
head(kospi)

kospi2 = kospi[,1:2]
kospi2[,1] = as.Date(kospi2[,1], format = "%m/%d/%Y")

moving_average = function(data, window_size = 5){
  n = length(data)
  s = window_size
  
  #ma_vec = c(data[1:(s-1)])
  ma_vec = rep(NA,(s-1))
  for(i in s:n){
    x = data[(i-s+1):i]
    ma_vec[i] = mean(x)
  }
  return(ma_vec)
}

kospi_ma5 = moving_average(data = kospi2[,2], window_size = 5)
kospi_ma20 = moving_average(data = kospi2[,2], window_size = 20)
kospi_ma60 = moving_average(data = kospi2[,2], window_size = 60)
kospi_ma120 = moving_average(data = kospi2[,2], window_size = 120)

kospi3 = data.frame("Date" = kospi2[,1],
                    "price" = kospi2[,2],
                    "ma5" = kospi_ma5,
                    "ma20" = kospi_ma20,
                    "ma60" = kospi_ma60,
                    "ma120" = kospi_ma120)

kospi_df = reshape2::melt(data = kospi3, id = "Date")

plotted = ggplot(data = kospi_df) +
  geom_line(aes(x = Date, y = value, colour = variable)) +
  scale_color_manual(values = c("black", "green", "red", "orange", "purple"),
                     labels = c("price", "ma5", "ma20", "ma60", "ma120")) +
  labs(color = "Moving Average") +
  theme(legend.position =  c(0.07, 0.85),
        legend.margin = margin(0,0,0,0))

plotted 

plotted2 = plotted + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = "KOSPI Chart")

plotted2
