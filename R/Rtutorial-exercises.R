#### command reference for:
#### Chris Chapman, Introduction to R (short course)
#### Copyright (c) 2013, Google, Inc.

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#### NOTES
#### This file should match the tutorial PDF but may get slightly out of sync
####
#### Commands must execute in sequence; there is some serial dependency.
####   "Rtutorial-storesales.Rdata" data is required from separate file.
####   "Rcbc.R" file is required for optional choice-based conjoint portion.

# Tutorial starts here!

#### R AS A CALCULATOR
123+456
exp(2)
10^6

x <- 5
x = 6

pi*x^2


#### vectors
1:10
5:8
x <- 1:10
x
y <- pi*x^2
y

seq(from=11, to=101, by=2)
?seq
sum(x)
mean(y)
var(y)
seq(0, 1, length.out = 11)
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
seq(17) # same as 1:17, or even better seq_len(17)
?sum
?"+"
??bayesm

#### matrices
xy <- cbind(x, y)
xy
(xy <- cbind(x, y))
(yx <- t(xy))

# matrix math
# with single elements
xy - 10   # elements are recycled
xy - 1:10 # watch out for the order!
# between matrices
xy + xy 
xy * yx   # doesn't work the same way!
xy %*% yx
xy * xy   # very different
# matrix functions
det(xy %*% yx)  # essentially zero. why? (hint: think volume)
eigen(xy %*% yx)

#### inspecting objects
ls()

x
length(x)
dim(xy)    # defined above; checking before redefinition

x <- 1:10000; y <- pi*x^2; xy <- cbind(x, y) # make xy large
ls()
dim(xy)
head(xy,10)
tail(xy)
str(xy)


#### indexing part 1
# elements
x[4]
y[4]
xy[3141, 2]     # key concept:  [ ROW, COLUMN ]

# dimensions
xy[4, ]
xy[, 2]  # oops too many!
tail(xy[, 2])

# ranges
x[4:6]
y[c(1,3,7,5,3,1,1)]
xy[11:20, ]

# index by vector
(z <- seq(from=1, to=100, by=13)^2)
xy[z, ]

# index by array
(zz <- cbind(z, c(1,2,2,1,1,1,2,2)))
xy[zz]

# negative indexing
z[-4]
xy[z[-4],1]

# using that to remove a bad data point
dim(xy)
xy <- xy[-1:-3,]
dim(xy)
1000 * 24 * 5280 * 12 * 2.54
xy[50:70, ]


#### data frames
str(xy)
class(xy)
xy.df <- data.frame(xy)
str(xy.df)
head(xy.df)
head(xy)

#### indexing part 2: data frames
xy.df <- data.frame(xy[1:20, ])  # make it easier to work with!
str(xy.df)

names(xy)
colnames(xy)
names(xy.df)
names(xy.df) <- c("Store", "Sales")
data.frame(matrix(rbind(1:5)))
data.frame(cbind(1:5)%*%rbind(1:5))
xy.df[, 2]
xy.df$Store
xy.df[, "Sales"]

# add rows
xy.df <- rbind(xy.df, xy.df[1:10, ])
dim(xy.df)
# add cols
xy.df <- cbind(xy.df, xy.df[ , 2])
head(xy.df)

# add variables

names(xy.df)[3] <- "Sales2"
xy.df$random <- rnorm(nrow(xy.df))
head(xy.df)

#### boolean indexing
xy.df[xy.df$Store==10, ]
xy.df$Store == 10
ind <- xy.df$Store == 10
ind
xy.df[ind, ]

xy.df[xy.df$Sales < 100, ]
xy.df[xy.df$Sales > 100 & xy.df$Sales < 500, ]
xy.df[xy.df$Sales < 50 | xy.df$Sales > 1000, ]

subset(xy.df, Sales < 50 | Sales > 1000)

subset( xy.df, (Sales<50) | (Sales>1000) )

low <- 50; high <- 1000
new.xydf <- subset( xy.df, (Sales<low) | (Sales>high) )
new.xydf$Sales

############################
#### pop quiz revisited ####
############################
xy[50:70, ]
head(xy[ xy[,"y"] > 10000,])
which(xy[,"y"]>10000)
which(xy[,"y"]>10000)[1]
ind<-which(xy[,"y"]>10000)[1:5] #first 5 items greater than 10000
xy[ind,] #comment here

#### functions briefly
my <- function(x) {
  print("Hi, Mom!")
  print(x)
}
pi
my(pi)

my(xy[1:5, "y"])
my
data.frame

# functions are objects too
my
my(my)
my(my(my))

my2 <- function(x) 
  {

  return(x^2)
}

my2(2:4)
yy <- my2(2:4)
yy

# clean up!
ls()
rm(x)
rm(x, y, z, xy, xy.df, new.xydf, ind, low, high)  # warning about x, but not error
ls()
rm(my, my2, yy, yx, zz)

ls(all.names=TRUE)


#### Break!

#### READ AND WRITE
## CSVs
my.object <- read.csv("filename.csv")
?read.csv
my.object <- read.csv("filename.csv", 
    stringsAsFactors=FALSE, header=TRUE, row.names=1)
?read.csv
head(my.object)
str(my.object)

write.csv(my.object, "filename.csv")

## load and save
hi <- 'Hello, world!'
hi
save(hi, file="hello.Rdata")

hi <- 'Make me a sandwich!'
hi
load("hello.Rdata") # silently overwrites objects!
hi

## sessions
save.image("mywork.Rdata")
ls()
rm(list=ls())  # caution: deletes all objects!
ls()
load("mywork.Rdata")
ls()


#### PRELIMINARIES TO DATA ANALYSIS

# install some tools we'll use
install.packages(c("car", "digest", "psych", "MCMCpack", "xtable", "bayesm", "ChoiceModelR"))

# read the store sales data we'll use throughout the class
current.wd <- "~/Documents/Chris Documents/papers/General/R tutorial/handouts/Chapman R tutorial handouts"
setwd(current.wd)   # <== CHANGE FOR YOUR SYSTEM!

load("Rtutorial-storesales.Rdata")
head(store.sales)
summary(store.sales)
store.sales$storeId <- factor(store.sales$storeId)
store.sales$talkerYN <- factor(store.sales$talkerYN)

library(car)
some(store.sales)


#### PLOTTING
# histogram
# frequency of price points in store sales data
hist(store.sales$price)
hist(store.sales$visitors)
hist(store.sales$visitors, breaks=50)
hist(store.sales$visitors, breaks=50, 
     prob=TRUE)

# density
plot(density(store.sales$visitors))
plot(density(store.sales$visitors, bw=0.5))
plot(density(store.sales$visitors, bw=3))

# histogram + density
hist(store.sales$visitors, breaks=50, prob=TRUE)
# plot(density(store.sales$visitors, bw=3))
lines(density(store.sales$visitors, bw=3), col="blue")

# boxplot
boxplot(store.sales[store.sales$storeId==5, "visitors"])


# boxplot with factors
boxplot(store.sales[store.sales$storeId, "visitors"] ~ 
        store.sales$storeId[store.sales$storeId])

boxplot(store.sales[store.sales$store<=20, "visitors"] ~ 
        store.sales$store[store.sales$store<=20], 
        horizontal=TRUE, xlab="Visitors", ylab="Store")

# dotchart
dotchart(store.sales[store.sales$store==1, "visitors"])
with(store.sales[store.sales$week==52,], dotchart(visitors, labels=storeId))
with(store.sales[store.sales$week==52,], 
     dotchart(visitors, labels=paste("Store",storeId)))

# add some labels
with(store.sales[store.sales$week==52,], 
    dotchart(visitors, 
        labels=paste("Store",storeId),
        main="Visitors per Store in Week 52", 
        xlab="Visitors"
    )
)


# scatter plot
plot(ourBottles ~ visitors, data=store.sales)
plot(jitter(ourBottles) ~ jitter(visitors), data=store.sales)
abline(lm(ourBottles ~ visitors, data=store.sales))

with(store.sales[store.sales$talkerYN==1,], 
    abline(lm(ourBottles ~ visitors), col="Blue"))
with(store.sales[store.sales$talkerYN==0,], 
    abline(lm(ourBottles ~ visitors), col="Red"))

# some exploration and reshaping
with(store.sales, plot(ourBottles ~ lbsCheese))
with(store.sales, plot(jitter(ourBottles) ~ lbsCheese))
with(store.sales, plot(jitter(ourBottles) ~ sqrt(lbsCheese)))
with(store.sales, plot(log(jitter(ourBottles)) ~ sqrt(lbsCheese)))

with(store.sales, cor.test(ourBottles, lbsCheese))
with(store.sales, cor.test(log(ourBottles), sqrt(lbsCheese)))

# scatter plot matrix
pairs(~ ourBottles + theirBottles + lbsCheese + talkerYN + visitors, 
    data=store.sales)
pairs(~ourBottles + theirBottles + lbsCheese + talkerYN + visitors, 
    data=subset(store.sales, storeId==1))

require(car)
scatterplotMatrix(~ourBottles + theirBottles + lbsCheese + talkerYN + visitors, 
    data=subset(store.sales, storeId==1))


#### CBC in R

# make it repeatable since various parts are pseudorandom
set.seed(4567)
# define CBC with five attributes, with 3-5 levels each
attr.list   <- c(3, 3, 5, 5, 4)

# Friendly names for attributes	
  attr.names  <- c("Size", "Performance", "Design", "Memory", "Price")
  attr.labels <- c(   
      "Nano",  "Thumb",  "Full-length",
      "Low speed",  "Medium speed", "High speed",
      "Tie-dye",  "Silver",  "Black",  "White",  "Prada",
      "1.0GB",  "8.0GB",  "16GB",  "32GB",  "256GB",
      "$9",  	"$29",  "$59",   "$89"
  )

# generate the design matrix
tmp.tab <- generateMNLrandomTab(attr.list,
            respondents=3, cards=3, trials=12) 
head(tmp.tab)
tmp.des <- convertSSItoDesign(tmp.tab)
head(tmp.des)

#  Survey presentation : "Fielding" to a spreadsheet
current.wd <- "~/Desktop/"  
writeCBCdesignCSV(tmp.tab, attr.list=attr.list, 
  lab.attrs=attr.names, lab.levels=attr.labels, 
  filename=paste(current.wd,"writeCBCtest.csv",sep=""), 
  delimeter=",")

# Importing from a local spreadsheet:
tmp.win <- readCBCchoices(tmp.tab, filename=paste(current.wd, "writeCBCtest.csv", sep=""))

# Importing from Google Docs:
# Download it as CSV first, and then:
current.wd <- "~/Downloads/"
tmp.win <- readCBCchoices(tmp.tab, 
  filename=paste(current.wd, "writeCBCtest - Sheet 1.csv",
  sep=""))

# clean up Survey data in R
(tmp.win.exp <- expandCBCwinners(tmp.win))

# Aggregate MNL Estimation for rectangular CBC design using Rcbc
tmp.pws <- estimateMNLfromDesign(tmp.des, tmp.win)
(tmp.res <- data.frame(attr=attr.labels, util=t(tmp.pws)[,1]))  # nicer formatting to import to a spreadsheet

# Preference simulation
tmp.prod1 <- c(1, 5, 10, 14, 17)   # product 1
# attr.labels[tmp.prod1]
tmp.prod2 <- c(2, 6, 11, 15, 20)   # product 2
# attr.labels[tmp.prod2]
tmp.pref  <- marketSim(
    tmp.ind.pws,   # individual-level utilities
    list(tmp.prod1, tmp.prod2) ) # products
colMeans(tmp.pref)   # average preference


##################
# HB Models with individual-level estimation for CBC
# CBCgolfexercise
# importing Sawtooth Software SSI/Web files
# estimating with ChoiceModelR

current.wd <- "~/Documents/Chris Documents/papers/General/R tutorial/handouts/Chapman R tutorial handouts"

tmp.raw.all <- read.csv(paste0(current.wd,"/CBCgolfexercise.csv"))
head(tmp.raw.all, 20)
table(tmp.raw.all$sys_RespNum)
summary(as.numeric(table(tmp.raw.all$sys_RespNum)))

# remove every 4th row (none specification) as out of scope
tmp.cutrows <- seq(from=4, to=nrow(tmp.raw.all), by=4)
tmp.raw.all <- tmp.raw.all[-tmp.cutrows, ]
str(tmp.raw.all)

tmp.tab <- tmp.raw.all[, 4:6]  # design columns
head(tmp.tab)
str(tmp.tab)

tmp.win <- tmp.raw.all[, 7] # response
summary(tmp.win)

tmp.logitHB <- estimateMNLfromDesignHB(tmp.tab, tmp.win, kCards=3, kTrials=17, kResp=100)

# get individual-level betas
(tmp.attrs <- findSSIattrs(tmp.tab)) # infer the CBC structure
tmp.HBindbetas <- extractHBbetas(tmp.logitHB, tmp.attrs)
head(tmp.HBindbetas)

# create products for market simulation
golf1 <- c(1, 5, 8)  # "1", "1", "1"
golf2 <- c(2, 7, 11) # "2", "3", "4"

tmp.sim1 <- marketSim(tmp.HBindbetas, list(golf1, golf2))
head(tmp.sim1)
colMeans(tmp.sim1)

# if "use.error" or using HB draws, probably want to repeat
tmp.sim2 <- marketSim(tmp.HBindbetas, list(golf1, golf2), style="first", draws=1000, use.error=TRUE)
head(tmp.sim2)
colMeans(tmp.sim2)



##################
#### Overview of stats
# based on ?swiss
summary(swiss)
fert.mod1 <- lm(Fertility ~ Agriculture + Education + Catholic , data = swiss)
summary(fert.mod1)


#### STATISTICS MODELS

# distribution
table(store.sales$ourBottles)
mean(store.sales$ourBottles)
median(store.sales$ourBottles)

summary(store.sales)
summary(store.sales$income)

require(psych)
describe(store.sales)

quantile(store.sales$income)
quantile(store.sales$income, pr=0.80)
quantile(store.sales$income, pr=c(0, 0.20, 0.5, 0.80, 0.98))

inc.cdf <- ecdf(store.sales$income)
inc.cdf
plot(inc.cdf)

inc.cdf(70000)
inc.cdf(seq(from=30000, to=80000, by=5000))


# bivariate
cor(store.sales$income, store.sales$ourBottles)   # coefficient
cor.test(store.sales$income, store.sales$ourBottles)
cor(store.sales)

t.test(1:20, 5:18)

t.test(1:20, jitter(3:22))
t.test(1:20, jitter(3:22), paired=TRUE) 

with(store.sales,
  t.test(ourBottles[storeId==1], ourBottles[storeId==36])
)


# first a bit of data cleanup!
summary(store.sales)

store.sales$storeId <- as.factor(store.sales$storeId)

(tmp.date <- (store.sales$week-1) * 7)
(tmp.date <- as.Date(tmp.date, origin="2012-01-02"))

store.sales$date <- tmp.date
table(store.sales$date)
summary(store.sales)
head(store.sales)
some(store.sales)


#### Apply and aggregate

xy <- matrix(1:100, ncol=4, byrow=TRUE)
head(xy)

colMeans(xy)
rowMeans(xy)

apply(xy, 1, mean)
apply(xy, 2, mean)

apply(xy, 1, summary)
apply(xy, 2, summary)

apply(xy, 2, function(x) { log(summary(x)) }) # anonymous fn

xy.log <- apply(xy, 2, function(x) { log(summary(x)) }) # results are a matrix
xy.log

xy.log[5,] - xy.log[2,]    # interquartile range


# aggregate

aggregate(ourBottles ~ price + talkerYN, data=store.sales, mean)
bot.ag <- aggregate(ourBottles ~ price + talkerYN, data=store.sales, mean)

head(bot.ag)
bot.ag[bot.ag$price==12.99 & bot.ag$talkerYN==1, 3]

xtabs(ourBottles ~ ., data=bot.ag)
bot.xtab <- xtabs(ourBottles ~ ., data=bot.ag)
bot.xtab

library(xtable)
xtable(bot.xtab)

rm(xy, xy.log, bot.ag, bot.xtab, hi, inc.cdf, my.object, tmp.date)
ls()


#### linear regression
mod1 <- lm(ourBottles ~ storeId + visitors, data=store.sales)
summary(mod1)
anova(mod1)

mod2 <- update(mod1, . ~ . + lbsCheese)
summary(mod2)

BIC(mod1, mod2)

mod3 <- step(lm(ourBottles ~ ., data=store.sales))

## poisson models
summary(glm(ourBottles~., data=store.sales, family=poisson))

# which is better? ... a priori choice!


#### pop quiz 2
# add log of price
store.sales$logprice <- log(store.sales$price)
# does price or logprice work better in the model?

mod.p1 <- lm(ourBottles ~ price + lbsCheese + talkerYN, data=store.sales)
mod.p2 <- lm(ourBottles ~ logprice + lbsCheese + talkerYN, data=store.sales)

summary(mod.p1)
summary(mod.p2)
BIC(mod.p1, mod.p2)
anova(mod.p1, mod.p2)


# is the model good?  diagnostic plots
plot(mod3)

# what's going on?  is there something in one of the other variables?
with (store.sales, plot(visitors, ourBottles))
with (store.sales, plot(week, ourBottles))

acf(residuals(mod3))   # correlated error structure
durbinWatsonTest(mod3, max.lag=12)

library(nlme)          # so let's fit a correlated-error model
mod4 <- gls(ourBottles ~ storeId + lbsCheese + talkerYN + price + 
        theirBottles, 
        correlation=corARMA(p=2) , 
        data=subset(store.sales, as.numeric(as.character(storeId)) < 6))

plot(mod4)
summary(mod4)
summary(mod3)


#### Bayesian regression

library(MCMCpack)
mod.b1 <- MCMCregress(ourBottles ~ visitors, 
    data=store.sales, burnin=10000, mcmc=10000, verbose=5000, 
    b0=0, B0=0.1, marginal.likelihood="Chib95")
plot(mod.b1)
summary(mod.b1)

mod.b2 <- MCMCregress(ourBottles ~ visitors + lbsCheese, 
    data=store.sales, burnin=10000, mcmc=10000, verbose=5000, 
    b0=0, B0=0.1, marginal.likelihood="Chib95")
plot(mod.b2)
summary(mod.b2)

BF.12 <- BayesFactor(mod.b1, mod.b2)
BF.12

PostProbMod(BF.12)

mod.b3 <- MCMCregress(ourBottles ~ talkerYN + price, 
    data=store.sales, burnin=10000, mcmc=10000, verbose=5000, 
    b0=0, B0=0.1, marginal.likelihood="Chib95")
    
BF.123 <- BayesFactor(mod.b1, mod.b2, mod.b3)
summary(BF.123)
PostProbMod(BF.123)


