#This is Terry's learing R -Scrip 
#########10##################30########40########50########60########70######80
##############################################################################
# To understand computations in R, two slogans are helpful: 1. Everything that 
#  exists is an object. 2. Everything that
# | happens is a function call.
###############################################################################
##########   see http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml  
###############################################################################
# starting a new analysis: set working directory, read in a file and explore the file...

## removing all objects from a workspace
rm(list = ls())
###############################################################################
##  How to set a working directory when the file path is longer than 80 
##  characters and you want to stay in 80 wide or less?
##
setwd("S:/1 PROJECTS/Shopper Research/Pricing/Price Elasticity/Rumhaven PE 2016/data/modeling data and syntax")
setwd(paste("S:/1 PROJECTS/Shopper Research/Pricing/Price Elasticity/Rumhaven",
            "PE 2016/data/modeling data and syntax", sep=""))
# or
file <- file.path("S:/1 PROJECTS/Shopper Research/Pricing/Price Elasticity/",
                  "Rumhaven PE 2016/data/modeling data and syntax")
setwd(file)

setwd("S:/1 PROJECTS/Shopper Research/Test Market/Intuition 2016 test market/Data/final analysis")




# Type data(mtcars) to load the data
intuition.df<-read.csv(file="intuition.analyis.file.csv")

dim(intuition.df)
str (intuition.df)
head(intuition.df, n=60)
tail(intuition.df, n=60)
summary(intuition.df)
table(intuition.df$state)

##### how to see what packages are installed in R  ##############
installed.packages()

############################# merging two datafiles  #################
# merge two data frames by ID and Country
total <- merge(data frameA,data frameB,by=c("ID","Country"))

# also use dplyr left_join/right_join/...
#

# merge two data frames by ID and Country
total <- merge(data frameA,data frameB,by=c("ID","Country"))
Adding the option "all=TRUE" includes all cases from both datasets.
mydata <- merge(mydata1, mydata3, by=c("country","year"), all=TRUE) 

################## merging using merge ###############################
In SQL database terminology, the default value of all = FALSE gives a natural join, a special 
case of an inner join.
Specifying all.x = TRUE gives a left (outer) join, all.y = TRUE a right (outer) join, and both 
(all = TRUE a (full) outer join.
                                                                                                                                                                                                                     DBMSes do not match NULL records, equivalent to incomparables = NA in R.
food.lion.match.wip.basic.1.df<-merge(FoodLion.match.1.popular.df, FoodLion.match.2.bfr.df)

food.lion.match.wipalltrue1.df<-merge(FoodLion.match.1.popular.df, FoodLion.match.2.bfr.df,
                                      by=c("store.id", "week.ending"),all=TRUE)
food.lion.match.wipallxtrue1.df<-merge(FoodLion.match.1.popular.df, FoodLion.match.2.bfr.df,
                                       by=c("store.id", "week.ending"),all.x=TRUE)
rm(food.lion.match.wip1.df,food.lion.match.wip.basic.1.df,food.lion.match.wipalltrue1.df)
rm(test.merge)

################# exploring a new data file #########################################
dim(andy)
str(andy)
summary(andy)
describe (andy) # from psych 
names(andy)
head(andy)
tail(andy)

## first some scatterplots
pairs( ~log(gco.v4.df$malibu.coconut.rum.1.75l.sales.units.cy)+
         log(gco.v4.df$malibu.coconut.rum.750ml.sales.units.cy)+
         log(gco.v4.df$rum.haven.coconut.rum.1.75l.sales.units.cy)+
         log(gco.v4.df$rum.haven.coconut.rum.750ml.sales.units.cy)+
         log(gco.v4.df$malibu.coconut.rum.750ml.price) + 
         log(gco.v4.df$rum.haven.coconut.rum.750ml.price) +
         gco.v4.df$state + gco.v4.df$malibu.750ml.display.probability.5,
       data=gco.v4.df)
pairs( ~(gco.v4.df$malibu.coconut.rum.1.75l.sales.units.cy)+
         (gco.v4.df$malibu.coconut.rum.750ml.sales.units.cy)+
         (gco.v4.df$rum.haven.coconut.rum.1.75l.sales.units.cy)+
         (gco.v4.df$rum.haven.coconut.rum.750ml.sales.units.cy)+
         (gco.v4.df$malibu.coconut.rum.750ml.price) + 
         (gco.v4.df$rum.haven.coconut.rum.750ml.price) +
         gco.v4.df$state + gco.v4.df$malibu.750ml.display.probability.5,
       data=gco.v4.df)
# or
pairs(gco.v4.df[ , c(15:34)])

####
####
library(car)
scatterplotMatrix() # car package. redline is smoothed fit line; green linear fit

### for discrete variables use 
install.packages("gpairs") # run once
library (gpairs)
gpairs(gco.v4.df)


######## looking for correlations #################
install.packages("corrplot")
install.packages("gplots")
library(corrplot)
library(gplots)

corrplot.mixed(corr=(gco.v4.df[ ,15:34 ], use="complete.obs"),
               upper="ellipse", t1.pos="lt", col = colorpanel (50, "red", "gray60", "blue4"))
#####################################################################################
#####################################################################################
# function_name <- function(arg1, arg2){
#  # Manipulate arguments in some way
#	# Return a value
# }
# The "variable name" you assign will become the name of your function. arg1 and
# arg2 represent the arguments of your function. You can manipulate the arguments
# you specify within the function. After sourcing the function, you can use the 
# function by typing:
# 
# function_name(value1, value2)
#
# If you want to see the source code for any function, just type the function name without any arguments or
# parentheses  i.e. function_name
# args()   # shows the arguments to a function
# arguments after an ellipses must have default values

# Function Documentation
# 
# Functions should contain a comments section immediately below the function definition line. 
# These comments should consist of a one-sentence description of the function; a list of the function's arguments,
# denoted by Args:, with a description of each (including the data type); and a description of the return value,
# denoted by Returns:. The comments 
# should be descriptive enough that a caller can use the function without reading any of the function's code.
# 

#return.list <- list(mean.selected, directory, pollutant, getwd(), id) # can only return one thing but can return a list
#return(return.list)
############################## creating a cumulative sum variable ###############
################################################################################81#
storeID = unique(ht.df$store.id)
bfr.250.ml.dollar.cume = unlist(lapply(storeID, function(x)
{cumsum(ht.df$total.bfr.250.ml.dollars.cy[ht.df$store.id==x])}))

ht.df<-cbind(bfr.250.ml.dollar.cume, ht.df)

rumhaven.cume.dollars.indicator  <-as.numeric(rumhaven.cume.dollars>0) # lappply loops every element inside the object (i.e. StoreID) function(x) 
Ralphs.df<-cbind(Ralphs.df,rumhaven.cume.dollars.indicator)
# creating a new dataframe with only the store weeks Rumhaven in distribution
ralphs.rumhaven.in.distribution.df<-Ralphs.df[Ralphs.df$rumhaven.cume.dollars.indicator ==1, ]

describe(harris.teeter.matched.stores.only.analysis.file1.df)
################################################################################
############ Making price variables                             ###############
################################################################################
harris.teeter.matched.stores.only.analysis.file1.df$bfr.250ml.aup<-
  harris.teeter.matched.stores.only.analysis.file1.df$total.bfr.250.ml.dollars.cy/
  harris.teeter.matched.stores.only.analysis.file1.df$total.bfr.250.ml.units.cy

sum(is.na(harris.teeter.matched.stores.only.analysis.file1.df$bfr.250ml.aup ))
setwd("S:/1 PROJECTS/Shopper Research/Test Market/Barefoot Refresh Single serve/Single Serve 2016 Test/Data/Harris.teeter.analysis")
write.csv(harris.teeter.matched.stores.only.analysis.file1.df, file="harris.teeter.matched.stores.only.analysis.file1.csv")
########################################################################################################################################.
################################################################################
library("lme4")# linear mixed modeling
test.hlm1<-lmer(total.bfr.250.ml.dollars.cy~cluster+final.ht.test.cell+wine.sq.footage+
                  bfr.250ml.max.display.cwd+bfr.features.cwd+(1|store.id),
                data=harris.teeter.matched.stores.only.analysis.file1.df)
################################################################################################################################################################
#############################################################################################################################################
########################## how to select ramdom samples from a datafile   #############################
#############################################################################################################################################
my.data.sample.df<-my.data[sample(nrow(my.data),20),]   # this selects random rows.
### using dplry:
my.data.100sample<-my.data %.% sample frac(0.01, replace = TRUE)

head(quiz1.df, n=2)

####  You can get the column names of a data frame with the `names()' function.
####  You can use the `nrows()' function to compute the number of rows in a data frame.
####  The `tail()' function is an easy way to extract the last few elements of an R object.
####    The single bracket [ operator can be used to extract individual rows of a data frame, such as 
####    quiz1.df[47,0]  to bring back the 47th row and all columns... 
####    tail(quiz1.df, n=3) # brings back the last 3 rows
########################## changing a variable name ############################################################################################
names(harris.teeter.displays.2.merge.df)[3]<-"week.ending" 
names(harris.teeter.displays.2.merge.df)[3]<-"week.ending"  ### note, names is only a vector of column names so no row indictator needed.

####################################################################################################################################################################
############################################################################
###### managing memory
options(java.parameters = "-Xmx1000m")


### how to read in data files
# setting working directory where files are located
setwd("U:/1 PROJECTS/Shopper Research/Test Market/Rum Haven 2015 Test market/Data")
#   List all the objects in your local workspace using ls().
#   List all the files in your working directory using list.files() or dir().
#  ?list.files how to use help, put a ? in front?

install.packages("xlsx") # to read in an excel file
library(xlsx) #  to read in an excel file
GiantEagle.df = read.xlsx('August2015_RumHavenGCOData.xlsx', 1, stringsAsFactors = FALSE)  ##Raw data for GiantEagle "1" is the first worksheet in the file
Ralphs.df = read.xlsx('August2015_RumHavenGCOData.xlsx', 2, stringsAsFactors = FALSE)  ##Raw data for Ralphs RUmhaven data
# to read in a .csv file use Ralphs.data = read.csv("xxxxx")
Pre.Test fi
read.csv(ralphs)

install.packages("tidyr")
  library(tidyr)
library(dplyr)

####1. read in the data
### make sure you enough memory to read in the data file (into RAM) also 
### set comment.char= "" if there are no commented lines in your file!
### use colClasses argument to make the datafile run much faster. if all columns are numeric then just set
### colClasses = "numeric" 
### one can see what the clases of each column by 
# initial <- read.table("datatable.txt", nrows =100)
# classes <- sapply(initial, class)
# tabAll<- read.table ("datatable.txt", colclasses = classes)
#  FoodLion.avg.price.df <- read.csv("Z:/1 PROJECTS/Shopper Research/Test Market/Barefoot Refresh Single serve/Single Serve 2016 Test/Data/FoodLion.avg.price.csv")
########## reading in data from the wepb #########################################
dataset_url <- "http://s3.amazonaws.com/practice_assignment/diet_data.zip"
download.file(dataset_url, "diet_data.zip")
unzip("diet_data.zip", exdir = "diet_data")


###################################################################################
#### background on variable names
#  The preferred form for variable names is all lower case letters and words 
#   separated with dots (variable.name)
# assigning variable names
#  (y <- setNames(x, letters[1:4]))
#  

#### An else statement should always be surrounded on the same line by curly braces.

###   if (condition) {
###   one or more lines
###   } else {
###   one or more lines
###   }
### 
###   see http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#filenames for more details


 ### when using [] is like selection criteria so that...
Eglass.df[is.na(Eglass$total.category.weighted.distribution),]## this will return records with NAs in total.category.weighted.distribution variable

####. Filter out records with category weighted distribution <10
Eglass = Eglass[Eglass$total.category.weighted.distribution>=10 & is.na(Eglass$total.category.weighted.distribution)==FALSE, ]  ## all NAs are kept

#### by sum on a logicial vector it will tell you how many trues such as
sum(is.na(tmp))  ##check 




head(quiz1.df, n=2)

####  You can get the column names of a data frame with the `names()' function.
####  You can use the `nrows()' function to compute the number of rows in a data frame.
####  The `tail()' function is an easy way to extract the last few elements of an R object.
####    The single bracket [ operator can be used to extract individual rows of a data frame, such as 
####    quiz1.df[47,0]  to bring back the 47th row and all columns... 
####    tail(quiz1.df, n=3) # brings back the last 3 rows


sapply(kroger.br.187.df, class) ### to find out the class of variables in your datafile


Eglass = read.csv('EconomyGlass 2015a.csv', stringsAsFactors = FALSE)  ### a lot of extra NAs
Eglass = Eglass[Eglass$price.segment!='',]  ## get rid of blank rows. This is using a logical vector. the pattern is rows, column so the "," is every column
table(Eglass$price.segment)   ## check



LOad # reads in saved workspaces.


### convert time from character to date
Eglass$time = as.Date(Eglass$time, format = "%m/%d/%Y")

############ testing to see if variables are numeric or not.
sapply(potential.test.stores.df, is.numeric)
sapply(potential.control.stores.df, is.numeric)
## the following have negative numbers and need to be replaced with 0
# barefoot.refresh.units.cy
names(hannaford.v1.df)  # to get variable column numbers
hannaford.v1.df[,6:109][is.na(hannaford.v1.df[,6:109])]<-0  #### replacing NAs with 0
summary(hannaford.v1.df)
hannaford.v1.df[,6:109][hannaford.v1.df[,6:109]<0]<-0  #### replacing negitive numbers with 0
summary(hannaford.v1.df)
describe(hannaford.v1.df)
sum(is.na(hannaford.v1.df[,6:109])) # counting the number of missing values
### showing pattern of missing values!!
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(gco.v1.df[,c(16,19,22,25)], col=c('navyblue','red'), numbers=TRUE,
                  sortVars=TRUE, labels=names(gco.v1.df[,c(16,19,22,25)]), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))
######################################################################################################

#########################################################################################################################
###########################################Saving and Loading Rdata files ################################################
##########################################################################################################################
## save ## The most flexible way to save data objects from R uses the save 
## function. By default, save writes an R object (or multiple R objects) 
## to an R-readable binary file that can be opened using load. Because 
## save can store multiple objects (including one's entire current
## workspace), it provides a very flexible way to "pick up where you left off." For
## example, using save.image('myworkspace.RData'), you could save everything about
## your current R workspace, and then load('myworkspace.RData') later and be exactly
## where you were before. But it is also a convenient way to write data to a file
## that you plan to use again in R. Because it saves R objects "as-is," there's no 
## need to worry about problems reading in the data or needing to change structure 
## or variable names because the file is saved (and will load) exactly as it looks
## in R. The dataframe will even have the same name (i.e., in our example, the loaded 
## object will be caleld mydf). The .RData file format is also very space-saving, 
## thus taking up less room than a comparable comma-separated variable file containing
## the same data. To write our dataframe using save, we simply supply the name of the 
## dataframe and the destination file:
save(mydf, file = "saveddf.RData")

save(all.potential.stores.harris.teeter.df, file="all.potential.stores.harris.teeter.RData")
LOad # reads in saved workspaces.

save.image(myworkspace.RData)



##  dput (and dget)

##  Sometimes we want to be able to write our data in a way that makes it 
##  exactly reproducible(like save), but we also want to be able to read
##  the file. Because save creates a binary file, we can only open the
##  file in R (or another piece of software that reads .RData files). If 
##  we want, for example, to be able to look at or change the file in a 
##  text editor, we need it in another format. One R-specific solution for
##  this is dput. The dput function saves data as an R expression. This
##  means that the resulting file can actually be copied and pasted into
##  the R console. This is especially helpful if you want to share (part
##  of) your data with someone else. Indeed, it is rquired that when you
##  ask data-related questions on StackOverflow, that you supply your 
##  data using dput to make it easy for people to help you. We can also
##  simply write the output of dput to the console to see what it looks
##  like. Let's try that before writing it to a file:
##  
##  dput(mydf)

##########################################################################################################################
##########################################################################################################################
############### setting test cell          ###############################################################################
####################################################################################################################################################################################################################################################
##########################################################################################################################
test.df=data.frame(date=c(rep(c('01/04/16','01/11/16','01/18/16','01/25/16'),4)),test_store=c(rep(c(rep('test',4),rep('control',4)),2)),test_period=c(rep(c(0,0,1,1),4)))

test.df$test_dummy=ifelse(test.df$test_store=='control',0,
                          ifelse(as.Date(test.df$date,"%m/%d/%y")<=as.Date('2016-01-17',"%Y-%m-%d"),0,1))
### or 
test.df$test.cell.4.analysis[test.df$potential.test.cell=="Potential Control stores"]<-0
test.df$test.cell.4.analysis[test.df$potential.test.cell=="Potential Test Stores" & test.df$time.period=="Pretest.period"]<-0
test.df$test.cell.4.analysis[test.df$potential.test.cell=="Potential Test Stores" & test.df$time.period=="test.period"]<-1



##########################################################################################################################
### Joe's replace missing prices ##############################################
# checking to see how many missing rh 1.75l price fields
sum(is.na(gco.v1.df$rum.haven.coconut.rum.1.75l.price)) 

for (i in 1:length(gco.v1.df$rum.haven.coconut.rum.1.75l.price)){
  if(is.na(gco.v1.df$rum.haven.coconut.rum.1.75l.price[i]))
    temp <- gco.v1.df[gco.v1.df$chain==gco.v1.df$chain[i] & 
                        gco.v1.df$cluster==gco.v1.df$cluster[i] & gco.v1.df$state==gco.v1.df$state[i]
                      & gco.v1.df$iri.week.ending==gco.v1.df$iri.week.ending[i] ,]
  
  gco.v1.df$rum.haven.coconut.rum.1.75l.price[i] <- mean(temp$rum.haven.coconut.rum.1.75l.price,na.rm=TRUE)
}
# checking to see how many missing rh 1.75l price fields
sum(is.na(gco.v1.df$rum.haven.coconut.rum.1.75l.price))


###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
########################################## start Ted's pricing replacement ##################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
####################################################################################################
##  The mice package provides a nice function md.pattern() to get a better understanding of the pattern of missing data

library(mice)
md.pattern(data)

## A perhaps more helpful visual representation can be obtained using the VIM package as follows

library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Histogram of missing data","Pattern"))

###########################################################################################################################
###########################################################################################################################


setwd("Z:/Common/1 PROJECTS/Consumer Research/Ad & Promo Effectiveness/Radio/VLT/Data")
vinvault_dat=read.csv("full merged data file for analysis.csv",header=T)
#convert everything to numeric
for (i in c(9,15:53,54,55)){
  vinvault_dat[,i]=as.numeric(as.character(vinvault_dat[,i]))
}
#######################################################################
#fill in the missing prices
#how many are there
length (which(is.na(vinvault_dat$VLT_3L_price)==TRUE))

missing_VLT_price=vinvault_dat[which(is.na(vinvault_dat$VLT_3L_price)==TRUE),c(3,4,7,8,11,46)]
for (i in 1:dim(missing_VLT_price)[1]){ # why "[1]"? is this the # of rows in the missing price vector?
  #pull matching clusters
  tmp=vinvault_dat[which(vinvault_dat[,7]==missing_VLT_price[i,3]),]
  #pull matching chains 
  tmp2=tmp[which(tmp[,8]==missing_VLT_price[i,4]),]
  #pull matching weeks
  tmp3=tmp2[which(tmp[,11]==missing_VLT_price[i,5]),] # should tmp be tmp2
  #find mean of price and replace in missing
  missing_VLT_price[i,6]=mean(tmp3[,46],na.rm=T)
}  
vinvault_dat$VLT_3L_price[which(is.na(vinvault_dat$VLT_3L_price)==TRUE)]=missing_VLT_price[,6]

#how many missing prices are left
length (which(is.na(vinvault_dat$VLT_3L_price)==TRUE))

#step 2 replace still missing prices 4 week surrounding average
missing_VLT_price=vinvault_dat[which(is.na(vinvault_dat$VLT_3L_price)==TRUE),c(1,3,4,7,8,11,46)]
for (i in 1:dim(missing_VLT_price)[1]){
  #pull matching tdlink
  tmp=vinvault_dat[which(vinvault_dat[,1]==missing_VLT_price[i,1]),]
  #pull matching weeks
  lowrow=(which(tmp[,11]==missing_VLT_price[i,6])-2)
  hirow=(which(tmp[,11]==missing_VLT_price[i,6])+2)
  tmp2=tmp[c(ifelse(lowrow>0,lowrow,0):hirow),]
  #find mean of price and replace in missing
  missing_VLT_price[i,7]=mean(tmp2[,46],na.rm=T)
}  
vinvault_dat$VLT_3L_price[which(is.na(vinvault_dat$VLT_3L_price)==TRUE)]=missing_VLT_price[,7]

#how many missing prices are left
length (which(is.na(vinvault_dat$VLT_3L_price)==TRUE))

vinvault_dat$VLT_3L_price[which(is.na(vinvault_dat$VLT_3L_price)==TRUE)]=mean(vinvault_dat$VLT_3L_price,na.rm=T)

#how many missing prices are left
length (which(is.na(vinvault_dat$VLT_3L_price)==TRUE))
length (which(is.na(vinvault_dat$TNG_3L_price)==TRUE))
length (which(is.na(vinvault_dat$BOB_3L_price)==TRUE))
length (which(is.na(vinvault_dat$BLB_3L_price)==TRUE))
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
########################################## end Ted's pricing replacement ##################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
Using na.locf (Last Observation Carried Forward) from package zoo:
  
  R> library("zoo")
R> x <- c(12.2, NA, NA, 16.8, 10.1, NA, 12.0)
R> (na.locf(x) + rev(na.locf(rev(x))))/2
[1] 12.20 14.50 14.50 16.80 10.10 11.05 12.00
(does not work if first or last element of x is NA)



###########################################################################################################################
############################################################################################################################################
### how to recode text in a variable based upon a condition
all.potential.stores.harris.teeter.df$chain[all.potential.stores.harris.teeter.df$chain=="HarrisTeeter"]<-"Harris Teeter"   #sweet! it worked.
table(all.potential.stores.harris.teeter.df$chain) 
#############################################################################################################################################
#### selecting only rows that do not have an NA in a specific column:
test.ht.only.test.control.df<-subset(test.ht.final.df,!(is.na(test.ht.final.df$final.ht.test.cell)))
## selecting specific rows and replacing with a value.... missing bfr.dollar.share.popular is due to no popular sales for that store for that week so changing to 0 share
smiths.bfr.ss.4.matching.df[,101][test.df$bfr.total.dollars.cy ==0 & smiths.bfr.ss.4.matching.df$popular4.00.7.99.total.dollars.cy.1==0]<-0
sum(is.na(smiths.bfr.ss.4.matching.df$bfr.dollar.share.popular))
####################################### Important dplyr verbs to remember
dplyr verbs  Description
select()  select columns
filter()	filter rows
arrange()	re-order or arrange rows
mutate()	create new columns
summarise()	summarise values
group_by()	allows for group operations in the "split-apply-combine" concept
dplyr verbs in action
The two most basic functions are select() and filter() which selects columns and filters rows, respectively.
######################################################################################################################
### when using [] is like selection criteria so that...
Eglass.df[is.na(Eglass$total.category.weighted.distribution),]## this will return records with NAs in total.category.weighted.distribution variable

####. Filter out records with category weighted distribution <10
Eglass = Eglass[Eglass$total.category.weighted.distribution>=10 & is.na(Eglass$total.category.weighted.distribution)==FALSE, ] 
## all NAs are kept


#### by sum on a logicial vector it will tell you how many trues such as
sum(is.na(tmp))  ##check 

######################### how to tell if a variable is numeric or not   ###############
str(ralphs.rumhaven.in.distribution.df)

### how to set NA values to 0 as when one has missing values for units or dollars from store level data. They reprsent a 0.
# setting variables  from column 8 to 49 all "NA" to 0. remember [Row,Column]
ncol(Ralphs.df)    # number of columns 
metric.variables <- is.na(Ralphs.df[,8:49])
Ralphs.df[,8:49][metric.variables]=0
sum(is.na(Ralphs.df))  # checking to see if it worked

### setting one variable's NA to 0
Ralphs.df$MALIBU.Sales.Dollars.Cy[is.na(Ralphs.df$MALIBU.Sales.Dollars.Cy)] = 0 # setting NA to 0

###### creating a cume volume and cume vol >0 indicator variable
##### creating a cumulative dollar sales then indicator that the product was in distribution per store
### only use when data is sorted by storeid then date
storeID = unique(Ralphs.df$store.number)
rumhaven.cume.dollars = unlist(lapply(storeID, function(x) {cumsum(Ralphs.df$rumhaven.total.dollars.cy[Ralphs.df$store.number==x])}))

Ralphs.df<-cbind(Ralphs.df,rumhaven.cume.dollars)

rumhaven.cume.dollars.indicator  <-as.numeric(rumhaven.cume.dollars>0) # lappply loops every element inside the object (i.e. StoreID) function(x) 
Ralphs.df<-cbind(Ralphs.df,rumhaven.cume.dollars.indicator)
# creating a new dataframe with only the store weeks Rumhaven in distribution
ralphs.rumhaven.in.distribution.df<-Ralphs.df[Ralphs.df$rumhaven.cume.dollars.indicator ==1, ]

### how to delete variables
ralphs.rumhaven.in.distribution.df<- ralphs.rumhaven.in.distribution.df[,-57]  ### deleting 57th column



##### how to figure out the unique number of stores
storeID2 = unique(ralphs.rumhaven.in.distribution.df$storeid[ralphs.rumhaven.in.distribution.df$malibu.coconut.750.cume.dollars>0 
                                                             & ralphs.rumhaven.in.distribution.df$rumhaven.750ML.cume.dollars>0])
length(storeID2)## tells me how many storeids are saved

storeWeeks = (ralphs.rumhaven.in.distribution.df$storeid[ralphs.rumhaven.in.distribution.df$captain.Morgan.coconut.750ml.Cume.Dollar>0 
                                                         & ralphs.rumhaven.in.distribution.df$rumhaven.750ML.cume.dollars>0])
length(storeWeeks)## tells me how many storeids are saved

################ how to make a variable by adding two other variables togethe then adding it to the datafile.
malibu.total.coconut.dollars<-(ge.rumhaven.in.distribution.df$malibu.coconut.rum.1.75l.dollars +
                                 ge.rumhaven.in.distribution.df$malibu.coconut.rum.750ml.dollars)
ge.rumhaven.in.distribution.df<-cbind(ge.rumhaven.in.distribution.df,malibu.total.coconut.dollars)


#### how to select only certain rows using []
  sum (as.numeric(ralphs.rumhaven.in.distribution.df$rumhaven.750.ml.coconut.dollars.cy))/
  sum (as.numeric(ralphs.rumhaven.in.distribution.df$captain.morgan.coconut.750.cume.dollars[ralphs.rumhaven.in.distribution.df$captain.morgan.coconut.750.cume.dollars>0]))

  ## changing the sort order

  tempcheck <- order(as.numeric(ralphs.rumhaven.in.distribution.df$store.number))
  ralphs.rumhaven.in.distribution.df<-ralphs.rumhaven.in.distribution.df[tempcheck, ]
  str(ralphs.rumhaven.in.distribution.df)
  
  ############## miscellaneous  
  #use Ctrl+L to clear console screen
  
?complete.cases # returns a logical vectorindicating which cases are complete (i.e. "TRUE" or "FALSE") use it so subset a datafile
aq.complete.df <- airquality[complete.cases(airquality),]
class(Pre.Test[,1])
#take a numercial variable and assign it to a catergorical variable 
Pre.Test[,1] <- as.factor(as.character(Pre.Test[,1]))
table(Pre.Test[,1])

#na.rm remove missing values
do1 <- function(x){
  variable1 <- NULL
  variable1 <- mean(x?as.numeric[,2],na.rm=TRUE)
  return (variable1)
}

Pre.Test$Variable2 <- do1(Pre.Test)

########################################################################################################################

##################    how to sort a datafile by two variables

storeID = c(rep(101, 5), rep(202, 5))
Year = c(2000:2004, 2000:2004)
Sales = runif(n = 10, min = 50, max = 100)


tmp = data.frame(storeID, Year, Sales)

shuffle = sample(1:10)
tmp = tmp[shuffle, ]
tmp  ## now the rows are in a random order

reorder = order(tmp$storeID, tmp$Year)  ## ordered by storeID, then by Year
tmp = tmp[reorder,]
tmp


#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),] 
###################################################################################################################
####how to only select certain variables from a dataset.
The x.sub7 data frame contains all the rows but only the 1st, 3rd and 5th variables (columns) of the x.df data set.

x.sub7 <- x.df[, c(1, 3, 5)]
x.sub7
V1          V3          V5
1 -1.6862356  1.35898920  1.75368860
2  0.8610318 -0.01984841 -0.93262483
3 -1.3736436  0.17866428  0.42080132
4  0.7557265 -0.29684582  0.09372863
5  0.6296957  2.16226397  0.37218504
#################################################################################.

## Creating Vectors
### The c() function can be used to create vectors of objects.Consider "c" as concatenate!!!
#Using the vector() function
#> x <- c(0.5, 0.6) ## numeric> x <- c(TRUE, FALSE) ## logical
#> x <- c(T, F) ## logical
#> x <- c("a", "b", "c") ## character
#> x <- 9:29 ## integer
#> x <- c(1+0i, 2+4i) ## complex

#> x <- vector("numeric", length = 10)
#> x
#[1] 0 0 0 0 0 0 0 0 0 0

############### calculating Memory Requirements ################

# Calculating Memory Requirements Calculating Memory Requirements
# I have a data frame with 1,500,000 rows and 120 columns, all of which are numeric data. Roughly,
# how much memory is required to store this data frame?
# 1,500,000 × 120 × 8 bytes/numeric
# = 1440000000 bytes
# = 1440000000 / bytes/MB
# = 1,373.29 MB
# = 1.34 GB

##################################################################
###################  Make it Fast #################################
## The aphorism "make it run, make it right, make it fast" is a good process 
## for people to use when they are learning to write programs in R.
## Once the code produces the right results, you can profile its performance 
## using system.time() or RProf(). Compare the code you've written to other 
## samples of code using techniques that are known to produce faster results.
## Modify your code to improve its performance, while ensuring that the faster
## code generates the same outputs as those from the last step ("make it
## right").

############# BASIC STATS           #######################
Use dim() to obtain the dimensions of the data frame (number of rows and number of columns).  The output is a vector.
Use head() to obtain the first n observations and tail() to obtain the last n observations; by default, n = 6.  These are
good commands for obtaining an intuitive idea of what the data look like without revealing the entire data set, 
which could have millions of rows and thousands of columns.
##   mean (quiz1.df$Ozone, na.rm=TRUE)
###  Other common arithmetic operators are `+`, `-`, `/`, and `^` (where x^2 means 'x squared'). To take the
#   | square root, use the sqrt() function and to take the absolute value, use the abs() function.
# stat.desc() function from the pastecs package
# describe() function from the psych package
summary()

##################################################################### plots ####
plot(x = cars$speed, y = cars$dist, xlab="Speed", ylab = "Stopping Distance")
plot(cars, pch = 2) # using a triange shape as a point go ?points for more options
hist(mtcars$mpg)
boxplot(formula = mpg ~ cyl, data = mtcars)


################  HOW TO SUBSET A FILE #####################################
## Alternatively, you could create a subset of the 'Weight' column where the data
## where 'Day' is equal to 30.

andy[which(andy$Day == 30), "Weight"]
andy[which(andy[,"Day"] == 30), "Weight"]
## Or, we could use the subset() function to do the same thing:
subset(andy$Weight, andy$Day==30)


quiz1.subset1.df<- subset(quiz1.df, Ozone>31)

quiz1.subset2.df<- subset(quiz1.subset1.df, Temp>90)
mean(quiz1.subset2.df$Solar.R)

max(quiz1.df$Ozone[quiz1.df$Month==5], na.rm=TRUE) ### the placement of na.rm is very important

##############           Subsetting Data   ####.
Quiz1.subset3.df<-subset(quiz1.df, Ozone>31 & Temp>90) # selecting only records with ozone >31 and Temp >90

How can I subset a data set?

The R program (as a text file) for all the code on this page.

Subsetting is a very important component of data management and there are several ways that one can subset data in R. This page aims to give a fairly exhaustive list of the ways in which it is possible to subset a data set in R.

First we will create the data frame that will be used in all the examples. We will call this data frame x.df and it will be composed of 5 variables (V1 - V5) where the values come from a normal distribution with a mean 0 and standard deviation of 1; as well as, one variable (y) containing integers from 1 to 5.

x <- matrix(rnorm(30, 1), ncol = 5)
y <- c(1, seq(5))

#combining x and y into one matrix
x <- cbind(x, y)

#converting x into a data frame called x.df
x.df <- data.frame(x)
x.df
V1         V2          V3        V4          V5 y
1 -1.6862356  1.3950211  1.35898920 1.8492410  1.75368860 1
2  0.8610318 -0.5698281 -0.01984841 0.3570547 -0.93262483 1
3 -1.3736436  0.1280908  0.17866428 1.6930332  0.42080132 2
4  0.7557265  1.8622043 -0.29684582 1.0555782  0.09372863 3
5  0.6296957  1.7943359  2.16226397 0.1604166  0.37218504 4
6  0.4694073  1.3096533  1.90324318 1.9372227  1.43930020 5
In order to verify which names are used for the variables in the data frame we use the names function.

names(x.df)
[1] "V1" "V2" "V3" "V4" "V5" "y"
Subsetting rows using the subset function
The subset function with a logical statement will let you subset the data frame by observations. In the following example the x.sub data frame contains only the observations for which the values of the variable y is greater than 2.

x.sub <- subset(x.df, y > 2)
x.sub
V1       V2         V3        V4         V5 y
4 0.7557265 1.862204 -0.2968458 1.0555782 0.09372863 3
5 0.6296957 1.794336  2.1622640 0.1604166 0.37218504 4
6 0.4694073 1.309653  1.9032432 1.9372227 1.43930020 5
Subsetting rows using multiple conditional statements
There is no limit to how many logical statements may be combined to achieve the subsetting that is desired. The data frame x.sub1 contains only the observations for which the values of the variable y is greater than 2 and for which the variable V1 is greater than 0.6.

x.sub1 <- subset(x.df, y > 2 & V1 > 0.6)
x.sub1
V1       V2         V3        V4         V5 y
4 0.7557265 1.862204 -0.2968458 1.0555782 0.09372863 3
5 0.6296957 1.794336  2.1622640 0.1604166 0.37218504 4
Subsetting both rows and columns
It is possible to subset both rows and columns using the subset function. The select argument lets you subset variables (columns). The data frame x.sub2 contains only the variables V1 and V4 and then only the observations of these two variables where the values of variable y are greater than 2 and the values of variable V2 are greater than 0.4.

x.sub2 <- subset(x.df, y > 2 & V2 > 0.4, select = c(V1, V4))
x.sub2
V1        V4
4 0.7557265 1.0555782
5 0.6296957 0.1604166
6 0.4694073 1.9372227
In the data frame x.sub3 contains only the observations in variables V2-V5 for which the values in variable y are greater than 3.

x.sub3 <- subset(x.df, y > 3, select = V2:V5)
x.sub3
V2       V3        V4       V5
5 1.794336 2.162264 0.1604166 0.372185
6 1.309653 1.903243 1.9372227 1.439300
Subsetting rows using indices
Another method for subsetting data sets is by using the bracket notation which designates the indices of the data set. The first index is for the rows and the second for the columns. The x.sub4 data frame contains only the observations for which the values of variable y are equal to 1. Note that leaving the index for the columns blank indicates that we want x.sub4 to contain all the variables (columns) of the original data frame.

x.sub4 <- x.df[x.df$y == 1, ]
x.sub4
V1        V2          V3        V4         V5 y
1 -1.6862356  1.395021  1.35898920 1.8492410  1.7536886 1
2  0.8610318 -0.569828 -0.01984841 0.3570547 -0.9326248 1
Subsetting rows selecting on more than one value
We use the %in% notation when we want to subset on multiple values of y. The x.sub5 data frame contains only the observations for which the values of variable y are equal to either 1 or 4.

x.sub5 <- x.df[x.df$y %in% c(1, 4), ]
x.sub5
V1        V2          V3        V4         V5 y
1 -1.6862356  1.395021  1.35898920 1.8492410  1.7536886 1
2  0.8610318 -0.569828 -0.01984841 0.3570547 -0.9326248 1
5  0.6296957  1.794336  2.16226397 0.1604166  0.3721850 4
Subsetting columns using indices
We can also use the indices to subset the variables (columns) of the data set. The x.sub6 data frame contains
only the first two variables of the x.df data frame. Note that leaving the index for the rows blank indicates 
that we want x.sub6 to contain all the rows of the original data frame.

x.sub6 <- x.df[, 1:2]
x.sub6
V1         V2
1 -1.6862356  1.3950211
2  0.8610318 -0.5698281
3 -1.3736436  0.1280908
4  0.7557265  1.8622043
5  0.6296957  1.7943359
6  0.4694073  1.3096533
The x.sub7 data frame contains all the rows but only the 1st, 3rd and 5th variables (columns) of the x.df data set.

x.sub7 <- x.df[, c(1, 3, 5)]
x.sub7
V1          V3          V5
1 -1.6862356  1.35898920  1.75368860
2  0.8610318 -0.01984841 -0.93262483
3 -1.3736436  0.17866428  0.42080132
4  0.7557265 -0.29684582  0.09372863
5  0.6296957  2.16226397  0.37218504
6  0.4694073  1.90324318  1.43930020
Subsetting both rows and columns using indices
The x.sub8 data frame contains the 3rd-6th variables of x.df and only observations number 1 and 3.

x.sub8 <- x.df[c(1, 3), 3:6]
x.sub8
V3       V4        V5 y
1 1.3589892 1.849241 1.7536886 1
3 0.1786643 1.693033 0.4208013 2
###########################################################################################
####################################### Matrices  #########################################
#   Matricies are vectors with a dimension attribute.  The dimension attribute is itself an
#   integer vector of length 2 (nrow, ncol)
#   m<-matrix(nrow=2, ncol=3)
#   Matrices are constructed columwise, starting in the upper left and going down the column
m <- matrix(nrow=2, ncol=3)# you need the "nrow" part if you do not know the attribute order..
dim(m)
m <- matrix(1:6, nrow=2, ncol=3)
m

###########################################################################################
#######################################################################################
################  cbind and rbind ############################
## One thing to note, rbind needs 2 arguments. The first is an existing data frame and the 
## second is what you want to append to it. This means that there are occassions when you might
## want to create an empty data frame just so there's something to use as the existing data 
## frame in the rbind argument. 
 rbind(existing data, new data)
# results in a matrix: cbind results in adding each variable as a new column, rbind adding them as a new row.

x <- 1:3
y <- 10:12
cbind(x, y)
cbind.result <- cbind(x, y)
rbind.results <-rbind(x, y)
class (rbind.results)

andy_david <- rbind(andy, read.csv(files_full[2]))

##################### unlist ##########################
df1.df <- data.frame(a=1:8, b=10:17)
df1.df # prints out the dataframe in its two columns
unlist(df1.df) prints out the list of numbers in the two columns as one line.
#######################################################################################
################### vectors  ####################################################

##    When given two vectors of the same length, R simply performs the specified arithmetic 
####  operation (`+`, `-`, `*`, etc.) element-by-element. If the vectors are of different lengths, R 'recycles' the shorter vector until
#   it is the same length as the longer vector.

#
####################### lapply  ####################################################
### always returns a list 
x <- list(a = 1:20, b = rnorm(30))
lapply(x, mean)
# creating an anonymous function
x<-list(a = matrix(1:4,2,2), b=matrix(1:6,3,2))
x

lapply(x, function(elt) elt[,1])



### sapply - tries to simlpify the results of lapply
## if the results is a list where every element is length 1, then a vector is returned
## if the result is a list where every element is a vector fo the same length (>1, a matrix is returned.
## if confused, a list is returned
### apply #####################################################################
##  it is most often used to apply a function to the rows or columns of a matrix
##  It can be used with general arrays, taking the average of an array of matrices
## not really faster than a loop.

x <- matrix(rnorm(200), 20, 10)
x
apply (x, 2, mean) # calculates the mean of each colum of the matrix. keeping the second dimension (columns)  collapsing the rows to their mean

apply (x, 1, sum)# calculates the sum of each row of the matrix.
## mapply is a multifariate apply of sores which applies a function in parallel over a set of arguments. The other applys to a single
## object like a list. this allows you to take multiple list arguments and apply in parallel. The number of arguments needs to be same as 
## number of objects..can be used to "vectorize" a function that does not take vectors.
str(mapply)
function(FUN, ..., MoreArgs = null, SIMPLY = TRUE, USE>NAMES = TRUE) #this is the result
  
  ## tapply is used to apply a function over subsets of a vector.
  function( X, INDEX, FUN = NULL, ..., simplify = TRUE)
    X is a vector, INDEX is a factor or a list of factors, FUN is a function to be applied, ... contains other argumjents to be passed to FUN, simplify should we simplify results?
x <- c(rnorm(10), runif(10), rnorm(10,1))
  x
################################################ comon r functions ######
General
builtins() # List all built-in functions
options()  # Set options to control how R computes & displays results

?NA        # Help page on handling of missing data values
abs(x)     # The absolute value of "x"
append()   # Add elements to a vector
c(x)       # A generic function which combines its arguments 
cat(x)     # Prints the arguments
cbind()    # Combine vectors by row/column (cf. "paste" in Unix)
diff(x)    # Returns suitably lagged and iterated differences
gl()       # Generate factors with the pattern of their levels
grep()     # Pattern matching
identical()  # Test if 2 objects are *exactly* equal
jitter()     # Add a small amount of noise to a numeric vector
julian()     # Return Julian date
length(x)    # Return no. of elements in vector x
ls()         # List objects in current environment
mat.or.vec() # Create a matrix or vector
paste(x)     # Concatenate vectors after converting to character
range(x)     # Returns the minimum and maximum of x
rep(1,5)     # Repeat the number 1 five times
rev(x)       # List the elements of "x" in reverse order
seq(1,10,0.4)  # Generate a sequence (1 -> 10, spaced by 0.4)
sequence()     # Create a vector of sequences
sign(x)        # Returns the signs of the elements of x
sort(x)        # Sort the vector x
order(x)       # list sorted element numbers of x
tolower(),toupper()  # Convert string to lower/upper case letters
unique(x)      # Remove duplicate entries from vector
system("cmd")  # Execute "cmd" in operating system (outside of R)
vector()       # Produces a vector of given length and mode

formatC(x)     # Format x using 'C' style formatting specifications
floor(x), ceiling(x), round(x), signif(x), trunc(x)   # rounding functions

Sys.getenv(x)  # Get the value of the environment variable "x"
Sys.putenv(x)  # Set the value of the environment variable "x"
Sys.time()     # Return system time
Sys.Date()     # Return system date
getwd()        # Return working directory
setwd()        # Set working directory
?files         # Help on low-level interface to file system
list.files()   # List files in a give directory
file.info()    # Get information about files

# Built-in constants:
pi,letters,LETTERS   # Pi, lower & uppercase letters, e.g. letters[7] = "g"
month.abb,month.name # Abbreviated & full names for months
Maths
log(x),logb(),log10(),log2(),exp(),expm1(),log1p(),sqrt()   # Fairly obvious
cos(),sin(),tan(),acos(),asin(),atan(),atan2()       # Usual stuff
cosh(),sinh(),tanh(),acosh(),asinh(),atanh()         # Hyperbolic functions
union(),intersect(),setdiff(),setequal()             # Set operations
+,-,*,/,^,%%,%/%                                     # Arithmetic operators
  <,>,<=,>=,==,!=                                      # Comparison operators
  eigen()      # Computes eigenvalues and eigenvectors

deriv()      # Symbolic and algorithmic derivatives of simple expressions
integrate()  # Adaptive quadrature over a finite or infinite interval.

sqrt(),sum()
?Control     # Help on control flow statements (e.g. if, for, while)
?Extract     # Help on operators acting to extract or replace subsets of vectors
?Logic       # Help on logical operators
?Mod         # Help on functions which support complex arithmetic in R
?Paren       # Help on parentheses
?regex       # Help on regular expressions used in R
?Syntax      # Help on R syntax and giving the precedence of operators
?Special     # Help on special functions related to beta and gamma functions
Graphical
help(package=graphics) # List all graphics functions

plot()                # Generic function for plotting of R objects
par()                 # Set or query graphical parameters
curve(5*x^3,add=T)    # Plot an equation as a curve
points(x,y)           # Add another set of points to an existing graph
arrows()              # Draw arrows [see errorbar script]
abline()              # Adds a straight line to an existing graph
lines()               # Join specified points with line segments
segments()            # Draw line segments between pairs of points
hist(x)               # Plot a histogram of x
pairs()               # Plot matrix of scatter plots
matplot()             # Plot columns of matrices

?device               # Help page on available graphical devices
postscript()          # Plot to postscript file
pdf()                 # Plot to pdf file
png()                 # Plot to PNG file
jpeg()                # Plot to JPEG file
X11()                 # Plot to X window
persp()               # Draws perspective plot
contour()             # Contour plot
image()               # Plot an image
Fitting / regression / optimisation
lm  	# Fit liner model
glm		# Fit generalised linear model
nls		# non-linear (weighted) least-squares fitting
lqs		# "library(MASS)" resistant regression

optim		# general-purpose optimisation
optimize	# 1-dimensional optimisation
constrOptim	# Constrained optimisation
nlm		# Non-linear minimisation
nlminb		# More robust (non-)constrained non-linear minimisation
Statistical
help(package=stats)   # List all stats functions

?Chisquare            # Help on chi-squared distribution functions
?Poisson              # Help on Poisson distribution functions
help(package=survival) # Survival analysis

cor.test()            # Perform correlation test
cumsum(); cumprod(); cummin(); cummax()   # Cumuluative functions for vectors
density(x)            # Compute kernel density estimates
ks.test()             # Performs one or two sample Kolmogorov-Smirnov tests
loess(), lowess()     # Scatter plot smoothing
mad()                 # Calculate median absolute deviation
mean(x), weighted.mean(x), median(x), min(x), max(x), quantile(x)
rnorm(), runif()  # Generate random data with Gaussian/uniform distribution
splinefun()           # Perform spline interpolation
smooth.spline()       # Fits a cubic smoothing spline
sd()                  # Calculate standard deviation
summary(x)            # Returns a summary of x: mean, min, max etc.
t.test()              # Student's t-test
var()                 # Calculate variance
sample()              # Random samples & permutations
ecdf()                # Empirical Cumulative Distribution Function
qqplot()              # quantile-quantile plot
###############################################################################
##  Strategy for the PRogramming FUnctions or just programming!  ##############
###############################################################################
# In this article, we'll apply the general concepts from Strategy for the 
# Programming Assignments to flesh out one of many potential solutions to 
# pollutantmean(). We'll walk through the following steps:

# Summarize the objective,
# Describe the inputs and outputs,
# Generate a list of working assumptions to guide subsequent design decisions,
# Use information from the preceding steps to develop a design, and
# Develop the function prototype, coding the design steps as comments into the
# function prototype.
# Finally, we'll end with a set of next steps for the student.