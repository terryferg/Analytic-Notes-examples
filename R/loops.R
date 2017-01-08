### testing loops.
###########################################################################
###################### For Loops ##########################################
###########################################################################

x<- c("a", "b", "c", "d")
y<- c(1, 2, 3, 4)
for(i in 1:4) {
        print(x[i])
}
for(i in seq_along(x)){
        print(x[i])
}



#### Nested loops
x<-matrix(1:6, nrow=2,ncol=3, dimnames=list(c("row1", "row2"), 
          c("Column1", "Column2","Col3")), byrow=TRUE)
x

for(i in seq_len(nrow(x))) {
        for(j in seq_len(ncol(x))){print(x[i,j])}
        
}

for (i in 1:10){
coin<-rbinom(5,9,.99) ## give me 5 observations of flipping a coin 9 times with probablility of heads at.9. How many heads?size=9, with probability of heads at .9
print(coin)
}
###########################################################################
############################ WHILE LOOPS    ###############################
###########################################################################

count<-0
while(count<10) {
        print(count)
        count<-count+1
}

##### if more than one condition to test
z<-5
while (z>=3 && z<=10) { ### r always checks from left to right 
        print(z)
        coin<-rbinom(1,1,.5) ##  a random number generator
        print("coin:") showing what the random number is
        print(coin)
        if(coin==1) {### random walk
           z<-z+1     
        }else {z<-z-1}
        }
######################## repeat ####################
####### repeats until you call break  ##############
##################################
x0<-1
tolerance<-1e-8
repeat {x1 <-computeEstimate() # some function this uses computeEstimate() as an example only
if(abs(x1 - x0)<tolerance) {break} else{x0<-x1}
}
################### better to use a for loop and check for converge  ############

##############################################################################
##########################################################
################################## next, return ##################################

for (i in 1:100) {
        if (i<=20) { #### skip the first 20 iterations
                next
        }
        ### do something here like print i
        print(i)
}
################################################################################