#Import ggplot2 for graphics
library(ggplot2)

#set the working dirrectory where the data is stored
setwd("~/documents/sampling/")

#Read the data from csv
dat = read.csv("MSDS_6370_Industry3.csv", header = TRUE)

#Cumulative Square- Root F method for selecting strata
#Function based on function created by Timothy R. Johnson
#https://rpubs.com/trjohns/stratify
stratify <- function (y, strata, breaks) 
{
  h <- hist(y, plot = FALSE, breaks = breaks)
  g <- length(h$counts)
  z <- data.frame(lower = rep(NA, g), upper = rep(NA, g), frequency = h$counts, 
                  sqrtf = sqrt(h$counts), csqrtf = cumsum(sqrt(h$counts)), 
                  stratum = NA)
  k <- 1:(strata - 1) * max(z$csqrtf)/strata
  for (i in 1:g) {
    z$lower[i] <- h$breaks[i]
    z$upper[i] <- h$breaks[i + 1]
  }
  for (i in 1:(strata - 1)) {
    tmp <- which(abs(z$csqrtf - k[i]) == min(abs(z$csqrtf - 
                                                   k[i])))
    z$stratum[c(1:g) <= tmp & is.na(z$stratum)] <- i
  }
  z$stratum[is.na(z$stratum)] <- strata
  
  return(list(output = z, cutpoints = k))
}

#*********************************************************************************
#STRATIFICATION OF SALES
#*********************************************************************************
#Certainty Strata For Sales will be Instances Greater than 1 million

#Subset of the data we will be stratifying
subsetsale<- subset(dat, sales < 1000000)
#Use cum sqrt f to break sales into 20 groups (5% each) then divide into 6 strata
sales.cumsqurtftable<-stratify(subsetsale$sales, strata = 5, breaks = 20)
sales.cumsqurtftable
#applying stratum to sales 
salesstrata<-dat
salesstrat<-function(x=2) {
  if (x <= 50000){
    return(1) 
  }else if (x <=100000 & x>50000){
    return (2)
  }else if (x <=200000 & x>100000){
    return (3)
  }else if (x <=550000 & x>200000){
    return (4)
  }else if (x <=1000000 & x>550000){
    return (5)
  }else{
    return (6)
  }
}
salesstrat(250909)

salesstrata$strata<-mapply(salesstrat, salesstrata$sales)

hist(salesstrata$strata)

write.csv(file="salesStrata", x=salesstrata)



ggplot(data=salesstrata, aes(x=inventory, y=sales) ) + geom_point(alpha=.4, col=factor(salesstrata$strata)) + scale_y_continuous(limits = c(0, 5000000)) + scale_x_continuous(limits = c(0, 5000000))

ggplot(data=salesstrata, aes(x=salesstrata$strata, fill=factor(strata)) ) + geom_histogram() 
#+ scale_x_continuous(limits = c(0, 600000))
#*********************************************************************************
#STRATIFICATION OF INVENTORY
#*********************************************************************************
#Certainty Strata For Inventory will be Instances Greater than 1 million

#Subset of the data we will be stratifying
subsetinven<- subset(dat, inventory < 1000000)
#Use cum sqrt f to break sales into 20 groups (5% each) then divide into 6 strata
inven.cumsqurtftable<-stratify(subsetinven$inventory, strata = 6, breaks = 20)
 
#applying stratum to sales cutoff take from output of above funtion ^^^
invenstrata<-dat
invenstrat<-function(x=2) {
  if (x <= 50000){
    return(1) 
  }else if (x <=100000 & x>50000){
    return (2)
  }else if (x <=150000 & x>100000){
    return (3)
  }else if (x <=300000 & x>150000){
    return (4)
  }else if (x <=500000 & x>300000){
    return (5)
  }else if (x <=1000000 & x>500000){
    return (6)
  }else{
    return (7)
  }
}
invenstrat(1000001)

invenstrata$strata<-mapply(invenstrat, invenstrata$inventory)

hist(invenstrata$strata)

write.csv(file="invenStrata", x=invenstrata)

ggplot(data=invenstrata, aes(x=inventory, y=sales) ) + geom_point(alpha=.4, col=factor(invenstrata$strata)) + scale_y_continuous(limits = c(0, 5000000)) + scale_x_continuous(limits = c(0, 5000000))

ggplot(data=invenstrata, aes(x=invenstrata$strata, fill=factor(strata)) ) + geom_histogram() + scale_x_continuous(limits = c(0, 1500000))


#exploratory plots
ggplot(data=dat, aes(x=inventory, y=sales) ) + geom_point(alpha=.1, col="red") #+ scale_y_continuous(limits = c(0, 5000000)) + scale_x_continuous(limits = c(0, 5000000))

ggplot(data=dat, aes(x=sales) ) + geom_freqpoly() + scale_x_continuous(limits = c(0, 200000)) +geom line(x=1000000)

ggplot(data=dat, aes(x=invenstrata$strata) ) + geom_histogram()



ggplot(data=dat, aes(x=sales) ) + geom_histogram(fill="green4") + geom_vline(xintercept=1000000, linetype="dashed", color = "red")+scale_x_continuous(limits = c(0, 1500000))

ggplot(data=dat, aes(x=inventory) ) + geom_histogram(fill="green4") + geom_vline(xintercept=1000000, linetype="dashed", color = "red")+scale_x_continuous(limits = c(0, 1500000))


