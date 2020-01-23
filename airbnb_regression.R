airbnb <- read.csv("AB_NYC_2019.csv", stringsAsFactors = F)

library(dplyr)
glimpse(airbnb)
## There are 48895 observations in total and there are 16 variables.



######################  Checking missing data  ######################

missing<-array(0,length(names(airbnb)))

for (i in 1:length(names(airbnb)))
{
  # turn the missing value "" in the character vectors into NA
  airbnb[which(airbnb[,i]==""),i] <- NA  
  #missing[i] <- length(airbnb[!complete.cases(airbnb[,i]),i])  # equivalent as below
  missing[i] <- length(airbnb[is.na(airbnb[,i]),i])
}
missing

par(mfrow=c(1,1))
par(las=2) # make label text perpendicular to axis
par(mar=c(13,8,4,2)) # increase y-axis margin.
barplot(missing, main="Missing Observations", names.arg=names(airbnb),
         ylim=c(0,60),col='light blue')
## name is missing. host_name is missing. That's fine. Now look at obs with  
## missing last_review and reviews_per_month.



# check if where last_review (i=13) == NA and reviews_per_month (i=14) == NA, 
# number_of_reviews == 0
number_of_reviews_0_last_review <- airbnb$number_of_reviews[is.na(airbnb[,13])]
length(number_of_reviews_0_last_review[number_of_reviews_0_last_review!=0])
number_of_reviews_0_reviews_per_month <- airbnb$number_of_reviews[is.na(airbnb[,14])]
length(number_of_reviews_0_reviews_per_month[number_of_reviews_0_reviews_per_month!=0])

## Yes they all have 0 review. 
## We don't need to remove any data!
####################################################################



######################  First scrutinization  ######################
# find 11 obs with price == 0, so remove them.
ab <- airbnb[-which(airbnb$price==0),]
## Now data is ready to use.
####################################################################



######################  Some preparation  ######################
ab$neighbourhood <- as.factor(ab$neighbourhood)
ab$neighbourhood_group <- as.factor(ab$neighbourhood_group)
ab$room_type <- as.factor(ab$room_type)

# Determine popularity
# Determine popularity
ab$popularity <- ifelse(ab$availability_365 < 122,"Popular","Unpopular")
ab$popularity <- as.factor(ab$popularity)


# Only use those "Popular" and "Intermediate" obs
abg <- ab[which(ab$popularity=="Popular"),]
nrow(abg)
## 30,632 obs good
####################################################################



######################  Explore  ######################

par(las=1) # make label text parallel to axis
par(mar=c(5,5,4,2)) 
boxplot(abg$price~abg$neighbourhood_group,  log = "y", main="Price by Neighbourhood Group")
boxplot(abg$price~abg$room_type,  log = "y", main="Price by Room Type")
plot(abg$minimum_nights,abg$price,log = "y",  
     main="Plot of Price against Min Nights")
summary(abg$neighbourhood)
## The smallest smaple size is 15. So maybe go with neightbourhood rather than 
## neightbourhood_group to make more precise prediction.

par(mfrow=c(1,2))
par(mar=c(6,5,4,4)) 
par(las=2)
boxplot(ab$number_of_reviews~ab$popularity, names=c("Popular","Unpopular"),
        main="Number of Reviews by Popularity",xlab="")
boxplot(ab$number_of_reviews~ab$popularity, names=c("Popular","Unpopular"),
        ylim=c(0,60), main="Number of Reviews by Popularity",xlab="")


####################################################################



######################  Regression  ######################
attach(abg)

# simple linear regression to test neighbourhood
result00 <- lm( price ~ neighbourhood )
summary(result00)
## use neighbourhood_group instead of neighbourhood

# simple linear regression to test minimum_nights
result000 <- lm( price ~ minimum_nights )
summary(result000)

# fit with room_type, minimum_nights, neighbourhood_group
result1 <- lm( price ~ room_type + minimum_nights + neighbourhood_group )
summary(result1)


###############
# Diagnostics #
###############

# check the residual plots
par(mfrow=c(2,2))
par(mar=c(5,5,2,1)) 
par(las=0)
plot(result1)


par(mfrow=c(1,1))
library(MASS)
boxcox(result1,lambda = seq(-0.34, -0.26, 0.02))
## lambda ~ -0.28


result2 <- lm( price^(-0.28) ~ room_type + minimum_nights + neighbourhood_group )
summary(result2)
par(mfrow=c(2,2))
plot(result2)


library(multcomp)
pairwise_room<-glht(result2, linfct = mcp(room_type= "Tukey"))
summary(pairwise_room)
pairwise_location<-glht(result2, linfct = mcp(neighbourhood_group= "Tukey"))
summary(pairwise_location)


# simple linear regression on number_of_reviews
result0000 <- lm( price ~ number_of_reviews )
summary(result0000)
# simple linear regression on reviews_per_month
result00000 <- lm( price ~ reviews_per_month )
summary(result00000)



# examine high leverage minimum_nights
summary(minimum_nights)
h <- 1/length(minimum_nights) + (minimum_nights-mean(minimum_nights))^2/sum((minimum_nights-mean(minimum_nights))^2)
minimum_nights[which(h> 8/length(minimum_nights) )] # h > (p+1)/n then high leverage
abg2 <- abg[-which(h> 8/length(minimum_nights) ),] # remove obs with high leverage minimum_nights
detach(abg)
attach(abg2)

result3 <- lm( price ~ room_type + minimum_nights + neighbourhood_group )
summary(result3)
par(mfrow=c(2,2))
par(mar=c(5,5,2,1)) 
par(las=0)
plot(result3)


# examine outliers
library(MASS)
price[which( studres(result2) > 3 )]  # studentized residuals > 3 then probably outliers
abg3 <- abg2[-which( studres(result2) > 3 ),] # remove outliers
detach(abg2)
attach(abg3)
result4 <- lm( price ~ room_type + minimum_nights + neighbourhood_group )
summary(result4)
par(mfrow=c(2,2))
par(mar=c(5,5,2,1)) 
par(las=0)
plot(result4)








