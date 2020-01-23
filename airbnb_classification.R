airbnb <- read.csv("AB_NYC_2019.csv", stringsAsFactors = F)

library(dplyr)
glimpse(airbnb)
## There are 48895 observations in total and there are 16 variables.


######################  Data cleaning  ######################

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

# set where reviews_per_month==NA to 0
airbnb$reviews_per_month[is.na(airbnb[,14])] = 0

# find 11 obs with price == 0, so remove them.
ab <- airbnb[-which(airbnb$price==0),]
## Now data is ready to use.
####################################################################


######################  Some preparation  ######################
ab$neighbourhood <- as.factor(ab$neighbourhood)
ab$neighbourhood_group <- as.factor(ab$neighbourhood_group)
ab$room_type <- as.factor(ab$room_type)

# Determine popularity
ab$popularity <- ifelse(ab$availability_365 < 122,"Popular","Unpopular")
ab$popularity <- as.factor(ab$popularity)
####################################################################


######################  Explore  #################################
# check if the two classes have comparable sizes.
length(ab$popularity[ab$popularity=='Popular'])
length(ab$popularity[ab$popularity=='Unpopular'])
## 30632 18252

par(mfrow=c(1,2))
par(mar=c(6,5,4,4)) 
par(las=2)
boxplot(ab$number_of_reviews~ab$popularity, 
        main="Number of Reviews by Popularity",xlab="")
boxplot(ab$number_of_reviews~ab$popularity, 
        ylim=c(0,60), main="Number of Reviews by Popularity",xlab="")


boxplot(ab$reviews_per_month~ab$popularity, 
        main="Reviews Per Month by Popularity",xlab="")
boxplot(ab$reviews_per_month~ab$popularity, 
        ylim=c(0,8), main="Reviews Per Month by Popularity",xlab="")



boxplot(ab$price~ab$popularity, 
        main="Price by Popularity",xlab="")
boxplot(ab$price~ab$popularity, 
        ylim=c(0,500), main="Price by Popularity",xlab="")


boxplot(ab$minimum_nights~ab$popularity, 
        main="Minimum Nights by Popularity",xlab="")
boxplot(ab$minimum_nights~ab$popularity, 
        ylim=c(0,20), main="Minimum Nights by Popularity",xlab="")


par(mfrow=c(1,1))
boxplot(ab$availability_365~ab$neighbourhood_group, 
        main="Available Days by Neighbourhood Group",xlab="")
abline(h=122,col='red')

par(mar=c(7,5,4,4)) 
boxplot(ab$availability_365~ab$room_type, 
        main="Available Days by Room Type",xlab="")
abline(h=122,col='red')

par(las=1) # make label text parallel to axis
par(mar=c(5,5,4,2)) 


####################################################################



######################  Classification  ######################
# set "unpopular" as the reference class - Don't do it!
# ab$popularity<-relevel(ab$popularity, ref = "Unpopular") 
## cause flip in ROC
contrasts(ab$popularity)

set.seed(199)
# split data into train and test sets
row_train<-sample.int(nrow(ab), floor(.6*nrow(ab)), replace = F)
train<-ab[row_train, ]
test<-ab[-row_train, ]

########################
# Logistic Regression #
########################

### Validation Set ###

# fit logistic regression using training data
logistic_train<-glm(popularity~price+minimum_nights+number_of_reviews+reviews_per_month, 
                  family=binomial, data=train)
summary(logistic_train)
# predicted survival rate for test data based on training data
preds<-predict(logistic_train,newdata=test, type="response")

library(ROCR)
# produce the numbers associated with classification table
rates<-prediction(preds, test$popularity)
# store the true positive and false positive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
# plot ROC curve and overlay the diagonal line for random guessing
par(mfrow=c(1,1))
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

# compute the AUC
auc<-performance(rates, measure = "auc")
auc



### k fold CV ###
library(boot) ##for cv.glm function
library(ipred) # for errorest function

logistic <- glm(popularity~price+minimum_nights+number_of_reviews+reviews_per_month, 
                    family=binomial, data=ab)
summary(logistic)
set.seed(100)
logistic.5fold <- cv.glm(ab,logistic, K=5)
logistic.5fold$delta[1]
logistic.10fold <- cv.glm(ab,logistic, K=10)
logistic.10fold$delta[1]


########################
# LDA #
########################

### Validation Set ###

# fit lda using training data
library(MASS)
library(klaR)
lda_train<-lda(popularity~price+minimum_nights+number_of_reviews+reviews_per_month, 
                    data=train)
lda_train


lda_test <- predict(lda_train, test)
preds <- lda_test$posterior[,2]
rates <- prediction(preds, test$popularity)
roc_result <- performance(rates, measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")


auc <- performance(rates, measure = "auc")
auc

# this returns the k fold CV estimate for the test classification error rate
cv.da <- function(object, newdata) 
{
  return(predict(object, newdata = newdata)$class)
} 
set.seed(5)
errorest(popularity ~ price+minimum_nights+number_of_reviews+reviews_per_month, data=ab, 
         model=lda, estimator="cv", est.para=control.errorest(k=5), predict=cv.da)$err 
errorest(popularity ~ price+minimum_nights+number_of_reviews+reviews_per_month, data=ab, 
         model=lda, estimator="cv", est.para=control.errorest(k=10), predict=cv.da)$err 


###############################
# Improved Logistic Regression #
###############################
### Validation Set ###

# fit logistic regression using training data
logistic2_train<-glm(popularity~price+minimum_nights+number_of_reviews+reviews_per_month
                     +neighbourhood_group+room_type, 
                    family=binomial, data=train)
summary(logistic2_train)
# predicted survival rate for test data based on training data
preds<-predict(logistic2_train,newdata=test, type="response")
# produce the numbers associated with classification table
rates<-prediction(preds, test$popularity)
# store the true positive and false positive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
# plot ROC curve and overlay the diagonal line for random guessing
par(mfrow=c(1,1))
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

# compute the AUC
auc<-performance(rates, measure = "auc")
auc
# confusion matrix when threshold is 0.5. True values in the rows. 
confusion.mat<-table(test$popularity,preds > 0.5)
confusion.mat


### k fold CV ###
logistic2 <- glm(popularity~price+minimum_nights+number_of_reviews+reviews_per_month
                 +neighbourhood_group+room_type, 
                family=binomial, data=ab)
summary(logistic2)
set.seed(100)
logistic2.5fold <- cv.glm(ab,logistic2, K=5)
logistic2.5fold$delta[1]
logistic2.10fold <- cv.glm(ab,logistic2, K=10)
logistic2.10fold$delta[1]


##############################
# Some Plots for Fun #
##############################
pairs(ab[,c(10,11,12,14)], lower.panel = NULL, main="Scatterplot of Quantitative Variables")
## can't control xlim and ylim.
## Other quantities may not be that correlated as they appear 
## because they are squeezed in the left corner.
plot(train$price, logistic_train$fitted.values)
## only show S shape when there is only one predictor
partimat(popularity ~ price+minimum_nights+number_of_reviews+reviews_per_month,
         nplots.vert=2, nplots.hor=3, data=train, method="lda")
## can't control xlim and ylim and can't remove the data points. 
## Boundaries are hidden in the left corner.


which(logistic_train$fitted.values==max(logistic_train$fitted.values))
ab$price[c(5768, 26342, 13405, 38665,916,  3546, 18357, 27295  )]
ab$minimum_nights[c(5768, 26342, 13405, 38665,916,  3546, 18357, 27295  )]
ab$number_of_reviews[c(5768, 26342, 13405, 38665,916,  3546, 18357, 27295  )]
ab$reviews_per_month[c(5768, 26342, 13405, 38665,916,  3546, 18357, 27295  )]
## max(logistic_train$fitted.values) = 1, min(logistic_train$fitted.values)=0.2041754
library(scatterplot3d)
s3d <- with(train, scatterplot3d(minimum_nights,price,number_of_reviews, 
                                 color = as.numeric(popularity),xlim=c(100,1000),
                                 ylim=c(100,1000),pch = 19))
## may explain the warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred. 
## Some unpopular sample may be really separated in the 4d predictor space.
