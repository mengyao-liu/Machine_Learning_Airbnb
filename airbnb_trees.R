airbnb <- read.csv("AB_NYC_2019.csv", stringsAsFactors = F)
library(dplyr)

######################  Data cleaning  ######################
# set where reviews_per_month==NA to 0
airbnb$reviews_per_month[is.na(airbnb[,14])] = 0

# find 11 obs with price == 0, so remove them.
ab <- airbnb[-which(airbnb$price==0),]

######################  Some preparation  ######################
ab$neighbourhood <- as.factor(ab$neighbourhood)
ab$neighbourhood_group <- as.factor(ab$neighbourhood_group)
ab$room_type <- as.factor(ab$room_type)

# Determine popularity
ab$popularity <- ifelse(ab$availability_365 < 122,"Popular","Unpopular")
ab$popularity <- as.factor(ab$popularity)
####################################################################


######################  Regression Tree  ######################
# Only use those "Popular" obs
abg <- ab[which(ab$popularity=="Popular"),]
nrow(abg)
## 30,632 obs good

set.seed(199)
# split data into train and test sets
row_train<-sample.int(nrow(abg), floor(.5*nrow(abg)), replace = F)
train<-abg[row_train, ]
test<-abg[-row_train, ]


##################################
### recursive binary splitting ###
##################################
library(tree)
tree.train<-tree(price~room_type + minimum_nights + neighbourhood_group, data=train)
summary(tree.train)
# plot tree
plot(tree.train)
text(tree.train, cex=0.75, pretty=0) 
tree.train

# test MSE
tree.pred.test<-predict(tree.train, newdata=test) 
mean((tree.pred.test-test$price)^2)

# try excluding outliers
hist(abg$minimum_nights,xlim=c(0,200),breaks=100,ylim=c(0,5000))
hist(abg$price,breaks=100,xlim=c(0,1000),ylim=c(0,5000))

temp1 <- train[-which(train$price>400),] # bin<500
temp2 <- temp1[-which(temp1$minimum_nights>30),] # bin<100 obs
nrow(temp2)
tree.temp<-tree(price~room_type + minimum_nights + neighbourhood_group, data=temp2)
summary(tree.temp)
# plot tree
plot(tree.temp)
text(tree.temp, cex=0.75, pretty=0) 
tree.temp

test1 <- test[-which(test$price>400),] # bin<500
test2 <- test1[-which(test1$minimum_nights>30),] # bin<100 obs
nrow(test2)
tree.pred.test<-predict(tree.temp, newdata=test2) 
mean((tree.pred.test-test2$price)^2)

######################
### prune the tree ###
######################
set.seed(1)
cv.train<-cv.tree(tree.train, K=10) 
trees.num<-cv.train$size[which.min(cv.train$dev)]
trees.num

# fit tree with size chosen by pruning
prune.train<-prune.tree(tree.train, best=trees.num)
prune.train
# plot pruned tree
plot(prune.train)
text(prune.train, cex=0.75, pretty=0)

## same tree as recursive binary splitting

###############
### bagging ###
###############
library(randomForest) ##for random forests (and bagging)
set.seed(2)
# bagging is special case of random forest when mtry = number of predictors
bag.train<-randomForest(price~room_type + minimum_nights + neighbourhood_group, 
                        data=train, mtry=3, importance=TRUE)
bag.train 
# importance measures of predictors
importance(bag.train)
# graphical version
varImpPlot(bag.train)

# test MSE
pred.bag<-predict(bag.train, newdata=test)
mean((pred.bag-test$price)^2)

#####################
### random forest ###
#####################
set.seed(2)
# bagging is special case of random forest when mtry = number of predictors
forest.train<-randomForest(price~room_type + minimum_nights + neighbourhood_group, 
                        data=train, mtry=1, importance=TRUE)
forest.train 
# importance measures of predictors
importance(forest.train)
# graphical version
varImpPlot(forest.train)

# test MSE
pred.forest<-predict(forest.train, newdata=test)
mean((pred.forest-test$price)^2)


# try excluding outliers
set.seed(2)
# bagging is special case of random forest when mtry = number of predictors
forest.temp<-randomForest(price~room_type + minimum_nights + neighbourhood_group, 
                           data=temp2, mtry=1, importance=TRUE)
forest.temp 
# importance measures of predictors
importance(forest.temp)
# graphical version
varImpPlot(forest.temp)

pred.forest<-predict(forest.temp, newdata=test2)
mean((pred.forest-test2$price)^2)



######################  Classification Tree  ######################
contrasts(ab$popularity)
# split data into train and test sets
set.seed(199)
row_train<-sample.int(nrow(ab), floor(.6*nrow(ab)), replace = F)
train<-ab[row_train, ]
test<-ab[-row_train, ]

##################################
### recursive binary splitting ###
##################################
tree.train<-tree(popularity~price+minimum_nights+number_of_reviews+reviews_per_month
                 +neighbourhood_group+room_type, data=train)
summary(tree.train)
tree.train
# plot tree
plot(tree.train)
text(tree.train, cex=0.75, pretty=0) 
tree.train

# tesr error rate
pred.test<-test[,"popularity"]
tree.pred.test<-predict(tree.train, newdata=test, type="class") 
table(pred.test, tree.pred.test)
1 - mean(tree.pred.test==pred.test) 


temp1 <- train[-which(train$price>400),]
temp2 <- temp1[-which(temp1$minimum_nights>30),]
nrow(temp2)
tree.temp<-tree(popularity~price+minimum_nights+number_of_reviews+reviews_per_month
                +neighbourhood_group+room_type, data=temp2)
summary(tree.temp)
# plot tree
plot(tree.temp)
text(tree.temp, cex=0.75, pretty=0) 
tree.temp


######################
### prune the tree ###
######################
set.seed(1)
cv.train<-cv.tree(tree.train, K=10, FUN=prune.misclass) 
trees.num<-cv.train$size[which.min(cv.train$dev)]
trees.num

# fit tree with size chosen by pruning
prune.train<-prune.tree(tree.train, best=trees.num)
prune.train
# plot pruned tree
plot(prune.train)
text(prune.train, cex=0.75, pretty=0)

## same tree as recursive binary splitting

# test error rate
prune.pred.test<-predict(prune.train, newdata=test, type="class")
table(pred.test, prune.pred.test)
1 - mean(prune.pred.test==pred.test) 


###############
### bagging ###
###############
set.seed(2)
# bagging is special case of random forest when mtry = number of predictors
bag.train<-randomForest(popularity~price+minimum_nights+number_of_reviews+reviews_per_month
                        +neighbourhood_group+room_type, data=train, mtry=6, importance=TRUE)
bag.train 
# importance measures of predictors
importance(bag.train)
# graphical version
varImpPlot(bag.train)

# test MSE
pred.bag<-predict(bag.train, newdata=test)
1 - mean(pred.bag==pred.test)


#####################
### random forest ###
#####################
set.seed(2)
# bagging is special case of random forest when mtry = number of predictors
forest.train<-randomForest(popularity~price+minimum_nights+number_of_reviews+reviews_per_month
                           +neighbourhood_group+room_type, data=train, mtry=3, importance=TRUE)
forest.train 
# importance measures of predictors
importance(forest.train)
# graphical version
varImpPlot(forest.train)

# test MSE
pred.forest<-predict(forest.train, newdata=test)
1 - mean(pred.forest==pred.test)






