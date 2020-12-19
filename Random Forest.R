#Reference: https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/
  
table(train$survived)
prop.table(table(test$survived))

#install.packages("ROSE")
library(ROSE)
data_over <- ovun.sample(survived ~ ., data = train, method = "over",N = 1300)$data
table(data_over$survived)


#Tunable parameters include:
#Ntree: Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
#Mtry: Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
#maxnodes: Maximum number of terminal nodes trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by nodesize)


#rf_over
set.seed(321)
rf_clf_over = randomForest(survived ~ ., data=data_over, ntree=250, mtry=4, maxnodes=15, importance=TRUE)
rf_clf_over
rf_pred_over <- predict(rf_clf_over,test[,-2])
rf_conf_over <- table(observed=test[,2],predicted=rf_pred_over)
print('misclassification error rate:')
(sum(rf_conf_over)-sum(diag(rf_conf_over)))/sum(rf_conf_over)


#rf_training
set.seed(321)
# install.packages("randomForest")
library(randomForest)
rf_classifier = randomForest(survived ~ ., data=train, ntree=150, mtry=4, maxnodes=15, importance=TRUE)
rf_classifier


#rf_prediction
rf_pred <- predict(rf_classifier,test[,-2])
rf_conf <- table(observed=test[,2],predicted=rf_pred)
print('misclassification error rate:')
(sum(rf_conf)-sum(diag(rf_conf)))/sum(rf_conf)


#rf_tuning_1
#install.packages("randomForest")
ntry = 15
oob.err = double(ntry)
test.err = double(ntry)
for (i in 1:ntry){
  rf = randomForest(survived ~ ., data=train, ntree=50*i, mtry=3, maxnodes=3, importance=TRUE)
  oob.err[i] = rf$err.rate[50*i,1]
  rf_p = predict(rf,test[,-2])
  test.err[i] = 1-mean(rf_p==test[,2])
}

matplot((1:15)*50, cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of trees created in each model")
legend("right",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


#rf_tuning_2
#install.packages("randomForest")
ntry = 6
oob.err = double(ntry)
test.err = double(ntry)
for (i in 1:ntry){
  rf = randomForest(survived ~ ., data=train, ntree=250, mtry=i, maxnodes=3, importance=TRUE)
  oob.err[i] = rf$err.rate[250,1]
  rf_p = predict(rf,test[,-2])
  test.err[i] = 1-mean(rf_p==test[,2])
}

matplot(1:ntry, cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Error",xlab="Number of Predictors Considered at Each Split", cex.lab=1.4)
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


#rf_tuning_3
#install.packages("randomForest")
ntry = 30
oob.err = double(ntry)
test.err = double(ntry)
for (i in 1:ntry){
  rf = randomForest(survived ~ ., data=train, ntree=250, mtry=3, maxnodes=i*2, importance=TRUE)
  oob.err[i] = rf$err.rate[250,1]
  rf_p = predict(rf,test[,-2])
  test.err[i] = 1-mean(rf_p==test[,2])
}

matplot((1:ntry)*2, cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Error",xlab="Maximum number of nodes in each tree",cex.lab=1.4)
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


#Each features's importance is assessed based on two criteria:
#-MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance when that particular variable is omitted from the training set. Caveat: if two variables are somewhat redundant, then omitting one of them may not lead to massive gains in prediction performance, but would make the second variable more important.
#-MeanDecreaseGini: GINI is a measure of node impurity. Think of it like this, if you use this feature to split the data, how pure will the nodes be? Highest purity means that each node contains only elements of a single class. Assessing the decrease in GINI when that feature is omitted leads to an understanding of how important that feature is to split the data correctly.


#analysis_1
varImpPlot(rf_classifier,main="Feature Importance Plot")


#reprtree is a package to implement the concept of representative trees from ensembles of tree-based machines introduced by Banerjee, et al, 2012.
#reprtree currently implements the d2 metric (closeness based on prediction) from Banerjee, et al, using either euclidean distance (for numeric predictions) or the percentage mismatch (for multi-class classification). The package implements representative trees for random forests currently, and will be extended to bagged trees.
#Plotting representative trees is based on the tree and plotrix packages. Other nicer visualizations will be developed.


#analysis_2
# plot sample tree
#install.packages("devtools")
library(devtools)
devtools::install_github('araastat/reprtree')
library(reprtree)
library(rpart)
library(rpart.plot)
library(tree)

tree_sample = randomForest(survived ~ ., data=train, ntree=150, mtry=4, maxnodes=15, importance=TRUE)

#tree <- getTree(tree_sample, k=1, labelVar=TRUE)
#realtree <- reprtree:::as.tree(tree,tree_sample)
#reprtree:::plot.getTree(realtree)
#summary(train)

#tree_sample
tree_sample = randomForest(survived ~ ., data=train, ntree=150, mtry=4, maxnodes=15)
reprtree:::plot.getTree(tree_sample)

realtree <- getTree(tree_sample, k=1, labelVar=TRUE)
realtree <- reprtree:::as.tree(realtree, tree_sample)
realtree
#tree:::plot.tree(realtree)
#reprtree:::plot(realtree)

rpart_tree <- rpart(survived~., data=data2)
rpart.plot(rpart_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)


#analysis_3_auc
# install.packages("ROCR")
library(ROCR)
rf_prob <- predict(rf_classifier,test[,-2],type="prob")
rf_auc <-performance(prediction(rf_prob[,2], test[,2]),measure="auc")
rf_auc@y.values


#Note: Random forest is affected by multicollinearity but not by outlier problem.