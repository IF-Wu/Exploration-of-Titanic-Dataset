library(carData)
library(car)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)


#PCA
train$sex = ifelse(train$sex == 'female',1,0)
train$survived = ifelse(train$survived == 1, "survived","not survived")
#embarked: S<-1, C<-2, Q<-3
train$embarked[train$embarked == 'S'] <- -1
train$embarked[train$embarked == 'C'] <- 0
train$embarked[train$embarked == 'Q'] <- 1
train = mutate(train,pclass =as.numeric(pclass),sibsp=as.numeric(sibsp),parch=as.numeric(parch),fare=as.numeric(fare),embarked=as.numeric(embarked))

pr.out<-prcomp(train[,-2],scale.=TRUE)
#pr.out$x
data_plot = data.frame(cbind(train$survived,pr.out$x))
data_plot = mutate(data_plot,PC1 =as.numeric(PC1),PC2=as.numeric(PC2),PC3=as.numeric(PC3),PC4=as.numeric(PC4),PC5=as.numeric(PC5),
                   PC6=as.numeric(PC6),PC7=as.numeric(PC7))
names(data_plot) = c('Classification','PC1','PC2','PC3','PC4','PC5','PC6','PC7')
ggplot(data=data_plot)+
  geom_point(mapping = aes(PC1,PC2,colour=Classification))

#(bar plot) visualize how much percent variance each principal component
#explained 
eigs <- pr.out$sdev^2
pro_var = eigs/sum(eigs)
barplot(pro_var,names.arg = c('PC1','PC2','PC3','PC4','PC5','PC6','PC7'),main = "Scree Plot",xlab = 'Principal Component',
        ylab = 'Proportion Variable Explained')



#KNN
library(class)

test$sex = ifelse(test$sex == 'female',1,0)
test$survived = ifelse(test$survived == 1, "survived","not survived")
#embarked: S<-1, C<-2, Q<-3
test$embarked[test$embarked == 'S'] <- -1
test$embarked[test$embarked == 'C'] <- 0
test$embarked[test$embarked == 'Q'] <- 1
test = mutate(test,pclass =as.numeric(pclass),sibsp=as.numeric(sibsp),parch=as.numeric(parch),fare=as.numeric(fare),embarked=as.numeric(embarked))
#k = 12
knn_1 = knn(train = train[,c(-2,-6,-8)],test = test[,c(-2,-6,-8)],cl=train$survived,k=16)
confu_table1 = table(knn_1,factor(test$survived),dnn=c("Predicted","True"))
error_knn_1 = 1 - sum(diag(confu_table1))/sum(confu_table1)
confu_table1
K = seq(1,60,1)
mis_error = seq(1,60,1)
i = 1
for (k_x in K)
{
  knn_k = knn(train = train[,c(-2,-6,-8)],test = test[,c(-2,-6,-8)],cl=train$survived,k=k_x)
  confu_table_x = table(knn_k,factor(test$survived),dnn = c("Predicted",'True'))
  error_knn_x = 1 - sum(diag(confu_table_x))/sum(confu_table_x)
  mis_error[i] = error_knn_x
  i = i+1
}
mis_error_line = data.frame(cbind(K,mis_error))
highlight <- mis_error_line %>% filter(mis_error==min(mis_error_line$mis_error))
ggplot(data = mis_error_line,mapping = aes(x=K,y=mis_error))+
  geom_line()+
  geom_point(alpha=0.3)+
  geom_point(data=highlight,mapping=aes(x=K,y=mis_error),color='red',size=3)+
  labs(x='K Value',y='Misclassfication Error')
K_mis_error = data.frame(cbind(K,mis_error))
min(mis_error)