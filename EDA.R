library(carData)
library(car)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)

#load data
data=read.csv("Titanic.csv")


#get number missing values for each variable
number_na=data.frame(sapply(data, function(x) length(which(x == "?"))))
number_na=cbind(variable = row.names(number_na), number_na)
rownames(number_na)=1:nrow(number_na)
names(number_na)[1]="variable"
names(number_na)[2]="number"


#we can plot the number of missing value
plot_na=replace(data, data == "?", NA)
naniar::gg_miss_upset(plot_na)
naniar::gg_miss_var(plot_na)


#subset the dataset, we finally select one response variable and seven explanatory variable
data=data[-c(3,8,10,12,13,14)]


#Missing data
data$age=gsub("?",NA, data$age,fixed = TRUE)
data$fare=gsub("?",NA, data$fare,fixed = TRUE)
data$embarked=gsub("?",NA, data$embarked,fixed = TRUE)
#age replace NA with mean
data[is.na(data$age), 4] <- mean(as.numeric(data$age), na.rm = TRUE)
#remove all NA
data2=na.omit(data)


#change to the reasonable types of variables
summary(data2)
data2$age=as.numeric(data2$age)
data2$fare=as.numeric(data2$fare)
data2$pclass=as.factor(data2$pclass)
data2$parch=as.numeric(data2$parch)
data2$sibsp=as.numeric(data2$sibsp)
data2$survived=as.factor(data2$survived)
data2$sex=as.factor(data2$sex)
data2$embarked=as.factor(data2$embarked)


#Train Test split
size1=floor(0.8 * nrow(data2))
set.seed(123)
train_ind=sample.int(n=nrow(data2), size = size1,replace = FALSE)
train=data2[train_ind, ]
test=data2[-train_ind, ]


#correlation plot
#change the categorical variables to numeric 
#female~1, male~2
#C~1, Q~2, S~3
correlation=data2[,c(2,1,3,4,5,6,7,8)]
correlation$sex=as.numeric(unclass(correlation$sex))
correlation$embarked=as.numeric(unclass(correlation$embarked))
correlation$survived=as.numeric(correlation$survived)
correlation$pclass=as.numeric(correlation$pclass)
corrplot::corrplot(cor(correlation), method="number",col=RColorBrewer::brewer.pal(n=8, name="RdBu"))


# Relationship between Survival and Ticket class
ggplot(data2, aes(x = pclass, fill = survived)) + 
  geom_bar(position = position_dodge()) + 
  theme_classic() + 
  xlab("Ticket Class") + 
  scale_x_discrete(labels = c("1st", "2nd", "3rd")) +
  ylab("Count") + 
  scale_fill_discrete(name = "Survival", labels = c("Dead", "Survived")) + 
  ggtitle("Relationship between Survival and Ticket class") 


# Relationship between Survival and Sex
ggplot(data2, aes(x = sex, fill = survived)) + 
  geom_bar(position = position_dodge()) + 
  theme_classic() + 
  xlab("Sex") + 
  scale_x_discrete(labels = c("Female", "Male")) +
  ylab("Count") + 
  scale_fill_discrete(name = "Survival", labels = c("Dead", "Survived")) + 
  ggtitle("Relationship between Survival and Sex") 


# Relationship between Survival and Number of Siblings/Spouses Aboard
ggplot(data2, aes(x = as.character(sibsp), fill = survived)) + 
  geom_bar(position = position_dodge()) + 
  theme_classic() + 
  xlab("Number of Siblings/Spouses Aboard") + 
  ylab("Count") + 
  scale_fill_discrete(name = "Survival", labels = c("Dead", "Survived")) + 
  ggtitle("Relationship between Survival and Number of Siblings/Spouses Aboard")


# Relationship between Survival and Number of Parents/Children Aboard
ggplot(data2, aes(x = as.character(parch), fill = survived)) + 
  geom_bar(position = position_dodge()) + 
  theme_classic() + 
  xlab("Number of Parents/Children Aboard") + 
  ylab("Count") + 
  scale_fill_discrete(name = "Survival", labels = c("Dead", "Survived")) + 
  ggtitle("Relationship between Survival and Number of Parents/Children Aboard")


# Relationship between Survival and Port of Embarkation
ggplot(data2, aes(x = embarked, fill = survived)) + 
  geom_bar(position = position_dodge()) + 
  theme_classic() + 
  xlab("Port of Embarkation") + 
  scale_x_discrete(labels = c("Cherbourge", "Queenstown", "Southampton")) +
  ylab("Count") + 
  scale_fill_discrete(name = "Survival", labels = c("Dead", "Survived")) + 
  ggtitle("Relationship between Survival and Port of Embarkation") 


# Relationship between Survival and Age
#children:<18; adult:18~65; elderly:>65
age_group=data2[,c("survived","age")]
age_group$age=as.integer(age_group$age)
age_group$age[which(age_group$age<18)]="Children"
age_group$age[which(age_group$age>=18 & age_group$age<=65)]="Adult"
age_group$age[which(age_group$age!="Children" & age_group$age!="Adult")]="Elderly"


ggplot(age_group, aes(x = age, fill = survived)) + 
  geom_bar(position = position_dodge()) + 
  theme_classic() +
  xlab("Age") + 
  ylab("Count") + 
  scale_fill_discrete(name = "Survival", labels = c("Dead", "Survived")) + 
  ggtitle("Relationship between Number of Survival/Death and Age")


# Relationship between Survival and Fare
#0-20:low; 21-60:middle; 61+:high
fair_group=data2[,c("survived","fare")]
fair_group$fare=as.integer(fair_group$fare)
fair_group$fare[which(fair_group$fare<=20)]="low"
fair_group$fare[which(fair_group$fare>20 & fair_group$fare<=60)]="middle"
fair_group$fare[which(fair_group$fare!="low" & fair_group$fare!="middle")]="high"


ggplot(fair_group, aes(x = fare, fill = survived)) + 
  geom_bar(position = position_dodge()) +
  theme_classic() +
  xlab("Fare") + 
  ylab("Count") + 
  scale_fill_discrete(name = "Survival", labels = c("Dead", "Survived")) + 
  ggtitle("Relationship between Number of Survival/Death and Fare")