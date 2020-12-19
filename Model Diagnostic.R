# Model Diagnostic for Logistic Regression
# Linearity assumption
glm_stepwise=glm(formula = survived ~ pclass + sex + age + sibsp + embarked, family = "binomial", data = train)
theme_set(theme_classic())
# Select only numeric predictors
mydata <- train %>%select_if(is.numeric) 
mydata=mydata[-c(1,3,4)]
predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
prob=predict(glm_stepwise,type = "response")
mydata <- mydata %>%
  mutate(logit = log(prob/(1-prob))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 1, alpha = 0.5,color="coral") +
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#check multicollinearity
vif(glm_stepwise)

#Influential values
#cook distance
plot(glm_stepwise, which = 4,id.n = 5)
# Extract model results
model.data <- augment(glm_stepwise) %>% mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = survived), alpha = .5) +
  theme_bw()
model.data %>% filter(abs(.resid) > 3) #no outliers


#Model Diagnostic for PCA
#outliers
library(grid)
library(DMwR)
library(outliers)
library(CORElearn)
library(dplyr)
library(rstatix)
# Mahalanobis distance
train %>%
  group_by(survived) %>%
  doo(~mahalanobis_distance(.)) %>%
  filter(is.outlier == TRUE)
train_con$MD <- round(m_dist, 1)

#Linearity
mydata <- train %>%select_if(is.numeric) 
pairs(mydata)