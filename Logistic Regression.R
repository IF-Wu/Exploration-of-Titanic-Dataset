library(carData)
library(car)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)
library(MASS)


# null model
my_logit_null = glm(survived ~ 1, data = train,family = 'binomial' )
# prediction
probabilities_null <- my_logit_null %>% predict(test, type = 'response')
predicted_classes_null <- ifelse(probabilities_null > 0.5, 1, 0)
# confusion matrix
conf_glm_null<-table(predicted_classes_null,test$survived,dnn = c("1","0"))
misclasification_null <- 1 - sum(diag(conf_glm_null))/sum(conf_glm_null)
print(misclasification_null)
knitr::kable(conf_glm_null, caption = "Confusion matrix, logistics regression null model")


# full model
my_logit_full = glm(survived ~ ., data = train,family = 'binomial' )
# prediction
probabilities_full <- my_logit_full %>% predict(test, type = 'response')
predicted_classes_full <- ifelse(probabilities_full > 0.5, 1, 0)
# confusion matrix
conf_glm_full<-table(predicted_classes_full,test$survived,dnn = c("1","0"))
misclasification_full <- 1 - sum(diag(conf_glm_full))/sum(conf_glm_full)
print(misclasification_full)
knitr::kable(conf_glm_full, caption = "Confusion matrix, logistics regression full model")


# stepwise regression starting with the full model
my_logit_stepwise <- my_logit_full %>% stepAIC(direction='both',trace = FALSE)
# prediction
probabilities_stepwise <- my_logit_stepwise %>% predict(test, type = 'response')
predicted_classes_stepwise <- ifelse(probabilities_stepwise > 0.5, 1, 0)
# confusion matrix
conf_glm_stepwise<-table(predicted_classes_stepwise,test$survived,dnn = c("1","0"))
misclasification_stepwise <- 1 - sum(diag(conf_glm_stepwise)/sum(conf_glm_stepwise))
misclasification_stepwise
knitr::kable(conf_glm_null, caption = "Confusion matrix, logistics regression null model")

  
# ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
# null model
predictions_null <- predict(my_logit_null, newdata=test, type="response")
ROCRpred_null <- prediction(predictions_null, test$survived)
ROCRperf_null <- performance(ROCRpred_null, measure = "tpr", x.measure = "fpr")
# full model
predictions_full <- predict(my_logit_full, newdata=test, type="response")
ROCRpred_full <- prediction(predictions_full, test$survived)
ROCRperf_full <- performance(ROCRpred_full, measure = "tpr", x.measure = "fpr")
# stepwise
predictions_stepwise <- predict(my_logit_stepwise, newdata=test, type="response")
ROCRpred_stepwise <- prediction(predictions_stepwise, test$survived)
ROCRperf_stepwise <- performance(ROCRpred_stepwise, measure = "tpr", x.measure = "fpr")

plot(ROCRperf_full, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1), main = 'ROC curve for Full Model')

plot(ROCRperf_stepwise, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1), main = 'ROC curve for Stepwise Model')


# AUC for null model
auc_null <- performance(ROCRpred_null, measure = "auc")
auc_null <- auc_null@y.values[[1]]
auc_null


# AUC for full model
auc_full <- performance(ROCRpred_full, measure = "auc")
auc_full <- auc_full@y.values[[1]]
auc_full


# AUC for stepwise model
auc_stepwise <- performance(ROCRpred_stepwise, measure = "auc")
auc_stepwise <- auc_stepwise@y.values[[1]]
auc_stepwise

AIC(my_logit_null, my_logit_full, my_logit_stepwise)

summary(my_logit_null)
summary(my_logit_full)
summary(my_logit_stepwise)


# hypothesis testing
with(my_logit_full, null.deviance - deviance)
with(my_logit_full, df.null - df.residual)
with(my_logit_full, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


# Interpretation
exp(coefficients(my_logit_stepwise))


# interpretation
exp(confint(my_logit_stepwise))
