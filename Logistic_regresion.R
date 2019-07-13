##############################
# Logistic regression
##############################

# We run script depuration_preprocessing to get cleaned dataset data

# We train the model

model_logistic = glm(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week + Benefits,
                        data = data[train,] , family = binomial)
summary(model_logistic)

# We get probabilities and convert them in predictions
contrasts(data[train,]$Income)
prob_train_logistic = predict(model_logistic, type = "response")
pred_train_logistic = ifelse(prob_train_logistic > .5,'>50K' , "<=50K")

# We do it using test set
prob_test_logistic = predict(model_logistic, data[test,] , type = "response")
pred_test_logistic = ifelse(prob_test_logistic > .5,'>50K' , "<=50K")


# We can use library caret to get information of the model like confusion 
# matrix, accuracy, sensitivity, specificity...
library(caret)
cf_train_logistic = caret::confusionMatrix(as.factor(pred_train_logistic), data[train,]$Income)
cf_train_logistic
indicators_train_logistic = c(cf_train_logistic$overall[1:2],cf_train_logistic$byClass[c(1,2,5,6)])
cf_test_logistic = caret::confusionMatrix(as.factor(pred_test_logistic), data[test,]$Income)
cf_test_logistic
indicators_test_logistic = c(cf_test_logistic$overall[1:2],cf_test_logistic$byClass[c(1,2,5,6)])

# We also plot a ROC curve. The area under the curve(AUC) is other measure which help us
# to choice the best model. How much area does the curve have better

library(pROC)
library(ggplot2)
library(grid)
# we create a function which plot a roc curve

roc_logistic = roc(response =data[test,]$Income , predictor = prob_test_logistic )
dev.off()
Curve_ROC = function(roc, name, color){
  auc = round(auc(roc), digits=5)
  my_text = paste('Area under the curve:',as.character(auc))
  roc_plot = ggroc(roc, col= color, cex.lab = 1.5, cex.axis = 1.5, main = "")
  my_grob = grid.text(my_text, x=0.6,  y=0.05, gp=gpar(col="black", fontsize=14, fontface="bold"))
  roc_plot = roc_plot +
    geom_abline(slope = 1, intercept = 1, linetype = 'dashed', size = 1)+
    labs(title="Curve Roc", subtitle= name)+
    annotation_custom(my_grob)
  roc_plot
}
roc_LOG = Curve_ROC(roc_logistic, 'Logistic regression', 'red' )
roc_LOG

model_logistic$deviance

# We graph a plot which we can see the significant variables of the model
k = summary(model_logistic)$coeff[,3]
kk = data.frame(k, names(k))
ocu = rep(FALSE, length(kk[,2]))
ocu[1:1] = 'Intercept'
ocu[2:2] = 'Education_Num'
ocu[3:3] = 'Age'
ocu[4:7] = 'Marital_status'

plot_influence_logistic = ggplot(kk, aes(kk[,2], abs(kk[,1]), color= ocu ))  +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip()+
  xlab('valiables') + ylab('|Z|') +
  geom_hline(yintercept = 1.5, color='gray', size=1)+
  labs(title = "Significativity of variables", subtitle = 'Logistic regression')
plot_influence_logistic


# Now we train some models with diferentnumber of variables(using model selection) 
# and then we compare them using anova function

model_logistic10 = glm(Income ~ Education_Num + Age  + Hours_Per_Week + Benefits + Relationship  + Gender + Occupation   ,
                        data = data[train,] , family = binomial)
model_logistic9 = glm(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week + Benefits + Relationship  + Gender + Occupation   ,
                         data = data[train,] , family = binomial)
model_logistic8 = glm(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week + Benefits + Relationship  + Gender   ,
                         data = data[train,] , family = binomial)
model_logistic6 = glm(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week + Benefits + Occupation   ,
                        data = data[train,] , family = binomial)
model_logistic4 = glm(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week    ,
                         data = data[train,] , family = binomial)
model_logistic3 = glm(Income ~ Education_Num + Age + Marital_status     ,
                         data = data[train,] , family = binomial)
model_logistic2 = glm(Income ~ Education_Num  + Marital_status     ,
                         data = data[train,] , family = binomial)
model_logistic1 = glm(Income ~ Marital_status     ,
                         data = data[train,] , family = binomial)
anova_logistic = anova(model_logistic1, model_logistic2, model_logistic3 ,
                      model_logistic4 , model_logistic, model_logistic6,
                      model_logistic8, model_logistic9  , model_logistic9,
                      test = "Chisq")
anova_logistic










