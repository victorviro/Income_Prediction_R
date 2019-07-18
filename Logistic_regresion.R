##############################
# Logistic regression
##############################

# See previously script depuration_preprocessing to get cleaned dataset and selection model script

# We train the model

model_logistic = glm(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week + Benefits,
                        data = data[train,] , family = binomial)
summary_logisitc = summary(model_logistic)
summary_logisitc

# We get probabilities and convert them in predictions using threshold .5
contrasts(data[train,]$Income)
prob_train_logistic = predict(model_logistic, type = "response")
pred_train_logistic = ifelse(prob_train_logistic > 0.5,'>50K' , "<=50K")

# We do it using test set
prob_test_logistic = predict(model_logistic, data[test,] , type = "response")
pred_test_logistic = ifelse(prob_test_logistic > 0.5,'>50K' , "<=50K")


# We can use library caret to get information of the model like confusion 
# matrix, accuracy, sensitivity, specificity...
library(caret)
cf_train_logistic = caret::confusionMatrix(as.factor(pred_train_logistic), data[train,]$Income)
cf_train_logistic
indicators_train_logistic = c(cf_train_logistic$overall[1:1],cf_train_logistic$byClass[c(1,2)])
indicators_train_logistic

cf_test_logistic = caret::confusionMatrix(as.factor(pred_test_logistic), data[test,]$Income)
cf_test_logistic
indicators_test_logistic = c(cf_test_logistic$overall[1:1],cf_test_logistic$byClass[c(1,2)])
indicators_test_logistic

# We also plot a ROC curve. The area under the curve(AUC) is other measure which help us
# to choice the best model. How much area does the curve have higher accuracy will have model
#https://rpubs.com/Cristina_Gil/Regresion_Logistica

# Using library pROC
library(pROC)
roc_logistic = roc(response =data[test,]$Income , predictor = prob_test_logistic)
plot(roc_logistic)
roc_logistic
# we use a function to plot a roc curve with title and colour(created in script Depuration)
roc_LOG = Curve_ROC(roc_logistic, 'Logistic regression', 'red' )
roc_LOG


# Significativity of variables
coefficients_logisitc = data.frame(round(summary_logisitc$coefficients, digits = 4))
Significativity = rep(TRUE, dim(coefficients_logisitc)[1])
Significativity[coefficients_logisitc$Pr...z..>0.1] = FALSE
coefficients_logisitc['Significativity'] = Significativity
coefficients_logisitc
# We graph a plot which we can see the most significant variables of the model(using values of statist Z instead p-value)
Z_value = summary_logisitc$coeff[,3]
Z_value_df = data.frame(Z_value, names(Z_value))
name_variables =names(Z_value)
name_variables[4:7] = 'Marital_status'
plot_significativity_logistic = ggplot(Z_value_df, aes(Z_value_df[,2], abs(Z_value_df[,1]), color= name_variables ))  +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() + xlab('valiables') + ylab('|Z|') +
  geom_hline(yintercept = 1.5, color='gray', size=1)+
  labs(title = "Significativity of variables", subtitle = 'Logistic regression')
plot_significativity_logistic



# We use function thresold_plot(script depuration) to visualize indicators of the model
# (accuracy,..) for different values of threshold
thresold_plot_logistic = thresold_plot(roc_logistic)
thresold_plot_logistic

# We can see that a value 0.55 predict aprox 0.8 both sensitivity and specificity
# But we can see the values of threshold best accuracy
coords(roc_logistic, 'best', 'threshold')
# We use function threshold_table(script depuration)
threshold_table_logisitic= threshold_table(c(0.4,0.45,0.4941281,0.5, 0.53, 0.57, 0.62), prob_test_logistic)
threshold_table_logisitic
# We can improve sensitivity increasing threshold but at the expense of loss 
# specificity and a little bit accuracy




# Now we train some models with diferent number of variables(using best subset selection) 
# and then we compare them using anova function
#http://uc-r.github.io/logistic_regression
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















