##############################
# SVM
##############################

# See previously script depuration_preprocessing to get cleaned dataset and selection model script

# Required packages
library(e1071)


#We train the model
model_svm = svm(Income  ~ Marital_status + Hours_Per_Week  + Age + Benefits + Hours_Per_Week,
              data[train,], kernel= 'radial', cost=50, probability=TRUE)

# Matrix confusion and statistics
pred_test_svm = predict(model_svm, data[test,])
cf_train_svm = caret::confusionMatrix(fitted(model_svm), data[train,]$Income)
cf_train_svm
indicators_train_svm = c(cf_train_svm$overall[1:1],cf_train_svm$byClass[c(1,2)])
indicators_train_svm

cf_test_svm = caret::confusionMatrix(pred_test_svm, data[test,]$Income)
cf_test_svm
indicators_test_svm = c(cf_test_svm$overall[1:1],cf_test_svm$byClass[c(1,2)])
indicators_test_svm

# ROC curve. We use function Curve_ROC defined in depuration script
prob_test_svm = predict(model_svm, newdata =  data[test,], decision.values = TRUE, probability = TRUE)
roc_svm = roc(response =data[test,]$Income , predictor = as.numeric(prob_test_svm) )

roc_SVM = Curve_ROC(roc_svm, 'Support Vector Machines', 'yellow' )
roc_SVM



# We test any values of hyperparametercost and identify which value produces the best fitting model
costs_param = c(0.001, 0.01, 0.1, 1, 5, 10, 25)
tune.out = tune(svm, Income  ~ Marital_status + Hours_Per_Week  + Age + Benefits + Hours_Per_Week,
                 data = data[train,], kernel = "radial",
                 ranges = list(cost = costs_param))
# We extract the best model
(bestmod = tune.out$best.model)

(performance_svm = data.frame(tune.out$performances))
performance_svm$cost=costs_param
plot(tune.out)


# We can see some graphics of model
plot(model_svm, data[train,], Age ~ Benefits  )
plot(model_svm, data[train,], Education_Num ~ Benefits  )





