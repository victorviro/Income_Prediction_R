

##############################
# Linear and quadratic discriminant analysis
##############################

# We run script depuration_preprocessing to get cleaned dataset 

# We train the model
library(MASS)
model_lda = lda(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week + Benefits,
                data = data[train,])

# We get predictions for both train and test
pred_train_lda = predict(model_lda)$class
pred_test_lda = predict(model_lda, data[test,])$class

# Confusion matrix and statistics
library(caret)
cf_train_lda = caret::confusionMatrix(pred_train_lda, data[train,]$Income)
cf_train_lda
indicators_train_lda = c(cf_train_lda$overall[1:1],cf_train_lda$byClass[c(1,2)])

cf_test_lda = caret::confusionMatrix(pred_test_lda, data[test,]$Income)
cf_test_lda
indicators_test_lda = c(cf_test_lda$overall[1:1],cf_test_lda$byClass[c(1,2)])
indicators_test_lda

# ROC curve. We use function Curve_ROC defined in depuration script
library(pROC)
prob_test_lda = predict(model_lda, data[test,])$posterior[,2]
roc_lda<-  roc(response =data[test,]$Income , predictor = prob_test_lda )
library(grid)
roc_LDA= Curve_ROC(roc_lda, 'Linear Discriminant analysis', 'blue' )
roc_LDA

# We use function thresold_plot(script depuration) to visualize indicators of the model
# (accuracy,..) for different values of threshold
thresold_plot_lda = thresold_plot(roc_lda)
thresold_plot_lda

coords(roc_lda, 'best', 'threshold')
# We use function threshold_table(script depuration)
threshold_table_lda= threshold_table(c(0.4,0.45,0.5, 0.5375239, 0.57, 0.62), prob_test_lda)
threshold_table_lda

##############################
# Quadratic discriminant analysis 
##############################

# We train the model
model_qda = qda(Income ~ Education_Num + Age + Marital_status + Hours_Per_Week + Benefits,
                data = data[train,])

# We get predictions for both train and test
pred_train_qda = predict(model_qda)$class
pred_test_qda = predict(model_qda, data[test,])$class

# Confusion matrix and statistics
cf_train_qda = caret::confusionMatrix(pred_train_qda, data[train,]$Income)
cf_train_qda
indicators_train_qda = c(cf_train_qda$overall[1:1],cf_train_qda$byClass[c(1,2)])
indicators_train_qda

cf_test_qda = caret::confusionMatrix(pred_test_qda, data[test,]$Income)
cf_test_qda
indicators_test_qda = c(cf_test_qda$overall[1:1],cf_test_qda$byClass[c(1,2)])
indicators_test_qda

# ROC curve. We need probabilities
prob_test_qda = predict(model_qda, data[test,])$posterior[,2]
roc_qda = roc(response =data[test,]$Income , predictor = prob_test_qda )

roc_QDA = Curve_ROC(roc_qda, 'Quadratic discriminant analysis', 'darkcyan' )
roc_QDA
# We can see that quadratic discriminant work a bit better than linear discriminant in this case


# We use function thresold_plot(script depuration) to visualize indicators of the model
# (accuracy,..) for different values of threshold
thresold_plot_qda = thresold_plot(roc_qda)
thresold_plot_qda

coords(roc_qda, 'best', 'threshold')
# We use function threshold_table(script depuration)
threshold_table_qda= threshold_table(c(0.4,0.4546504,0.5, 0.52, 0.57, 0.62), prob_test_qda)
threshold_table_qda
