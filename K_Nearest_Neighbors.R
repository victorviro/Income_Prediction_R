##############################
# K Nearest Neighbors (KNN)
##############################

# We run script depuration_preprocessing to get cleaned dataset

attach(data)
# We define predictor variables to train and test sets
train_Predictors = cbind( Age, Education_Num, Marital_status, Hours_Per_Week , Benefits)[train,]
test_Predictors = cbind( Age, Education_Num, Marital_status,  Hours_Per_Week , Benefits)[test,]

library(class)
k=7
# We train the model
model_knn = knn3(train_Predictors, data$Income[train],k)

# We get predictions for both train and test (and probabilities)
prob_test_knn = predict(model_knn, test_Predictors, type='prob')
pred_test_knn =  ifelse(prob_test_knn > .5,'>50K' , "<=50K")
prob_train_knn = predict(model_knn, train_Predictors, type='prob')
pred_train_knn  =  ifelse(prob_train_knn > .5,'>50K' , "<=50K")

library(caret)
# Confusion matrix and statistics
cf_train_knn = caret::confusionMatrix(as.factor(pred_train_knn[,2]), data$Income[train])
cf_train_knn
indicators_train_knn = c(cf_train_knn$overall[1:1],cf_train_knn$byClass[c(1,2)])
indicators_train_knn

cf_test_knn = caret::confusionMatrix(as.factor(pred_test_knn[,2]), data$Income[test])
cf_test_knn
indicators_test_knn = c(cf_test_knn$overall[1:1],cf_test_knn$byClass[c(1,2)])
indicators_test_knn

# ROC curve. We use function Curve_ROC defined in logistic regression and depuration scripts
roc_knn = roc(response =data$Income[test] , predictor =as.numeric(prob_test_knn[,2]) )

roc_KNN = Curve_ROC(roc_knn, 'k-Nearest Neighbour', 'green' )
roc_KNN

# We also can use library caret to train the model
knn1 = train(Income ~ Education_Num + Age + Marital_status, method='knn', 
            data=data[train,], trControl= trainControl(method='cv', number=10),
            preProcess=c('center','scale'), tuneGrid=data.frame(.k=1:50))

# We can plot number of neighboors
plot(knn1)

# # We create a function to return a table indicators of the model
# (accuracy,..) for different number of neighbors
plot_k_knn = function(list_values_k){
  for (k in list_values_k){
    model_knn = knn3(train_Predictors, data$Income[train],k)
    prob_test_knn_k = predict(model_knn, test_Predictors, type='prob')
    pred_test_knn_k =  ifelse(prob_test_knn_k > .5,'>50K' , "<=50K")
    cf_test_knn_k = caret::confusionMatrix(as.factor(pred_test_knn_k[,2]), data$Income[test])
    indicators_test_knn_k = c(cf_test_knn_k$overall[1:1],cf_test_knn_k$byClass[c(1,2)])
    indicators_test_knn_k['Neighbors']= k
    if (which(list_values_k==k)[[1]]>1){
      indicators_test_knn_table = rbind(indicators_test_knn_table, indicators_test_knn_k)
    }
    else{
      indicators_test_knn_table= indicators_test_knn_k
    }
  }
  indicators_test_knn_table = data.frame(indicators_test_knn_table)
}
indicators_test_knn_table= plot_k_knn(1:40)
indicators_test_knn_table

k_plot_knn = ggplot(indicators_test_knn_table,  aes(x=Neighbors, y= Specificity, color='specificity')) + geom_line() +
  geom_line(aes(y= Sensitivity, color='Sensitivity'), size=1) +
  geom_line(aes(x=Neighbors, y= Accuracy, color='Accuracy')) +
  labs(title="Plot values k", y="specificity/sensitivity", x="Neighbors")
k_plot_knn



# We use function thresold_plot(script depuration) to visualize indicators of the model
# (accuracy,..) for different values of threshold
thresold_plot_knn2 = thresold_plot(roc_knn)
thresold_plot_knn2


coords(roc_knn, 'best', 'threshold')
# We use function threshold_table(script depuration)
threshold_table_knn= threshold_table(c(0.4,0.45,0.5, 0.5646600, 0.62), prob_test_knn[,2])
threshold_table_knn

