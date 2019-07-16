##############################
# K Nearest Neighbors (KNN)
##############################

# We run script depuration_preprocessing to get cleaned dataset

attach(data)
# We define predictor variables to train and test sets
train_Predictors = cbind( Age, Education_Num, Marital_status, Hours_Per_Week , Benefits)[train,]
test_Predictors = cbind( Age, Education_Num, Marital_status,  Hours_Per_Week , Benefits)[test,]

library(class)
k=8
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
indicators_train_knn = c(cf_train_knn$overall[1:2],cf_train_knn$byClass[c(1,2,5,6)])

cf_test_knn = caret::confusionMatrix(as.factor(pred_test_knn[,2]), data$Income[test])
cf_test_knn
indicators_test_knn = c(cf_test_knn$overall[1:2],cf_test_knn$byClass[c(1,2,5,6)])


# ROC curve. We use function Curve_ROC defined in logistic regression and depuration scripts
roc_knn = roc(response =data$Income[test] , predictor =as.numeric(prob_test_knn[,2]) )

roc_KNN = Curve_ROC(roc_knn, 'k-Nearest Neighbour', 'green' )
roc_KNN

# We also can use library caret to train the model
knn1 = train(Income ~ Education_Num + Age + Marital_status, method='knn', 
            data=data[train,], trControl= trainControl(method='cv', number=10),
            preProcess=c('center','scale'), tuneGrid=data.frame(.k=1:20))

# We see the optimal number of neighboors
plot(knn1)


