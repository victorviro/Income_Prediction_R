
##############################
# Random forest
##############################

# We run script depuration_preprocessing to get cleaned dataset

# Required packages
library (randomForest)
library(rpart)
library(rpart.plot)

# We train the model
set.seed(1094)
model_rf = randomForest(Income ~ Marital_status   + Education_Num + Age + Hours_Per_Week + Benefits    ,
                         data = data, subset=train ,ntree=16, mtry = 2 , keep.inbag=T, importance = TRUE)
plot(model_rf)

# Confusion matrix and statistics
pred_test_rf = predict(model_rf, newdata = data[test,])

cf_train_rf = caret::confusionMatrix(model_rf$predicted, data[train,]$Income)
cf_train_rf
indicators_train_rf = c(cf_train_rf$overall[1:1],cf_train_rf$byClass[c(1,2)])
indicators_train_rf

cf_test_rf = caret::confusionMatrix(pred_test_rf, data[test,]$Income)
cf_test_rf
indicators_test_rf = c(cf_test_rf$overall[1:1],cf_test_rf$byClass[c(1,2)])
indicators_test_rf

# ROC curve. We use function Curve_ROC defined in depuration script
prob_test_rf = predict (model_rf, newdata = data[test,],'prob')
roc_rf = roc(response =data[test,]$Income , predictor = prob_test_rf[,2] )

roc_RF = Curve_ROC(roc_rf, 'Random Forest', 'brown' )
roc_RF


# We can see the importance of variables
importance(model_rf)
plot_influence_rf = varImpPlot(model_rf,col='blue')


set.seed(1094)
# To see the optimal number of trees we train a model with 500 trees and we add validation test
train_Predictors = cbind( Age, Education_Num, Marital_status, Hours_Per_Week , Benefits)[train,]
test_Predictors = cbind( Age, Education_Num, Marital_status, Hours_Per_Week , Benefits)[test,]
model_rf3 = randomForest(x= data.frame(train_Predictors), y= data$Income[train],
                         xtest = data.frame(test_Predictors), 
                         ytest = data$Income[test])
# We visualize the error(out of bag) and validation error for models with different
# number of trees
# First we extract OOB & validation errors
oob = model_rf3$err.rate[1:500]
validation = model_rf3$test$err.rate[1:500]
number_of_trees = 1:model_rf3$ntree
dataframe_rf = data.frame(oob,validation,number_of_trees)
number_trees = ggplot(dataframe_rf, aes(x= number_of_trees, y= oob, colour = 'Out of bag error')) +
  geom_line() +
  geom_line(aes(x=number_of_trees,y=validation, colour='test error'))+
  scale_y_continuous(labels = scales::dollar)+
  xlab("Number of trees")+ ggtitle("Plot number of trees") 
number_trees
# We can see there is overfitting
num_tree_validation = which.min(model_rf3$test$err.rate[1:500])
num_tree_validation
#16 is the number of trees choosen



# We use function tuneRF to choice parameter mtry, which means the
# number of variables randomly sampled as candidates at each split
set.seed(123)
attach(data)

number_variables_rf = tuneRF(
  x          = train_Predictors,
  y          = data[train]$Income,
  ntreeTry   = 16,
  mtryStart  = 4,
  stepFactor = 0.75,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)
number_variables_rf = data.frame(number_variables_rf)
number_variables_sampled_rf = ggplot(data.frame(number_variables_rf), 
       aes(x=mtry, y= OOBError)) + geom_line( color='blue') +
       geom_point(size=2.5) + ggtitle("Plot number of variables randomly sampled") 
number_variables_sampled_rf 


# We plot a single tree using library rpart
model_tree = rpart(Income ~ Education_Num + Marital_status  , data = data )
plot_tree = rpart.plot(model_tree, box.palette = 'RdBu', shadow.col = 'gray', nn=TRUE)


# We use function thresold_plot(script depuration) to visualize indicators of the model
# (accuracy,..) for different values of threshold
thresold_plot_rf = thresold_plot(roc_rf)
thresold_plot_rf

# We can see the values of threshold best accuracy
coords(roc_rf, 'best', 'threshold')
# We use function threshold_table(script depuration)
threshold_table_rf= threshold_table(c(0.4,0.45,0.5, 0.5312500, 0.55, 0.65), prob_test_rf[,2])
threshold_table_rf



