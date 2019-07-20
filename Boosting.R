##############################
# Boosting
##############################

# We run script depuration_preprocessing to get cleaned dataset

# Required packages
library(gbm)

# We transform our target variable binary to train our model boosting
Income2 = rep(0, length(data$Income))
Income2[data$Income =='>50K'] = 1
data$Income_Binaria = Income2

set.seed(1)
# Model
model_boost = gbm (Income_Binaria ~ Marital_status  + Education_Num + Age + Hours_Per_Week + Benefits,
                   data[train,],distribution = 'poisson',
                   n.trees = 200,
                   interaction.depth = 32,
                   verbose = FALSE)
summary(model_boost)
# We can see the numer of iterations and error
plot_iteration_number_boost=gbm.perf(model_boost, plot.it = T, oobag.curve = F)


# Confusion matrix and indicators
prob_test_boost = predict.gbm(model_boost, data[test ,], 5, type='response')
pred_test_boost = ifelse(prob_test_boost > .5,'>50K' , "<=50K")
prob_train_boost = predict.gbm(model_boost, data[train ,], 2, type='response')
pred_train_boost = ifelse(prob_train_boost > .5,'>50K' , "<=50K")

cf_train_boost = caret::confusionMatrix(as.factor(pred_train_boost), data[train,]$Income)
cf_train_boost
indicators_train_boost = c(cf_train_boost$overall[1:1],cf_train_boost$byClass[c(1,2)])
indicators_train_boost

cf_test_boost = caret::confusionMatrix(as.factor(pred_test_boost), data[test,]$Income)
cf_test_boost
indicators_test_boost = c(cf_test_boost$overall[1:1],cf_test_boost$byClass[c(1,2)])
indicators_test_boost
# ROC curve. We use function Curve_ROC defined in depuration script
roc_boost = roc(response =data[test,]$Income_Binaria , predictor = prob_test_boost )

roc_Boost = Curve_ROC(roc_boost, 'Boosting', 'darkslategrey' )
roc_Boost


# We are going to see the influence of variables
summary_boost = summary(model_boost)
plot_influencia_boost = ggplot(summary_boost, aes(var, rel.inf, color=var)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() + ggtitle("Plot Relative influence") +
  xlab('valiables') + ylab('Relative influence')
plot_influencia_boost
relative.influence(model_boost, 4, scale. = FALSE, sort. = FALSE)

plot_iteration_number_boost = gbm.perf(model_boost, plot.it = TRUE)




# We use function thresold_plot(script depuration) to visualize indicators of the model
# (accuracy,..) for different values of threshold
thresold_plot_boost = thresold_plot(roc_boost)
thresold_plot_boost

# We can see the values of threshold best accuracy
coords(roc_boost, 'best', 'threshold')
# We use function threshold_table(script depuration)
threshold_table_boost= threshold_table(c(0.4,0.45,0.4731661,0.5, 0.53, 0.57, 0.62), prob_test_boost)
threshold_table_boost



