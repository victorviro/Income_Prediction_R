
##############################
# Model selection
##############################

#Best Subset Selection
model_selection$bound
library(leaps)

# nvmax help us to choice how variables as maximum we desire to train our model
n=19
model_selection = regsubsets(Income ~ Age + Workclass + Education_Num + Marital_status + Hours_Per_Week +  Education_Num + Marital_status + Occupation + Relationship + Race + Gender + Benefits ,
                              data = data, nvmax = n)
# We assess the best set of variables for each model size
model_selection$xnames
summary_best_models = summary(model_selection)
summary_best_models$outmat
# We can also change the method of stepwise selection
#Forward stepwise
model_selection_forward = regsubsets(Income ~ Age + Workclass + Education_Num + Marital_status + Hours_Per_Week +  Education_Num + Marital_status + Occupation + Relationship + Race + Gender + Benefits ,
                              data = data, nvmax = n, method = 'forward')
#Backward stepwise
model_selection_backward = regsubsets(Income ~ Age + Workclass + Education_Num + Marital_status + Hours_Per_Week +  Education_Num + Marital_status + Occupation + Relationship + Race + Gender + Benefits ,
                              data = data, nvmax = n, method = 'backward')



# We can also get get the R-squared, adjusted R-squared, BIC and Cp
R2 = summary_best_models$rsq
adj_R2 = summary_best_models$adjr2
Cp = summary_best_models$cp
BIC = summary_best_models$bic
number_variables = 1:n
dataf = data.frame(number_variables, R2, adj_R2, Cp, BIC)

plot_R2 = ggplot(dataf, aes(x=number_variables, y= R2)) +geom_line( color='brown') + 
  geom_point(size=2.5) + ggtitle("Plot R-squeared") +
  xlab("Number of model variables") + ylab("R2")
plot_R2

# We want high values of adjusted R2
plot_adj = ggplot(dataf, aes(x=number_variables, y= adj_R2)) +geom_line( color='red') + 
  geom_point(size=2.5) + ggtitle("Plot adjusted R-squared") +
  xlab("Number of model variables") + ylab("Adjusted R2")
plot_adj

# We want low values of Cp and BIC

plot_cp = ggplot(dataf, aes(x=number_variables, y= Cp)) + geom_line( color= 'green') + 
  geom_point(size=2.5) + ggtitle("Plot Cp") +
  xlab("Number of model variables") + ylab("Cp")
plot_cp

plot_bic = ggplot(dataf, aes(x=number_variables, y= BIC))+ geom_line( color='blue') + 
  geom_point(size=2.5) + ggtitle("Plot BIC") +
  xlab("Number of model variables") + ylab("BIC")
plot_bic

plot(model_selection, scale = 'Cp')
# We see importance of variables
plot(model_selection, col='darkcyan')

# We choose the best model of 5 variables to train our models. These variables are, by importance order:
# Marital_status, Education_num, Age, Hours_per_week and Benefits

n=5
model_selection5 = regsubsets(Income ~ Age + Workclass + Education_Num + Marital_status + Hours_Per_Week +  Education_Num + Marital_status + Occupation + Relationship + Race + Gender + Benefits ,
                              data = data, nvmax = n)
# We assess the best set of variables for each model size
summary_best_models5 = summary(model_selection5)$outmat




