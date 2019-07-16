

##############################
# Load data
##############################

library(data.table)
Train = fread('http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.data')
Test = fread('http://mlr.cs.umass.edu/ml/machine-learning-databases/adult/adult.test')
Test[Test=='<=50K.'] = '<=50K'
Test[Test=='>50K.'] = '>50K' 

# We give names to columns
names(Test) = c("Age", "Workclass", "Fnlg", "Education", "Education_Num", "Marital_status", "Occupation", 'Relationship', 'Race', 'Gender','Capital_Gain','Capital_Loss','Hours_Per_Week', 'Native_Country','Income')
names(Train) = names (Test)
data = rbind(Test,Train)
head(data)
dim(data)


##############################
# Depuration
##############################

# Dataset is pretty clean
# We change missing values '?' in NA 
data[data=='?'] = NA
missing_values = colSums(is.na(data))
missing_values
# We delete missing values
data = na.omit(data)

# We do not remove outliers because are corrected data
par(mfrow=c(1,2))
hist(data$Age, col = 'blue')
boxplot(data$Age, col = "cadetblue1")

# We create Benefits variable which explains variables Capital_Gain and Capital_Loss 
data$Benefits = data$Capital_Gain-data$Capital_Loss

dataframe_prep = data[, c('Capital_Gain', 'Capital_Loss', 'Benefits')][Benefits != 0]
head(dataframe_prep)

# We replace some labels of cateforical variables Marital_status and Workclass

unique(data$Marital_status)
data_Marital_status = data$Marital_status
data$Marital_status = gsub('Married-AF-spouse', 'Married', data$Marital_status)
data$Marital_status = gsub('Married-civ-spouse', 'Married', data$Marital_status)
data$Marital_status = gsub('Married-spouse-absent', 'Married', data$Marital_status)
pie(table(as.factor(data$Marital_status)), radius=1.1)

unique(data$Workclass)
data_Workclass = data$Workclass
data$Workclass = gsub('Federal-gov', 'Empl-gov', data$Workclass)
data$Workclass = gsub('State-gov', 'Empl-gov', data$Workclass)
data$Workclass = gsub('Local-gov', 'Empl-gov', data$Workclass)

data$Workclass = gsub('Self-emp-inc', 'Empl-self', data$Workclass)
data$Workclass = gsub('Self-emp-not-inc', 'Empl-self', data$Workclass)
pie(table(as.factor(data$Workclass)), radius=1.1)


##############################
# Train and test
##############################

# To define training and testing sets we are going to see the proportion
# of observations for each label of our target variable Income 
addmargins(table(data$Income))
pie(table(as.factor(data$Income)), radius=1.1)

# We observe there are 3/4 observations labeled '<=50K' and 1/4 labeled '>50K'

# We need to stabilize(balance) this proportion to train our models and get good perform for both categories
income_old = data$Income
data_label1 = data[Income=='<=50K']
data_label2 = data[Income=='>50K']
tt = sample(c(TRUE, FALSE), nrow(data_label1), prob=c(0.35,0.65), rep = TRUE)
data_label1 = data_label1[tt]
data = rbind(data_label1, data_label2 )
pie(table(as.factor(data$Income)), radius=1.1)


# Now we can define training and testing sets. We use a vector to do this
train = sample(c(TRUE, FALSE), nrow(data), prob=c(0.75,0.25), rep = TRUE)#vector of length=nrow(data) with 75% Trues and 25% Falses
test = (!train)

data_train = data[train,]
data_test = data[test,]


# We convert cateforical variables in class character
cols = names(data)[sapply(data, class) == 'character']
data[,(cols) := lapply(.SD, as.factor), .SDcols = cols]



# we create a function to graph a curve ROC. This function we will use in next scripts
library(pROC)
library(ggplot2)
library(grid)
Curve_ROC = function(roc, name, color){
  auc = round(auc(roc), digits=5)
  my_text = paste('Area under the curve:',as.character(auc))
  roc_plot = ggroc(roc, col= color, cex.lab = 1.5, cex.axis = 1.5, main = "")
  dev.off()
  my_grob = grid.text(my_text, x=0.6,  y=0.05, gp=gpar(col="black", fontsize=14, fontface="bold"))
  roc_plot = roc_plot +
    geom_abline(slope = 1, intercept = 1, linetype = 'dashed', size = 1)+
    labs(title="Curve Roc", subtitle= name)+
    annotation_custom(my_grob)
  roc_plot
}
