##############################
# Related variables
##############################

library(ggplot2)

# We get the correlation matrix of numerical variables.  We create a plot to see it visually
numerical <- data[, c('Age', 'Education_Num', 'Hours_Per_Week', 'Benefits')]
corr <- round(cor(numerical), digit=3)
dev.off()
library(ggcorrplot)
Correlation_Matrix <- ggcorrplot(corr, p.mat = cor_pmat(numerical),
                                hc.order = TRUE, type = "lower",
                                color = c("#FC4E07", "white", "#00AFBB"),
                                outline.col = "white", lab = TRUE , title='Correlation matrix')
Correlation_Matrix
Correlation_Matrix_plotly <- ggplotly(Correlation_Matrix)
Correlation_Matrix_plotly

# We can use data visualization tools to see the relationship between categorical and numerical variables

# We graph a barplot where we can see relationship between variables Income and Marital_status
barplot1 <- ggplot(data, aes(Marital_status)) +
  geom_bar(aes(fill = Income)) + ggtitle("Barplot Marital_status-Income")
barplot1
library(plotly)
barplot1_plotly <- ggplotly(barplot1)
barplot1_plotly

# We graph a scatterplot where we can see relationship between variables Income, Benefits and Age
scatterplot1 <- ggplot(data, aes(x=Benefits, y= Age, color=Income))+
  geom_point() + ggtitle("Scatterplot") 
scatterplot1
scatterplot1_plotly <- ggplotly(scatterplot1)
scatterplot1_plotly



