# R Script for doing analysis
library(ggplot2)
#loading in the tree data and taking a look at it
WtData<-read.table(file="NetzerReportSampleTreeData.csv",header=TRUE,sep=",")
View(WtData)
# Making a graph of the Weight in KG  based on the DBH in cm
p<- ggplot(WtData, aes(x=DBHcm,y=DryWtKg)) + geom_point(aes(color=Clone))
p

#Doin a linear model regression on the WtData
fit1<-lm(DryWtKg~DBHcm,data=WtData)
summary(fit1)

# Using ggplot to make the linear regression graphic
p+stat_smooth(method="lm",col="red")
# Here is a way to generate graph with linear model summary in title by making a function
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(aes(color=WtData$Clone)) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(fit1)
#Fitting a smoothing and line to the data
s<-p+geom_smooth()
s
#