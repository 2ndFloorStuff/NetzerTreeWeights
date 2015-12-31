# R Script for doing analysis
library(ggplot2)
#loading in the tree data and taking a look at it
WtData<-read.table(file="NetzerReportSampleTreeData.csv",header=TRUE,sep=",")
View(WtData)
# Making a graph of the Weight in KG  based on the DBH in cm with title
p<- ggplot(WtData, aes(x=DBHcm,y=DryWtKg)) + geom_point(aes(color=Clone)) + 
  labs(title="Netzer Data (152-trees), Dry Tree Weights(Kg) for given DBH (cm)")
p

#Doing a linear model regression on the WtData 
fit1<-lm(DryWtKg~DBHcm,data=WtData)
summary(fit1)

# Using ggplot to make the linear regression graphic
p+stat_smooth(method="lm",col="red")
# Here is a way to generate graph with linear model summary in title by using a function
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  #aes(color=WtData$Clone) can be added after geom_point to add color & legend for clones
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(aes(color=WtData$Clone)) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(fit1)
#Fitting a smoothing and blue line to the data
s<-p+geom_smooth(col="blue")
s
#Polynomial Fit for the Netzer data using 2 cases
#1st with intercept + squared term
#2nd with intercept + linear coefficient + squared term
Pfit1<-lm(DryWtKg~DBHcm + I(DBHcm^2),data=WtData)
summary(Pfit1)

coef(Pfit1)
model(Pfit1)
plot(Pfit1)

library(mosaic)
library(mosaicData)
library(devtools)
devtools::install_github("ProjectMOSAIC/fetch")
require("ProjectMOSAIC/fetch")
u = fetchData("utilities.csv")
#selecting Sites
names(WtData)
tally(group_by(WtData,Site),sort=TRUE)
tally(group_by(WtData,Clone),sort=TRUE)


require(dplyr)
require(magrittr)
WtDataNew<-select(WtData,Site,Clone,DBHcm,DryWtKg)
WtDataNew
WDN<-mutate(WtDataNew,DBHin=DBHcm*2.54,DryWtlbs=DryWtKg*2.205)
WDN
y<-summarise(WDN,AvgWtLbs=mean(DryWtlbs),SdWtLbs=sd(DryWtlbs))
y

x1<-y$SdWtLbs
x1
x2<-y$AvgWtLbs
x2
COVWtLbs<-x1/x2
COVWtLbs

WtData %>% 
  filter(WtData,Clone=="DN34"|Clone=="DN17")%>%
  group_by(Site) %>%
  
  qplot(Site,total)
print
Sites<-factor(WtData$Site)
Sites
site<-"Ashland,<-subset(WtData)
plotPoints(DryWtKg~DBHcm,data=WtData)
 gfm<-fitModel(DryWtKg~A+B*DBHcm+C*DBHcm^2,data=WtData)
 coef(gfm)
 summary(gfm)
 model(gfm)
plotFun(gfm(DBHcm)~DBHcm,add=TRUE,col="red",lwd=1)
