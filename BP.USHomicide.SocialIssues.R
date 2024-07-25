# REGRESSION ANALYSIS...
# DEPENDENT: Homicide Rate
# INDEPENDENT: Unemployment, Income, Bachelor's Degree, High School Degree, Obesity, Robbery, Suicide, Teen Pregnancy
 
# IMPORT LIBRARIES & FILE...
library(YRmisc)
library(readxl)
SocialData_1_ <- read_excel("Documents/SocialData (1).xlsx", 
                            sheet = "data1")
sddf<- SocialData_1_[,c("stateShort","homcideRate","unempRate","income","bachDegree","hsDegree","obesityRate","robberyRate","suicideRate","teenPregRate")]
thesis<-sddf
View(thesis)

# GRAPHICAL ANALYSIS BEGINS HERE...
# FIGURES 1 - 9: Histograms
par(mfrow=c(3,3))
hist(thesis$homcideRate, col="red", xlab="Homicide Rate", ylab="Frequency", main="Fig. 1 Hist of Homicide Rate")
hist(thesis$unempRate, col="orange", xlab="Unemployment Rate", ylab="Frequency", main="Fig. 2 Hist Unemployment Rate")
hist(thesis$income, col="yellow", xlab="Income", ylab="Frequency", main="Fig. 3 Hist of Income")
hist(thesis$bachDegree, col="green", xlab="Bachelor's Degree", ylab="Frequency", main="Fig. 4 Hist of Bachelor's Degree")
hist(thesis$hsDegree, col="blue", xlab="High School Degree", ylab="Frequency", main="Fig. 5 Hist of High School Degree")
hist(thesis$obesityRate, col="purple", xlab="Obesity Rate", ylab="Frequency", main="Fig. 6 Hist of Obesity Rate")
hist(thesis$robberyRate, col="pink", xlab="Robbery Rate", ylab="Frequency", main="Fig. 7 Hist of Robbery Rate")
hist(thesis$suicideRate, col="cyan", xlab="Suicide Rate", ylab="Frequency", main="Fig. 8 Hist of Suicide Rate")
hist(thesis$teenPregRate, col="brown", xlab="Teen Pregnancy Rate", ylab="Frequency", main="Fig. 9 Hist of Teen Pregnancy Rate")

# FIGURES 10 - 17: Scatterplots
par(mfrow=c(3,3))
scatter.smooth(thesis$unempRate, thesis$homcideRate, xlab="Unemployment Rate", ylab="Homicide Rate", main="Fig. 10 Unemployment Rate vs. Homicide Rate", type="n") 
  text(thesis$unempRate, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
scatter.smooth(thesis$income, thesis$homcideRate, xlab="Income", ylab="Homicide Rate", main="Fig. 11 Income vs. Homicide Rate", type="n")
  text(thesis$income, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
scatter.smooth(thesis$bachDegree, thesis$homcideRate, xlab="Bachelor's Degree", ylab="Homicide Rate", main="Fig. 12 Bachelor's Degree vs. Homicide Rate", type="n") 
  text(thesis$bachDegree, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
scatter.smooth(thesis$hsDegree, thesis$homcideRate, xlab="High School Degree", ylab="Homicide Rate", main="Fig. 13 High School Degree vs. Homicide Rate", type="n") 
  text(thesis$hsDegree, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
scatter.smooth(thesis$obesityRate, thesis$homcideRate, xlab="Obesity Rate", ylab="Homicide Rate", main="Fig. 14 Obesity Rate vs. Homicide Rate", type="n") 
  text(thesis$obesityRate, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
scatter.smooth(thesis$robberyRate, thesis$homcideRate, xlab="Robbery Rate", ylab="Homicide Rate", main="Fig. 15 Robbery Rate vs. Homicide Rate", type="n") 
  text(thesis$robberyRate, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
scatter.smooth(thesis$suicideRate, thesis$homcideRate, xlab="Suicide Rate", ylab="Homicide Rate", main="Fig. 16 Suicide Rate vs. Homicide Rate", type="n") 
  text(thesis$suicideRate, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
scatter.smooth(thesis$teenPregRate, thesis$homcideRate, xlab="Teen Pregnancy Rate", ylab="Homicide Rate", main="Fig. 17 Teen Pregnancy Rate vs. Homicide Rate", type="n") 
  text(thesis$teenPregRate, thesis$homcideRate, as.character(thesis$stateShort), cex=1)
  
# FIGURES 10.1 - 17.1: Scatter plots - States with Outliers
# Drop states (outliers).  
thesis_drop_outliers <- thesis
drop_MS <- "MS"
drop_LA <- "LA"
thesis_drop_outliers <- thesis[thesis$stateShort != drop_MS & thesis$stateShort != drop_LA,]
View(thesis_drop_outliers)
  

par(mfrow=c(3,3))
scatter.smooth(thesis_drop_outliers$unempRate, thesis_drop_outliers$homcideRate, xlab="Unemployment Rate", ylab="Homicide Rate", main="Fig. 10.1 UR vs. HR", type="n") 
  text(thesis_drop_outliers$unempRate, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)
scatter.smooth(thesis_drop_outliers$income, thesis_drop_outliers$homcideRate, xlab="Income", ylab="Homicide Rate", main="Fig. 11.1 IL vs. HR", type="n")
  text(thesis_drop_outliers$income, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)
scatter.smooth(thesis_drop_outliers$bachDegree, thesis_drop_outliers$homcideRate, xlab="Bachelor's Degree", ylab="Homicide Rate", main="Fig. 12.1 BD vs. HR", type="n") 
  text(thesis_drop_outliers$bachDegree, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)
scatter.smooth(thesis_drop_outliers$hsDegree, thesis_drop_outliers$homcideRate, xlab="High School Degree", ylab="Homicide Rate", main="Fig. 13.1 HSD vs. HR", type="n") 
  text(thesis_drop_outliers$hsDegree, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)
scatter.smooth(thesis_drop_outliers$obesityRate, thesis_drop_outliers$homcideRate, xlab="Obesity Rate", ylab="Homicide Rate", main="Fig. 14.1 OR vs. HR", type="n") 
  text(thesis_drop_outliers$obesityRate, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)
scatter.smooth(thesis_drop_outliers$robberyRate, thesis_drop_outliers$homcideRate, xlab="Robbery Rate", ylab="Homicide Rate", main="Fig. 15.1 RR vs. HR", type="n") 
  text(thesis_drop_outliers$robberyRate, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)
scatter.smooth(thesis_drop_outliers$suicideRate, thesis_drop_outliers$homcideRate, xlab="Suicide Rate", ylab="Homicide Rate", main="Fig. 16.1 SR vs. HR", type="n") 
  text(thesis_drop_outliers$suicideRate, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)
scatter.smooth(thesis_drop_outliers$teenPregRate, thesis_drop_outliers$homcideRate, xlab="Teen Pregnancy Rate", ylab="Homicide Rate", main="Fig. 17.1 TPR vs. HR", type="n") 
  text(thesis_drop_outliers$teenPregRate, thesis_drop_outliers$homcideRate, as.character(thesis_drop_outliers$stateShort), cex=1)


# STATISTICAL ANALYSIS BEGINS HERE...
# TABLE 1: DESCRIPTIVE STATISTICS
des_stats<-ds.summ(thesis[,c("homcideRate","unempRate","income","bachDegree","hsDegree","obesityRate","robberyRate","suicideRate","teenPregRate")],3)[,-c(7,8)]
View(des_stats)

# TABLE 2: CORRELATION MATRIX
cor_matrix<-round(cor(thesis[,c("homcideRate","unempRate","income","bachDegree","hsDegree","obesityRate","robberyRate","suicideRate","teenPregRate")]),3)
View(cor_matrix)

# TABLE 3: REGRESSION ANALYSIS
thesis1<-na.omit(thesis) 
fit<-lm(homcideRate~unempRate+income+bachDegree+hsDegree+obesityRate+robberyRate+suicideRate+teenPregRate,na.action=na.omit,data=thesis1)
summary(fit)

# FIGURES 18 & 19: Fitted Residuals
#Vector; how much the computer missed each time...
thesis1$homcideRate
fit$fitted.values
fit$residuals
par(mfrow=c(2,2))
hist(fit$residuals,col="grey",xlab="Fitted Residuals",ylab="Frequency",main="Fig. 18 Histogram of Fitted Residuals")
plot(fit$residuals,xlab="Index",ylab="Fitted Residuals",main="Fig. 19 Plot of Fitted Residuals",type="n") 
text(fit$residuals,as.character(thesis1$stateShort),cex=1)



#
