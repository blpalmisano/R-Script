# PREDICTIVE ANALYTICS - Project 1, Regression Analysis 
# SECTOR: Energy 
# INDUSTRY: Oil Gas & Consumable Fuels Industry of the Energy 
library(YRmisc) 
library(readxl) 
sp1500 <- read_excel("Downloads/sp1500.xlsx", sheet = "Tab1") 
View(sp1500) 
dim(sp1500) 
names(sp1500) 
unique(sp1500$sector) 
data.frame(unique(sp1500$sector)) 
unique(sp1500$industry) 
data.frame(unique(sp1500$industry)) 
idf<-sp1500[sp1500$industry=="Oil Gas & Consumable Fuels",] 
dim(idf) 
# FIGURES 1-5: Histograms 
par(mfrow=c(3,3)) 
hist(idf$price, col="blue", xlab="Price", ylab="Frequency", main="Fig. 1 Hist of Price")  
hist(idf$eps, col="blue", xlab="EPS", ylab="Frequency", main="Fig. 2 Hist of EPS") 
hist(idf$bvps, col="blue", xlab="BVPS", ylab="Frequency", main="Fig. 3 Hist of BVPS") 
hist(idf$dta, col="blue", xlab="Total Assets", ylab="Frequency", main="Fig. 4 Hist of Debt/TotAssets") 
hist(idf$cr, col="blue", xlab="Current Ratio", ylab="Frequency", main="Fig. 5 Hist of Current Ratio") 
# FIGURES 6-9: SCATTERPLOTS  
par(mfrow=c(2,2)) 
plot(idf$eps, idf$price, xlab="EPS", ylab="Price", main="Fig. 6 EPS vs. Price", type="n") 
text(idf$eps, idf$price, as.character(idf$tkr), cex=0.5) 
plot(idf$bvps, idf$price, xlab="BVPS", ylab="Price", main="Fig. 7 BVPS vs. Price", type="n") 
text(idf$bvps, idf$price, as.character(idf$tkr), cex=0.5) 
plot(idf$dta, idf$price, xlab="Total Assets", ylab="Price", main="Fig. 8 Total Assets vs. Price", type="n")  
text(idf$dta, idf$price, as.character(idf$tkr), cex=0.5) 
plot(idf$cr, idf$price, xlab="Current Ratio", ylab="Price", main="Fig. 9 Current Ratio vs. Price", type="n")  
text(idf$cr, idf$price, as.character(idf$tkr), cex=0.5) 
# DESCRIPTIVE STATISTICS 
ds.summ(idf[,c("price","eps","bvps","dta","cr")],3)[,-c(7,8)]  
# CORRELATION MATRIX 
round(cor(idf[,c("price","eps","bvps","dta","cr")]),3) 
# REGRESSION ANALYSIS 
idf1<-na.omit(idf)  
fit<-lm(price~eps+bvps+dta+cr,na.action=na.omit,data=idf1) 
summary(fit) 
# FIGURES 10 & 11: FITTED RESIDUALS 
idf$price 
fit$fitted.values 
fit$residuals 
par(mfrow=c(2,2)) 
hist(fit$residuals,col="blue",xlab="Fitted Residuals",ylab="Frequency",main="Fig. 10 Histogram of Fitted Residuals",type="n")  
plot(fit$residuals,xlab="Index",ylab="Fitted Residuals",main="Fig. 11 Scatterplot of Fitted Residuals",type="n")  
text(fit$residuals,as.character(idf$tkr),cex=.5) 