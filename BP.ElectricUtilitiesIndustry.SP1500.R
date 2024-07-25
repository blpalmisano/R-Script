
# REGRESSION ANALYSIS...
# SECTOR: Energy
# INDUSTRY: Electric Utilities
# DEPENDENT VARIABLE (Stock Price)

# IMPORT LIBRARIES...
library(YRmisc)
library(readxl)

# Read spMerge: SP1500 data file
sp1500 <- read_excel("Downloads/sp1500.xlsx", sheet = "Tab1")
View(sp1500)

dim(sp1500)
names(sp1500)

# View sector & industry.
data.frame(unique(sp1500$sector))
data.frame(unique(sp1500$industry))

# Choose a sector.
idf<-sp1500[sp1500$industry=="Electric Utilities",c("tkr","price","eps","bvps","dta","cr")]
dim(idf) #Confirm that the dimensions have decreased. 
View(idf)

# Drop CEG & AEP (Outliers/NA Values).
drop_CEG <- "CEG"
idf <- idf[idf$tkr != drop_CEG,]
drop_AEP <- "AEP"
idf <- idf[idf$tkr != drop_AEP,]
View(idf)

# FIGURES 1-5: HISTOGRAMS
par(mfrow=c(3,3))
hist(idf$price, col="red", xlab="Price", ylab="Frequency", main="Fig. 1 Histogram of Price") 
hist(idf$eps, col="orange", xlab="EPS", ylab="Frequency", main="Fig. 2 Histogram of EPS")
hist(idf$bvps, col="yellow", xlab="BVPS", ylab="Frequency", main="Fig. 3 Histogram of BVPS")
hist(idf$dta, col="green", xlab="Total Assets", ylab="Frequency", main="Fig. 4 Histogram of Debt/TotAssets")
hist(idf$cr, col="blue", xlab="Current Ratio", ylab="Frequency", main="Fig. 5 Histogram of Current Ratio")

# FIGURES 6-9: SCATTERPLOTS 
par(mfrow=c(2,2))
plot(idf$eps, idf$price, xlab="EPS", ylab="Price", main="Fig. 6 EPS vs. Price", type="n")
  text(idf$eps, idf$price, as.character(idf$tkr), cex=0.5)
plot(idf$bvps, idf$price, xlab="BVPS", ylab="Price", main="Fig. 7 BVPS vs. Price", type="n")
  text(idf$bvps, idf$price, as.character(idf$tkr), cex=0.5)
plot(idf$dta, idf$price, xlab="Total Assets", ylab="Price", main="Fig. 8 Debt/Total Assets vs. Price", type="n") 
  text(idf$dta, idf$price, as.character(idf$tkr), cex=0.5)
plot(idf$cr, idf$price, xlab="Current Ratio", ylab="Price", main="Fig. 9 Current Ratio vs. Price", type="n") 
  text(idf$cr, idf$price, as.character(idf$tkr), cex=0.5)

# DESCRIPTIVE STATISTICS
des_stats<-ds.summ(idf[,c("price","eps","bvps","dta","cr")],3)[,-c(7,8)]
View(des_stats)

# CORRELATION MATRIX
cor_matrix<-round(cor(na.omit(idf[,c("price","eps","bvps","dta","cr")])),3)
View(cor_matrix)

# REGRESSION ANALYSIS
idf1<-na.omit(idf) 
fit<-lm(price~eps+bvps+dta+cr,na.action=na.omit, data=idf1)
summary(fit)

# FIGURES 10 & 11: FITTED RESIDUALS
idf$price
fit$fitted.values
fit$residuals

par(mfrow=c(2,2))
hist(fit$residuals,col="pink",xlab="Fitted Residuals",ylab="Frequency",main="Fig. 10 Histogram of Fitted Residuals") 
plot(fit$residuals,xlab="Index",ylab="Fitted Residuals",main="Fig. 11 Scatterplot of Fitted Residuals",type="n") 
  text(fit$residuals,as.character(idf$tkr),cex=.5)

