# PREDICTIVE ANALYTICS - Project 2
# SECTOR: Energy
# INDUSTRY: Energy Equipment & Services 
# COMPANY: Baker Hughes Company Class A

# IMPORT DATA
spdf <- read_excel("Brianna Palmisano (X03625986) Final Project - Baker Hughes.xlsx", 
                                                                     sheet = "Input Data")
View(spdf)
dim(spdf)
names(spdf)

# FIGURES 1-3: Histograms
par(mfrow=c(3,3))
  hist(spdf$Price, col="green", xlab="Price", ylab="Frequency", main="Fig. 1 Hist of Price") 
  hist(spdf$EPS, col="orange", xlab="EPS", ylab="Frequency", main="Fig. 2 Hist of EPS")
  hist(spdf$BVPS, col="blue", xlab="BVPS", ylab="Frequency", main="Fig. 3 Hist of BVPS")

# FIGURES 4-6: Timeseries
par(mfrow=c(3,3))
  ts.plot(tsdf$Price, col="green", xlab="Years", ylab="Frequency", main="Fig. 4 Price vs. Time") 
  ts.plot(tsdf$EPS, col="orange", xlab="Years", ylab="Frequency", main="Fig. 5 EPS vs. Time") 
  ts.plot(tsdf$BVPS, col="blue", xlab="Years", ylab="Frequency", main="Fig. 6 BVPS vs. Time") 

# FIGURES 7-9: Scatterplots
par(mfrow=c(3,3))
  scatter.smooth(spdf$EPS, spdf$Price, col="orange", xlab="EPS", ylab="Price", main="Fig. 7 Price vs. EPS")
  scatter.smooth(spdf$BVPS, spdf$Price, col="blue", xlab="BVPS", ylab="Price", main="Fig. 8 Price vs. BVPS")
  scatter.smooth(spdf$Obs, spdf$Price, col="green", xlab="Obs", ylab="Price", main="Fig. 9 Price vs. Obs") 

# TABLE 1: Desciptive Statistics 
ds.summ(spdf[,c("Price","EPS","BVPS","Obs")],3)[,-c(7,8)] 

# TABLE 2: Correlation Matrix
round(cor(spdf[,c("Price","EPS","BVPS","Obs")]),3)

# TABLE 3: Regression Analysis
tsdf<-na.omit(spdf) 
fit<-lm(Price~EPS+BVPS+Obs,na.action=na.omit,data=rdf)
summary(fit)

# FIGURES 10-12: Fitted Residuals
par(mfrow=c(3,3))
  hist(fit$residuals,col="red",xlab="Fitted Residuals",ylab="Frequency",main="Fig. 10 Histogram of Fitted Residuals") 
  scatter.smooth(tsdf$Price, fit$fitted.values, col="red",xlab="Index",ylab="Fitted Residuals",main="Fig. 11 Scatterplot of Fitted Residuals")
  pl.2ts(tsdf$Price, fit$fitted.values, "Fig. 12 Timeseries of Fitted Residuals") 
  
  
  