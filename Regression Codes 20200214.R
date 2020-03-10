#Create the file
salesdt<-read.csv("sales.csv", header=T)
salesdt
summary(salesdt)
# Fitting the sales on tgtmkt and aveinc
model1<-lm(sales~tgtmkt+aveinc, data=salesdt)
summary(model1)
# Fitting the sales on tgtmkt, aveinc and mall (dummy variable)
model2<-lm(sales~tgtmkt+aveinc+mall, data=salesdt)
summary(model2)
# An alternative is to declare "mall" as a factor.
# This is a good strategy in handling categorical predictors which may not be binary.
salesdt$mall=as.factor(salesdt$mall)
model3<-lm(sales~tgtmkt+aveinc+mall, data=salesdt)
summary(model3)
# Fitting an interaction
model4=lm(sales~tgtmkt+aveinc+mall+mall:tgtmkt, data=salesdt)
summary(model4)

#Best Subset Regression
fitness<-read.csv("fitness.csv", header=T)
fitness
summary(fitness)

full<-lm(oxy~age+weight+runtime+rstpulse+runpulse+maxpulse, data=fitness)
summary(full)
#All Possible Regression
# Load olsrr package
library(olsrr)
ols_step_best_subset(full)
#Forward Entry
forward<-ols_step_forward_p(full,details=TRUE, penter=.1)
forward
#Backward Elimination
backward<-ols_step_backward_p(full,details=TRUE, prem=.05)
backward

#Residual Analysis
# Creating residual plots
# 1. Fitted vs. Residuals
plot(x=model1$fitted.values, y=model1$residuals, xlab="Predicted", ylab="Residuals", main="Fitted vs. Residuals")
# 2. Target Market vs. residuals
plot(x=salesdt$tgtmkt, y=model1$residuals, xlab="Target Market", ylab="Residuals")
# 3. Target Market vs. residuals
plot(x=salesdt$aveinc, y=model1$residuals, xlab="Average Income", ylab="Residuals")

# Checking for Normality via histogram, NPP 
# and Shapiro-Wilk Test and Kolmogorov-Smirnov Test
hist(model1$residuals, xlab="Residuals", main="Histogram of Residuals")
qqnorm(model1$residuals)
qqline(model1$residuals)
shapiro.test(model1$residuals)
ks.test(model1$residuals, pnorm, 0 , 1)

# Checking for Homoskedasticity via the Breusch-Pagan test
# Install lmtest package
# Note that you may use the residual plots above as visualization
library(lmtest)
bptest(model1)

# Checking for first-order autocorrelation of residuals 
# via the Durbin-Watson test
dwtest(model1)

# Detecting multicollinearity via VIF
# Install car package
library(car)
vif(model1)

# Detecting Influential Observations
library(olsrr)
ols_plot_cooksd_bar(model1)
ols_plot_cooksd_chart(model1)
ols_plot_dfbetas(model1)
ols_plot_dffits(model1)
ols_plot_resid_lev(model1)


model11<-lm((sales^1/3)~tgtmkt+aveinc, data=salesdt)