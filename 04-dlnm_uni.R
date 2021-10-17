#########################################################################################################################################
# Univariate model
#########################################################################################################################################
# si
model_si <- manyglm(Y ~  si_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_si <- summary.manyglm(model_si, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_si)
model_si$coefficients
summary_si$cov.unscaled
plot(model_si)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_si,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# School closing
model_c1 <- manyglm(Y ~  c1_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c1 <- summary.manyglm(model_c1, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c1)
model_c1$coefficients
plot(model_c1)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c1,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Workplace closing
model_c2 <- manyglm(Y ~  c2_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c2 <- summary.manyglm(model_c2, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c2)
model_c2$coefficients
plot(model_c2)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c2,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)


# Cancel public events
model_c3 <- manyglm(Y ~  c3_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c3 <- summary.manyglm(model_c3, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c3)
model_c3$coefficients
plot(model_c3)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c3,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Restrictions on gatherings
model_c4 <- manyglm(Y ~  c4_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c4 <- summary.manyglm(model_c4, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c4)
model_c4$coefficients
plot(model_c4)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c4,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Close public transport
model_c5 <- manyglm(Y ~  c5_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c5 <- summary.manyglm(model_c5, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c5)
model_c5$coefficients
plot(model_c5)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c5,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Stay at home requirements
model_c6 <- manyglm(Y ~  c6_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c6 <- summary.manyglm(model_c6, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c6)
model_c6$coefficients
plot(model_c6)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c6,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Restrictions on internal movement
model_c7 <- manyglm(Y ~  c7_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c7 <- summary.manyglm(model_c7, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c7)
model_c7$coefficients
plot(model_c7)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c7,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# International travel controls
model_c8 <- manyglm(Y ~  c8_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_c8 <- summary.manyglm(model_c8, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_c8)
model_c8$coefficients
plot(model_c8)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_c8,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Residential
model_Residential <- manyglm(Y ~  Residential_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_Residential <- summary.manyglm(model_Residential, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_Residential)
model_Residential$coefficients
plot(model_Residential)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_Residential,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Workplace
model_Workplace <- manyglm(Y ~  Workplace_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_Workplace <- summary.manyglm(model_Workplace, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_Workplace)
model_Workplace$coefficients
plot(model_Workplace)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_Workplace,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Transit
model_Transit <- manyglm(Y ~  Transit_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_Transit <- summary.manyglm(model_Transit, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_Transit)
model_Transit$coefficients
plot(model_Transit)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_Transit,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Park
model_Park <- manyglm(Y ~  Park_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_Park <- summary.manyglm(model_Park, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_Park)
model_Park$coefficients
plot(model_Park)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_Park,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Grocery
model_Grocery <- manyglm(Y ~  Grocery_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_Grocery <- summary.manyglm(model_Grocery, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_Grocery)
model_Grocery$coefficients
plot(model_Grocery)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_Grocery,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# Retail
model_Retail <- manyglm(Y ~  Retail_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_Retail <- summary.manyglm(model_Retail, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_Retail)
model_Retail$coefficients
plot(model_Retail)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_Retail,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)

# non_Residential
model_non_Residential <- manyglm(Y ~  non_Residential_cb + offset(log(P1)), family="negative.binomial",data=df)
summary_non_Residential <- summary.manyglm(model_non_Residential, test = 'wald', nBoot = 1000)

# View the regression coefficients of the variables in the results
# names(model_non_Residential)
model_non_Residential$coefficients
plot(model_non_Residential)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_non_Residential,type="deviance")
hist(res2,xlab="Residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)
