#########################################################################################################################################
# Stepwise methods were used to screen model variables using AIC criterion--all
model_all <- manyglm(Y ~ c1_cb + c2_cb + c3_cb + c4_cb + c5_cb + c6_cb + c7_cb + c8_cb + offset(log(P1)), 
                     family="negative.binomial",
                     data=df)

model_b_all<- stepAIC(model_all,
                      scope = list(upper = ~ c1_cb + c2_cb + c3_cb + c4_cb + c5_cb + c6_cb + c7_cb + c8_cb + offset(log(P1)), 
                                   lower = ~ offset(log(P1))),
                      direction = "backward")
model_b_all$anova
#Residential_cb + Workplace_cb + Transit_cb + Park_cb + Grocery_cb + Retail_cb
# Forward stepwise regression
vars <- c("c1_cb","c2_cb","c3_cb","c4_cb","c5_cb","c6_cb","c7_cb","c8_cb")
result <- c()
for(i in 1:8){
  model <- manyglm(substitute(Y ~ x + offset(log(P1)), list(x=as.name(vars[i]))), 
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result                  

vars <- c("c2_cb","c3_cb","c4_cb","c5_cb","c6_cb","c7_cb","c8_cb")
result <- c()
for(i in 1:7){
  model <- manyglm(substitute(Y ~ x + c1_cb + offset(log(P1)), list(x=as.name(vars[i]))), 
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result 

#########################################################################################################################################
# Stepwise methods were used to screen model variables using AIC criterion--mobility
model_HMB <- manyglm(Y ~ Residential_cb + Workplace_cb + Transit_cb + Park_cb + Grocery_cb + Retail_cb + offset(log(P1)), 
                    family="negative.binomial",
                    data=df)
model_b_HMB<- stepAIC(model_HMB,
                     scope = list(upper = ~ Residential_cb + Workplace_cb + Transit_cb + Park_cb + Grocery_cb + Retail_cb + offset(log(P1)), 
                                  lower = ~ offset(log(P1))),
                     direction = "backward")
model_b_HMB$anova

vars <- c("Residential_cb","Workplace_cb", "Transit_cb", "Park_cb","Grocery_cb","Retail_cb")
result <- c()
for(i in 1:6){
  model <- manyglm(substitute(Y ~ x + offset(log(P1)), list(x=as.name(vars[i]))),
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result

vars <- c("Residential_cb","Workplace_cb", "Transit_cb", "Park_cb","Retail_cb")
result <- c()
for(i in 1:5){
  model <- manyglm(substitute(Y ~ x + Grocery_cb + offset(log(P1)), list(x=as.name(vars[i]))),
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result

vars <- c("Residential_cb","Workplace_cb", "Transit_cb", "Park_cb")
result <- c()
for(i in 1:4){
  model <- manyglm(substitute(Y ~ x + Grocery_cb + Retail_cb + offset(log(P1)), list(x=as.name(vars[i]))),
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result

vars <- c("Residential_cb","Workplace_cb", "Park_cb")
result <- c()
for(i in 1:3){
  model <- manyglm(substitute(Y ~ x + Transit_cb + Grocery_cb + Retail_cb + offset(log(P1)), list(x=as.name(vars[i]))),
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result

vars <- c("Residential_cb","Workplace_cb")
result <- c()
for(i in 1:2){
  model <- manyglm(substitute(Y ~ x + Park_cb + Transit_cb + Grocery_cb + Retail_cb + offset(log(P1)), list(x=as.name(vars[i]))),
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result

#########################################################################################################################################
# Stepwise methods were used to screen model variables using AIC criterion--all
model_all <- manyglm(Y ~ Residential_cb + non_Residential_cb + offset(log(P1)), 
                     family="negative.binomial",
                     data=df)

model_b_all<- stepAIC(model_all,
                      scope = list(upper = ~ Residential_cb + non_Residential_cb + offset(log(P1)), 
                                   lower = ~ offset(log(P1))),
                      direction = "backward")
model_b_all$anova
#Residential_cb + Workplace_cb + Transit_cb + Park_cb + Grocery_cb + Retail_cb
# Forward stepwise regression
vars <- c("Residential_cb", "non_Residential_cb")
result <- c()
for(i in 1:2){
  model <- manyglm(substitute(Y ~ x + offset(log(P1)), list(x=as.name(vars[i]))), 
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result                  

vars <- c("Residential_cb")
result <- c()
for(i in 1:1){
  model <- manyglm(substitute(Y ~ x + non_Residential_cb+ offset(log(P1)), list(x=as.name(vars[i]))), 
                   family="negative.binomial",
                   data=df)
  result <- rbind(result,c(vars[i],model[["aic"]]))
}
result 

#########################################################################################################################################
#rm(list = ls())
#source("03-load data.R")

#Final PHSM model
model_PHSM <- manyglm(Y ~ c1_cb + offset(log(P1)), 
                     family="negative.binomial",
                     data=df)
summary_PHSM <- summary.manyglm(model_PHSM, test = 'wald', nBoot = 1000)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_PHSM,type="deviance")
hist(res2,xlab="residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)


#Final HMB model
model_HMB <- manyglm(Y ~ Park_cb + Transit_cb + Grocery_cb + Retail_cb + offset(log(P1)), 
                    family="negative.binomial",data=df)
summary_HMB <- summary.manyglm(model_HMB, test = 'wald', nBoot = 1000)

# Plot of sensitivity test
# Residual square
res2 <- residuals(model_HMB,type="deviance")
hist(res2,xlab="residuals")
# Q-Q PLOT
qqnorm(res2);qqline(res2)
# Autocorrelation and partial autocorrelation
acf(res2)
pacf(res2)
