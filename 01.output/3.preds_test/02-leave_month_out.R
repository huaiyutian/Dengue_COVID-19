# !/usr/bin/env Rscript


for(yyear in 1:6){
  for(mmonth in 1:12){
  
#inla.setOption(num.threads = "4:1")


# Script to run INLA models in cross validation prediction mode

# Step 0: load packages pre-processed data and functions
# Step 1: rerun the selected model (fitted with config = TRUE for sampling) 
# Step 2: produce cross-validated posterior predictive samples leaving out one year and one month at a time

# Step 0: load packages and pre-processed data
source("01.output/6.preds_test/01-historic-load-all.R")

# Step 2: produce cross-validated posterior predictive samples leaving out one year and one month at a time

# define number of samples
s <- 1000

# replace dengue data in testing period with NA for out of sample prediction
casestopred <- data$dengue_case # response variable
idx.pred <- which(data$year_index == yyear & data$month == mmonth)
casestopred[idx.pred] <- NA # replace cases in year and month of interest to NA
mpred <- length(idx.pred)
    
    # set response variable and year indicator
    df$Y <- casestopred
    
    mymodel <- function(formula, data = df, family = "nbinomial", config = TRUE)
      
    {
      model <- inla(formula = formula, data = data, family = family, offset = log(E),
                    control.inla = list(strategy = 'adaptive'), 
                    control.compute = list(dic = TRUE, config = config, 
                                           cpo = TRUE, return.marginals = FALSE),
                    control.fixed = list(correlation.matrix = TRUE, 
                                         prec.intercept = 1, prec = 1),
                    control.predictor = list(link = 1, compute = TRUE), 
                    verbose = FALSE)
      model <- inla.rerun(model)
      return(model)
    }
    
    baseformula <- Y ~ 1 + f(T1, replicate = S, model = "rw1", cyclic = TRUE, constr = TRUE,
                             scale.model = TRUE,  hyper = precision.prior) +
      f(S, model = "bym2", replicate = T2, graph = "00.data/map.graph",
        scale.model = TRUE, hyper = precision.prior) + A 
    
    formula_2.6 <- update.formula(baseformula, ~. + basis_st + basis_cp)
    
    model <- mymodel(formula_2.6, df)
    
    
    
    xx <- inla.posterior.sample(s, model)
    xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[idx.pred]), xx)
    y.pred <- matrix(NA, mpred, s)
    for(s.idx in 1:s) {
        xx.sample <- xx.s[, s.idx]
        y.pred[, s.idx] <- rnbinom(mpred, mu = exp(xx.sample[-1]), size = xx.sample[1])
    }
    preds <- list(year = 2013 + yyear, month = mmonth, idx.pred = idx.pred, 
                  mean = apply(y.pred, 1, mean), median = apply(y.pred, 1, median),
                  lci = apply(y.pred, 1, quantile, probs = c(0.025)),
                  uci = apply(y.pred, 1, quantile, probs = c(0.975)))
    save(preds, file = paste0("01.output/3.preds_test/preds_2014-2019/preds_all_",2013 + yyear, "_", mmonth, ".RData"))
  }
}

