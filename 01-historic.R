#########################################################################################################################################
# R script to prepare data and lagged variables for INLA-DLNM modelling
# install INLA
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)


# load INLA
library(INLA)

#  select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes","splines")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

#########################################################################################################################################
# load shape file for research
map <- read_sf("00.data/ne_10m_admin_0_contries/ne_10m_admin_0_countries.shp")
# dim(map)

# create adjacency matrix
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "00.data/map.graph"
if (!file.exists(g.file)) nb2INLA(g.file, nb.map)

#########################################################################################################################################
# input case data
data<-read.csv('00.data/00.data-2014-2020.csv')

#set max lag
nlag<-3

# creating lagged variables
# define the monthly mean climate variable lag term matrix

# 2m temperature
lag_mt <- tsModel::Lag(data$mt, group = data$country_index, k = 0:nlag)

# Skin temperature
lag_st <- tsModel::Lag(data$st, group = data$country_index, k = 0:nlag)

# Realtive humidity
lag_rh <- tsModel::Lag(data$rh, group = data$country_index, k = 0:nlag)

# Convective precipitation
lag_cp <- tsModel::Lag(data$cp, group = data$country_index, k = 0:nlag)

# Total precipitation
lag_tp <- tsModel::Lag(data$tp, group = data$country_index, k = 0:nlag)

# cross basis matrix
# Set the lag section
lagknot = equalknots(0:nlag, 2)

# 2m temperature
var <- lag_mt
basis_mt <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$mt, 1)),
                       arglag = list(fun = "ns", knots = lagknot))

# Skin temperature
var <- lag_st
basis_st <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$st, 1)),
                       arglag = list(fun = "ns", knots = lagknot))

# Realtive humidit
var <- lag_rh
basis_rh <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$ rh, 1)),
                       arglag = list(fun = "ns", knots = lagknot))

# Convective precipitation
var <- lag_cp
basis_cp <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$cp, 1)),
                       arglag = list(fun = "ns", knots = lagknot))

# Total precipitation
var <- lag_tp
basis_tp <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$tp, 1)),
                       arglag = list(fun = "ns", knots = lagknot))

# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_mt) = paste0("basis_mt.", colnames(basis_mt))
colnames(basis_st) = paste0("basis_st.", colnames(basis_st))
colnames(basis_rh) = paste0("basis_rh.", colnames(basis_rh))
colnames(basis_cp) = paste0("basis_cp.", colnames(basis_cp))
colnames(basis_tp) = paste0("basis_tp.", colnames(basis_tp))

#########################################################################################################################################
# set up data and priors for INLA model
# set data for models
Y  <- data$dengue_case # response variable
E  <- data$population/10^5 # model offset so that response is equivalent to an incidence rate per 100,000 people
# random variable
T1 <- data$month # for random effect to account for annual cycle (seasonality)
T2 <- data$year_index # for random effect to account for inter-annual variability
S <- data$country_index # for country interaction with month and country spatial random effect
# Other covariables
A <- data$proportion_annual # The proportion of cases reported in last year among all cases reported during 2013-2019
GDP <- data$GDP_per_capita #GDP per capita

# create dataframe for model testing
df <- data.frame(Y, E, T1, T2, S, A, GDP)

# define priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# inla model function

# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE)
  
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

#########################################################################################################################################
# run models of increasing complexity in INLA
# fit a baseline model including spatiotemporal random effects
#########################################################################################################################################
# formulate a base model including: 
# state-specific monthly random effects to account for variation in seasonality between countries (random walk cyclic prior)
# year-specific spatial random effects to account for interannual variation in spatial overdisperson and dependency structures (modified Besag-York-Mollie prior bym2)
# baseline model
# except A SEA
baseformula <- Y ~ 1 + f(T1, replicate = S, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior) +
  f(S, model = "bym2", replicate = T2, graph = "00.data/map.graph",
    scale.model = TRUE, hyper = precision.prior) + A

###############################################################################################
# Non-GDP #
formula_1.1 <- update.formula(baseformula, ~. + basis_mt)
formula_1.2 <- update.formula(baseformula, ~. + basis_st)
formula_1.3 <- update.formula(baseformula, ~. + basis_rh)
formula_1.4 <- update.formula(baseformula, ~. + basis_tp)
formula_1.5 <- update.formula(baseformula, ~. + basis_cp)
formula_2.1 <- update.formula(baseformula, ~. + basis_mt + basis_rh)
formula_2.2 <- update.formula(baseformula, ~. + basis_mt + basis_tp)
formula_2.3 <- update.formula(baseformula, ~. + basis_mt + basis_cp)
formula_2.4 <- update.formula(baseformula, ~. + basis_st + basis_rh)
formula_2.5 <- update.formula(baseformula, ~. + basis_st + basis_tp)
formula_2.6 <- update.formula(baseformula, ~. + basis_st + basis_cp)
formula_2.7 <- update.formula(baseformula, ~. + basis_rh + basis_tp)
formula_2.8 <- update.formula(baseformula, ~. + basis_rh + basis_cp)
formula_3.1 <- update.formula(baseformula, ~. + basis_mt + basis_rh + basis_tp)
formula_3.2 <- update.formula(baseformula, ~. + basis_mt + basis_rh + basis_cp)
formula_3.3 <- update.formula(baseformula, ~. + basis_st + basis_rh + basis_tp)
formula_3.4 <- update.formula(baseformula, ~. + basis_st + basis_rh + basis_cp)

# GDP #
baseformula_GDP <- update.formula(baseformula, ~. + GDP)
formula_GDP_1.1 <- update.formula(baseformula, ~. + basis_mt + GDP)
formula_GDP_1.2 <- update.formula(baseformula, ~. + basis_st + GDP)
formula_GDP_1.3 <- update.formula(baseformula, ~. + basis_rh + GDP)
formula_GDP_1.4 <- update.formula(baseformula, ~. + basis_tp + GDP)
formula_GDP_1.5 <- update.formula(baseformula, ~. + basis_cp + GDP)

formula_GDP_2.1 <- update.formula(baseformula, ~. + basis_mt + basis_rh + GDP)
formula_GDP_2.2 <- update.formula(baseformula, ~. + basis_mt + basis_tp + GDP)
formula_GDP_2.3 <- update.formula(baseformula, ~. + basis_mt + basis_cp + GDP)
formula_GDP_2.4 <- update.formula(baseformula, ~. + basis_st + basis_rh + GDP)
formula_GDP_2.5 <- update.formula(baseformula, ~. + basis_st + basis_tp + GDP)
formula_GDP_2.6 <- update.formula(baseformula, ~. + basis_st + basis_cp + GDP)
formula_GDP_2.7 <- update.formula(baseformula, ~. + basis_rh + basis_tp + GDP)
formula_GDP_2.8 <- update.formula(baseformula, ~. + basis_rh + basis_cp + GDP)

formula_GDP_3.1 <- update.formula(baseformula, ~. + basis_mt + basis_rh + basis_tp + GDP)
formula_GDP_3.2 <- update.formula(baseformula, ~. + basis_mt + basis_rh + basis_cp + GDP)
formula_GDP_3.3 <- update.formula(baseformula, ~. + basis_st + basis_rh + basis_tp + GDP)
formula_GDP_3.4 <- update.formula(baseformula, ~. + basis_st + basis_rh + basis_cp + GDP)

# create a list of formulas
formulas <- list(baseformula, formula_1.1, formula_1.2, formula_1.3, formula_1.4, formula_1.5,
                 formula_2.1, formula_2.2, formula_2.3, formula_2.4, formula_2.5, formula_2.6, formula_2.7, formula_2.8,
                 formula_3.1, formula_3.2, formula_3.3, formula_3.4,
                 
                 baseformula_GDP, formula_GDP_1.1, formula_GDP_1.2, formula_GDP_1.3, formula_GDP_1.4, formula_GDP_1.5,
                 formula_GDP_2.1, formula_GDP_2.2, formula_GDP_2.3, formula_GDP_2.4, formula_GDP_2.5, formula_GDP_2.6, formula_GDP_2.7, formula_GDP_2.8,
                 formula_GDP_3.1, formula_GDP_3.2, formula_GDP_3.3, formula_GDP_3.4)
# create model label string
lab <- c("basemodel", "model_1.1", "model_1.2", "model_1.3", "model_1.4", "model_1.5",
         "model_2.1", "model_2.2", "model_2.3", "model_2.4", "model_2.5", "model_2.6", "model_2.7", "model_2.8",
         "model_3.1", "model_3.2", "model_3.3", "model_3.4",
         
         "basemodel_GDP", "model_GDP_1.1", "model_GDP_1.2", "model_GDP_1.3", "model_GDP_1.4", "model_GDP_1.5",
         "model_GDP_2.1", "model_GDP_2.2", "model_GDP_2.3", "model_GDP_2.4", "model_GDP_2.5", "model_GDP_2.6", "model_GDP_2.7", "model_GDP_2.8",
         "model_GDP_3.1", "model_GDP_3.2", "model_GDP_3.3", "model_GDP_3.4")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("01.output/1.history_model/", lab[i],".RData"))})

# create table to store DIC and select best model 
table0 <- data.table(Model  = c("base", "mt", "st", "rh", "tp", "cp",
                                "mt+rh","mt+tp","mt+cp", "st+rh","st+tp","st+cp", "rh+tp", "rh+cp",
                                "mt+rh+tp", "mt+rh+cp","st+rh+tp", "st+rh+cp",
                                "base_GDP", "mt_GDP", "st_GDP", "rh_GDP", "tp_GDP", "cp_GDP",
                                "mt+rh_GDP","mt+tp_GDP","mt+cp_GDP", "st+rh_GDP","st+tp_GDP","st+cp_GDP", "rh+tp_GDP", "rh+cp_GDP",
                                "mt+rh+tp_GDP", "mt+rh+cp_GDP","st+rh+tp_GDP", "st+rh+cp_GDP"), 
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
{
  load(paste0("01.output/1.history_model/",lab[i],".RData"))
  table0$DIC[i] <- round(model$dic$dic, 2)
  table0$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}



# View table
table0

# define position of best fitting model
best.fit <- which.min(table0$DIC)
# Write results of model selection
fwrite(table0, file = "01.output/1.history_model/history_model_selection.csv", quote = FALSE, 
       row.names = FALSE) 

#########################################################################################################################################
# To predict the best "historical" model for the number of dengue cases in 2020
#########################################################################################################################################
# load best models
load("01.output/1.history_model/model_2.6.RData")
# fitting and predicting results
pre_case <- model$summary.fitted.values$mean

# save the prediction results
attach(data)
data$pre_case <- pre_case

write.csv(data,file = "00.data/pre-dengue.csv",row.names = F)
