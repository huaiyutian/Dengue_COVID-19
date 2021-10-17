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

# Create adjacency matrix
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "00.data/map.graph"
if (!file.exists(g.file)) nb2INLA(g.file, nb.map)

#########################################################################################################################################
# input case data
data<-read.csv('00.data/00.data-2014-2019.csv')

#set max lag
nlag<-3

# Creating lagged variables
# Define the monthly mean climate variable lag term matrix

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
