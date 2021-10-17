# Load packages (assumed already installed)
rm(list = ls())
library(mvabund);library(dlnm);library(MASS)

####################################################################
# load the dataset
data_2020 <- read.csv("00.data/01.data2020.csv")

# Set maximum lag
nlag = 3

# Creating lagged variables
# Define the monthly mean intervention variable lag term matrix
lag_si <- tsModel::Lag(data_2020$si, group = data_2020$country_index, k = 0:nlag)
lag_c1 <- tsModel::Lag(data_2020$c1, group = data_2020$country_index, k = 0:nlag)
lag_c2 <- tsModel::Lag(data_2020$c2, group = data_2020$country_index, k = 0:nlag)
lag_c3 <- tsModel::Lag(data_2020$c3, group = data_2020$country_index, k = 0:nlag)
lag_c4 <- tsModel::Lag(data_2020$c4, group = data_2020$country_index, k = 0:nlag)
lag_c5 <- tsModel::Lag(data_2020$c5, group = data_2020$country_index, k = 0:nlag)
lag_c6 <- tsModel::Lag(data_2020$c6, group = data_2020$country_index, k = 0:nlag)
lag_c7 <- tsModel::Lag(data_2020$c7, group = data_2020$country_index, k = 0:nlag)
lag_c8 <- tsModel::Lag(data_2020$c8, group = data_2020$country_index, k = 0:nlag)

lag_non_Residential <- tsModel::Lag(data_2020$non_Residential, group = data_2020$country_index, k = 0:nlag)
lag_Residential <- tsModel::Lag(data_2020$Residential, group = data_2020$country_index, k = 0:nlag)
lag_Workplace <- tsModel::Lag(data_2020$Workplace, group = data_2020$country_index, k = 0:nlag)
lag_Transit <- tsModel::Lag(data_2020$Transit, group = data_2020$country_index, k = 0:nlag)
lag_Park <- tsModel::Lag(data_2020$Park, group = data_2020$country_index, k = 0:nlag)
lag_Grocery <- tsModel::Lag(data_2020$Grocery, group = data_2020$country_index, k = 0:nlag)
lag_Retail <- tsModel::Lag(data_2020$Retail, group = data_2020$country_index, k = 0:nlag)

# Define cross basis matrix (combining nonlinear exposure and hysteresis function)
# Set the lag section
lagknot = equalknots(0:nlag, 2)

var <- lag_si
si_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$si,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))
summary(si_cb)

var <- lag_c1
c1_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c1,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))

var <- lag_c2
c2_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c2,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))

var <- lag_c3
c3_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c3,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))

var <- lag_c4
c4_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c4,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))
var <- lag_c5
c5_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c5,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))
var <- lag_c6
c6_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c6,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))

var <- lag_c7
c7_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c7,na.rm=T)),
                    arglag = list(fun = "ns",  knots = lagknot))

var <- lag_c8
c8_cb <- crossbasis(var,
                    argvar = list(fun = "ns", Boundary.knots = range(data_2020$c8,na.rm=T)),
                    arglag = list(fun = "ns", knots = lagknot))

var <- lag_non_Residential
non_Residential_cb <- crossbasis(var,
                                 argvar = list(fun = "ns", Boundary.knots = range(data_2020$non_Residential,na.rm=T)),
                                 arglag = list(fun = "ns", knots = lagknot))

var <- lag_Residential
Residential_cb <- crossbasis(var,
                             argvar = list(fun = "ns", Boundary.knots = range(data_2020$Residential,na.rm=T)),
                             arglag = list(fun = "ns", knots = lagknot))


var <- lag_Workplace
Workplace_cb <- crossbasis(var,
                           argvar = list(fun = "ns", Boundary.knots = range(data_2020$Workplace,na.rm=T)),
                           arglag = list(fun = "ns", knots = lagknot))

var <- lag_Transit
Transit_cb <- crossbasis(var,
                         argvar = list(fun = "ns", Boundary.knots = range(data_2020$Transit,na.rm=T)),
                         arglag = list(fun = "ns", knots = lagknot))

var <- lag_Park
Park_cb <- crossbasis(var,
                      argvar = list(fun = "ns", Boundary.knots = range(data_2020$Park,na.rm=T)),
                      arglag = list(fun = "ns", knots = lagknot))

var <- lag_Grocery
Grocery_cb <- crossbasis(var,
                         argvar = list(fun = "ns", Boundary.knots = range(data_2020$Grocery,na.rm=T)),
                         arglag = list(fun = "ns", knots = lagknot))

var <- lag_Retail
Retail_cb <- crossbasis(var,
                        argvar = list(fun = "ns", Boundary.knots = range(data_2020$Retail,na.rm=T)),
                        arglag = list(fun = "ns", knots = lagknot))

# Specifies a unique column name for the inla() model
# Note: GLM (), GAM () or GLM.nb () models are not required
#colnames(si_cb) = paste0("si_cb.", colnames(si_cb))
#colnames(c1_cb) = paste0("c1_cb.", colnames(c1_cb))
#colnames(c2_cb) = paste0("c2_cb.", colnames(c2_cb))
#colnames(c3_cb) = paste0("c3_cb.", colnames(c3_cb))
#colnames(c4_cb) = paste0("c4_cb.", colnames(c4_cb))
#colnames(c5_cb) = paste0("c5_cb.", colnames(c5_cb))
#colnames(c6_cb) = paste0("c6_cb.", colnames(c6_cb))
#colnames(c7_cb) = paste0("c7_cb.", colnames(c7_cb))
#colnames(c8_cb) = paste0("c8_cb.", colnames(c8_cb))

#colnames(Residential_cb) = paste0("Residential_cb.", colnames(Residential_cb))
#colnames(Workplace_cb) = paste0("Workplace_cb.", colnames(Workplace_cb))
#colnames(Transit_cb) = paste0("Transit_cb.", colnames(Transit_cb))
#colnames(Park_cb) = paste0("Park_cb.", colnames(Park_cb))
#colnames(Grocery_cb) = paste0("Grocery_cb.", colnames(Grocery_cb))
#colnames(Retail_cb) = paste0("Retail_cb.", colnames(Retail_cb))

# set data for models
Y <- data_2020$dengue_case # response variable
N <- length(Y) # total number of data_2020 points
P1 <- data_2020$pre_case # prediction variable

# create dataframe for model testing
df <- data.frame(Y, P1)
