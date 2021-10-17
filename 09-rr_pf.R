# Look at the relative risk of specific metrics
RR-c1 <-cbind(c1_cp$allRRfit,c1_cp$allRRlow,c1_cp$allRRhigh)
RR-non_Residential <-cbind(non_Residential_cp$allRRfit,non_Residential_cp$allRRlow,non_Residential_cp$allRRhigh)
# Dengue Fever Attribution Fraction
# Load the function attrdl 

# SELECT REGION
## STUDY AREA
reg_all <- "all"

## REGION
reg <- "SEA"

## COUNTRY
reg_c <- "Thailand"


# CREATE A LIST WITH THE REGIONAL SERIES
## STUDY AREA
area_all <- as.character(unique(data_2020$area))
data <- lapply(area_all,function(x) data_2020[data_2020$area==x,])
names(data) <- area_all
m <- length(area_all)
df=data[[reg_all]]

## REGION
regions_reg <- as.character(unique(data_2020$regnames))
data_reg <- lapply(regions_reg,function(x) data_2020[data_2020$regnames==x,])
names(data_reg) <- regions_reg
m_reg <- length(regions_reg)
df_reg=data_reg[[reg]]

## COUNTRY
regions_c <- as.character(unique(data_2020$country))
data_c <- lapply(regions_c,function(x) data_2020[data_2020$country==x,])
names(data_c) <- regions_c
m_c <- length(regions_c)
df_c=data_c[[reg_c]]


source("attrdl.R")

# Calculate the region of PF
vars <- c("Belize",
          "Bolivia",
          "Brazil",
          "Colombia",
          "Costa Rica",
          "Dominican Republic",
          "Ecuador",
          "El Salvador",
          "Guatemala",
          "Honduras",
          "Jamaica",
          "Mexico",
          "Nicaragua",
          "Panama",
          "Peru",
          "Venezuela",
          "Cambodia",
          "Laos",
          "Malaysia",
          "Philippines",
          "Singapore",
          "Thailand",
          "Vietnam",
          
          "LA",
          "SEA",
          
          "all")
result <- c()
for(i in 1:26){
  reg = vars[i]
  {
    if (i <= 23)
      df  = data_c[[reg]]
    else if (i >= 24 && i <=25)
      df  = data_reg[[reg]]
    else
      df  = data[[reg]]
    
  }
  

# c1
# Central value (and percentile)
cen_c1 <- 0
min_c1 <- min(df$c1, na.rm = FALSE)
max_c1 <- max(df$c1, na.rm = FALSE)

sum(df$c1<cen_c1,na.rm=T)/sum(!is.na(df$c1))

ind <- c(2,3,4,5)

#
# All si Forward Attribution Risk (Quantity and Proportion)
total_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1)
total_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1),c(0.05,0.95))

# Attribute to low c1 fractions
low_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(min_c1,cen_c1))
low_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(min_c1,cen_c1)),c(0.05,0.95))

# Attribute to high c1  fractions
high_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(cen_c1,max_c1))
high_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(cen_c1,max_c1)),c(0.05,0.95))
# COMPARE WITH TABLE 1

# Attribute to c1：0-10 fractions
g0_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(0,10))
g0_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(0,10)),c(0.05,0.95))

# Attribute to c1：10-20 fractions
g1_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(10,20))
g1_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(10,20)),c(0.05,0.95))

# Attribute to c1：20-30 fractions
g2_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(20,30))
g2_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(20,30)),c(0.05,0.95))

# Attribute to c1：30-40 fractions
g3_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(30,40))
g3_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(30,40)),c(0.05,0.95))

# Attribute to c1：40-50 fractions
g4_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(40,50))
g4_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(40,50)),c(0.05,0.95))

# Attribute to c1：50-60 fractions
g5_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(50,60))
g5_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(50,60)),c(0.05,0.95))

# Attribute to c1：60-70 fractions
g6_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(60,70))
g6_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(60,70)),c(0.05,0.95))

# Attribute to c1：70-80 fractions
g7_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(70,80))
g7_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(70,80)),c(0.05,0.95))

# Attribute to c1：80-90 fractions
g8_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(80,90))
g8_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(80,90)),c(0.05,0.95))

# Attribute to c1：90-100 fractions
g9_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(90,100))
g9_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(90,100)),c(0.05,0.95))

# Attribute to c1：100-110 fractions
g10_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(100,110))
g10_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(100,110)),c(0.05,0.95))

# Attribute to c1：110-120 fractions
g11_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(110,120))
g11_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(110,120)),c(0.05,0.95))

# Attribute to c1：120-130 fractions
g12_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(120,130))
g12_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(120,130)),c(0.05,0.95))

# Attribute to c1：130-140 fractions
g13_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],dir="forw",cen=cen_c1,range=c(130,140))
g13_ice_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(130,140)),c(0.05,0.95))

###Total-------------------------------------------AN
# All si Forward Attribution Risk (Quantity and Proportion)
total_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1)
total_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1),c(0.05,0.95))

# Attribute to low c1 fractions
low_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(min_c1,cen_c1))
low_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(min_c1,cen_c1)),c(0.05,0.95))

# Attribute to low c1 fractions
high_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(cen_c1,max_c1))
high_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(cen_c1,max_c1)),c(0.05,0.95))
# COMPARE WITH TABLE 1

# Attribute to  c1：0-10 fractions
g0_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(0,10))
g0_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(0,10)),c(0.05,0.95))

# Attribute to  c1：10-20 fractions
g1_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(10,20))
g1_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(10,20)),c(0.05,0.95))

# Attribute to  c1：20-30 fractions
g2_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(20,30))
g2_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(20,30)),c(0.05,0.95))

# Attribute to  c1：30-40 fractions
g3_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(30,40))
g3_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(30,40)),c(0.05,0.95))

# Attribute to  c1：40-50 fractions
g4_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(40,50))
g4_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(40,50)),c(0.05,0.95))

# Attribute to  c1：50-60 fractions
g5_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(50,60))
g5_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(50,60)),c(0.05,0.95))

# Attribute to  c1：60-70 fractions
g6_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(60,70))
g6_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(60,70)),c(0.05,0.95))

# Attribute to  c1：70-80 fractions
g7_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(70,80))
g7_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(70,80)),c(0.05,0.95))

# Attribute to  c1：80-90 fractions
g8_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(80,90))
g8_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(80,90)),c(0.05,0.95))

# Attribute to  c1：90-100 fractions
g9_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(90,100))
g9_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(90,100)),c(0.05,0.95))

# Attribute to  c1：100-110 fractions
g10_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(100,110))
g10_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(100,110)),c(0.05,0.95))

# Attribute to  c1：110-120 fractions
g11_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(110,120))
g11_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(110,120)),c(0.05,0.95))

# Attribute to  c1：120-130 fractions
g12_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(120,130))
g12_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(120,130)),c(0.05,0.95))

# Attribute to  c1：130-140 fractions
g13_num_c1 <- attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",dir="forw",cen=cen_c1,range=c(130,140))
g13_ice_num_c1 <- quantile(attrdl(df$c1,c1_cb,df$dengue_case,coef = coef_c1[ind], vcov=vcov_c1[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_c1,range=c(130,140)),c(0.05,0.95))

# non_Residential
# Central value (and percentile)
cen_non_Residential <- 100
min_non_Residential <- min(df$non_Residential, na.rm = FALSE)
max_non_Residential <- max(df$non_Residential, na.rm = FALSE)

sum(df$non_Residential<cen_non_Residential,na.rm=T)/sum(!is.na(df$non_Residential))

#
# All non_Residential Forward Attribution Risk (Quantity and Proportion)
total_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential)
total_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential),c(0.05,0.95))

# Attribute to low non_Residential fractions
low_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(min_non_Residential,cen_non_Residential))
low_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(min_non_Residential,cen_non_Residential)),c(0.05,0.95))

# Attribute to high non_Residential  fractions
high_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(cen_non_Residential,max_non_Residential))
high_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(cen_non_Residential,max_non_Residential)),c(0.05,0.95))
# COMPARE WITH TABLE 1

# Attribute to non_Residential：0-10 fractions
g0_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(0,10))
g0_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(0,10)),c(0.05,0.95))

# Attribute to non_Residential：10-20 fractions
g1_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(10,20))
g1_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(10,20)),c(0.05,0.95))

# Attribute to non_Residential：20-30 fractions
g2_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(20,30))
g2_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(20,30)),c(0.05,0.95))

# Attribute to non_Residential：30-40 fractions
g3_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(30,40))
g3_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(30,40)),c(0.05,0.95))

# Attribute to non_Residential：40-50 fractions
g4_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(40,50))
g4_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(40,50)),c(0.05,0.95))

# Attribute to non_Residential：50-60 fractions
g5_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(50,60))
g5_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(50,60)),c(0.05,0.95))

# Attribute to non_Residential：60-70 fractions
g6_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(60,70))
g6_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(60,70)),c(0.05,0.95))

# Attribute to non_Residential：70-80 fractions
g7_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(70,80))
g7_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(70,80)),c(0.05,0.95))

# Attribute to non_Residential：80-90 fractions
g8_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(80,90))
g8_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(80,90)),c(0.05,0.95))

# Attribute to non_Residential：90-100 fractions
g9_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(90,100))
g9_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(90,100)),c(0.05,0.95))

# Attribute to non_Residential：100-110 fractions
g10_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(100,110))
g10_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(100,110)),c(0.05,0.95))

# Attribute to non_Residential：110-120 fractions
g11_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(110,120))
g11_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(110,120)),c(0.05,0.95))

# Attribute to non_Residential：120-130 fractions
g12_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(120,130))
g12_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(120,130)),c(0.05,0.95))

# Attribute to non_Residential：130-140 fractions
g13_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],dir="forw",cen=cen_non_Residential,range=c(130,140))
g13_ice_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(130,140)),c(0.05,0.95))

###Total-------------------------------------------AN
# All non_Residential Forward Attribution Risk (Quantity and Proportion)
total_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential)
total_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential),c(0.05,0.95))

# Attribute to low non_Residential fractions
low_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(min_non_Residential,cen_non_Residential))
low_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(min_non_Residential,cen_non_Residential)),c(0.05,0.95))

# Attribute to high non_Residential fractions
high_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(cen_non_Residential,max_non_Residential))
high_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(cen_non_Residential,max_non_Residential)),c(0.05,0.95))
# COMPARE WITH TABLE 1

# Attribute to  non_Residential：0-10 fractions
g0_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(0,10))
g0_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(0,10)),c(0.05,0.95))

# Attribute to  non_Residential：10-20 fractions
g1_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(10,20))
g1_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(10,20)),c(0.05,0.95))

# Attribute to  non_Residential：20-30 fractions
g2_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(20,30))
g2_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(20,30)),c(0.05,0.95))

# Attribute to  non_Residential：30-40 fractions
g3_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(30,40))
g3_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(30,40)),c(0.05,0.95))

# Attribute to  non_Residential：40-50 fractions
g4_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(40,50))
g4_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(40,50)),c(0.05,0.95))

# Attribute to  non_Residential：50-60 fractions
g5_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(50,60))
g5_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(50,60)),c(0.05,0.95))

# Attribute to  non_Residential：60-70 fractions
g6_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(60,70))
g6_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(60,70)),c(0.05,0.95))

# Attribute to  non_Residential：70-80 fractions
g7_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(70,80))
g7_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(70,80)),c(0.05,0.95))

# Attribute to  non_Residential：80-90 fractions
g8_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(80,90))
g8_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(80,90)),c(0.05,0.95))

# Attribute to  non_Residential：90-100 fractions
g9_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(90,100))
g9_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(90,100)),c(0.05,0.95))

# Attribute to  non_Residential：100-110 fractions
g10_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(100,110))
g10_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(100,110)),c(0.05,0.95))

# Attribute to  non_Residential：110-120 fractions
g11_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(110,120))
g11_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(110,120)),c(0.05,0.95))

# Attribute to  non_Residential：120-130 fractions
g12_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(120,130))
g12_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(120,130)),c(0.05,0.95))

# Attribute to  non_Residential：130-140 fractions
g13_num_non_Residential <- attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",dir="forw",cen=cen_non_Residential,range=c(130,140))
g13_ice_num_non_Residential <- quantile(attrdl(df$non_Residential,non_Residential_cb,df$dengue_case,coef = coef_non_Residential[ind], vcov=vcov_non_Residential[ind,ind],type="an",sim=T,nsim=1000,dir="forw",cen=cen_non_Residential,range=c(130,140)),c(0.05,0.95))

# Save to file
AAN <- data.frame(country = c(reg,reg,reg,reg,reg,reg),
                  var = c("c1","c1_min","c1_max","non_Residential","non_Residential_min","non_Residential_max"),
                  describe = c( cen_c1 , min_c1 ,max_c1,cen_non_Residential , min_non_Residential ,max_non_Residential),
                  total = c(total_c1/(total_c1-1)*100, total_ice_c1/(total_ice_c1-1)*100, total_non_Residential/(total_non_Residential-1)*100, total_ice_non_Residential/(total_ice_non_Residential-1)*100),
                  low = c(low_c1/(low_c1-1)*100, low_ice_c1/(low_ice_c1-1)*100, low_non_Residential/(low_non_Residential-1)*100, low_ice_non_Residential/(low_ice_non_Residential-1)*100),
                  high = c(high_c1/(high_c1-1)*100, high_ice_c1/(high_ice_c1-1)*100, high_non_Residential/(high_non_Residential-1)*100, high_ice_non_Residential/(high_ice_non_Residential-1)*100),
                  g0 = c(g0_c1/(g0_c1-1)*100, g0_ice_c1/(g0_ice_c1-1)*100, g0_non_Residential/(g0_non_Residential-1)*100, g0_ice_non_Residential/(g0_ice_non_Residential-1)*100),
                  g1 = c(g1_c1/(g1_c1-1)*100, g1_ice_c1/(g1_ice_c1-1)*100, g1_non_Residential/(g1_non_Residential-1)*100, g1_ice_non_Residential/(g1_ice_non_Residential-1)*100),
                  g2 = c(g2_c1/(g2_c1-1)*100, g2_ice_c1/(g2_ice_c1-1)*100, g2_non_Residential/(g2_non_Residential-1)*100, g2_ice_non_Residential/(g2_ice_non_Residential-1)*100),
                  g3 = c(g3_c1/(g3_c1-1)*100, g3_ice_c1/(g3_ice_c1-1)*100, g3_non_Residential/(g3_non_Residential-1)*100, g3_ice_non_Residential/(g3_ice_non_Residential-1)*100),
                  g4 = c(g4_c1/(g4_c1-1)*100, g4_ice_c1/(g4_ice_c1-1)*100, g4_non_Residential/(g4_non_Residential-1)*100, g4_ice_non_Residential/(g4_ice_non_Residential-1)*100),
                  g5 = c(g5_c1/(g5_c1-1)*100, g5_ice_c1/(g5_ice_c1-1)*100, g5_non_Residential/(g5_non_Residential-1)*100, g5_ice_non_Residential/(g5_ice_non_Residential-1)*100),
                  g6 = c(g6_c1/(g6_c1-1)*100, g6_ice_c1/(g6_ice_c1-1)*100, g6_non_Residential/(g6_non_Residential-1)*100, g6_ice_non_Residential/(g6_ice_non_Residential-1)*100),
                  g7 = c(g7_c1/(g7_c1-1)*100, g7_ice_c1/(g7_ice_c1-1)*100, g7_non_Residential/(g7_non_Residential-1)*100, g7_ice_non_Residential/(g7_ice_non_Residential-1)*100),
                  g8 = c(g8_c1/(g8_c1-1)*100, g8_ice_c1/(g8_ice_c1-1)*100, g8_non_Residential/(g8_non_Residential-1)*100, g8_ice_non_Residential/(g8_ice_non_Residential-1)*100),
                  g9 = c(g9_c1/(g9_c1-1)*100, g9_ice_c1/(g9_ice_c1-1)*100, g9_non_Residential/(g9_non_Residential-1)*100, g9_ice_non_Residential/(g9_ice_non_Residential-1)*100),
                  g10 = c(g10_c1/(g10_c1-1)*100, g10_ice_c1/(g10_ice_c1-1)*100, g10_non_Residential/(g10_non_Residential-1)*100, g10_ice_non_Residential/(g10_ice_non_Residential-1)*100),
                  g11 = c(g11_c1/(g11_c1-1)*100, g11_ice_c1/(g11_ice_c1-1)*100, g11_non_Residential/(g11_non_Residential-1)*100, g11_ice_non_Residential/(g11_ice_non_Residential-1)*100),
                  g12 = c(g12_c1/(g12_c1-1)*100, g12_ice_c1/(g12_ice_c1-1)*100, g12_non_Residential/(g12_non_Residential-1)*100, g12_ice_non_Residential/(g12_ice_non_Residential-1)*100),
                  g13 = c(g13_c1/(g13_c1-1)*100, g13_ice_c1/(g13_ice_c1-1)*100, g13_non_Residential/(g13_non_Residential-1)*100, g13_ice_non_Residential/(g13_ice_non_Residential-1)*100),

                  total_num = c(  total_num_c1 , total_ice_num_c1,total_num_non_Residential , total_ice_num_non_Residential),
                  low_num = c(  low_num_c1 , low_ice_num_c1,low_num_non_Residential , low_ice_num_non_Residential),
                  high_num = c(  high_num_c1 , high_ice_num_c1,high_num_non_Residential , high_ice_num_non_Residential),
                  g0_num = c( g0_num_c1 , g0_ice_num_c1,g0_num_non_Residential , g0_ice_num_non_Residential),
                  g1_num = c( g1_num_c1 , g1_ice_num_c1,g1_num_non_Residential , g1_ice_num_non_Residential),
                  g2_num = c( g2_num_c1 , g2_ice_num_c1,g2_num_non_Residential , g2_ice_num_non_Residential),
                  g3_num = c( g3_num_c1 , g3_ice_num_c1,g3_num_non_Residential , g3_ice_num_non_Residential),
                  g4_num = c( g4_num_c1 , g4_ice_num_c1,g4_num_non_Residential , g4_ice_num_non_Residential),
                  g5_num = c( g5_num_c1 , g5_ice_num_c1,g5_num_non_Residential , g5_ice_num_non_Residential),
                  g6_num = c( g6_num_c1 , g6_ice_num_c1,g6_num_non_Residential , g6_ice_num_non_Residential),
                  g7_num = c( g7_num_c1 , g7_ice_num_c1,g7_num_non_Residential , g7_ice_num_non_Residential),
                  g8_num = c( g8_num_c1 , g8_ice_num_c1,g8_num_non_Residential , g8_ice_num_non_Residential),
                  g9_num = c( g9_num_c1 , g9_ice_num_c1,g9_num_non_Residential , g9_ice_num_non_Residential),
                  g10_num = c( g10_num_c1 , g10_ice_num_c1,g10_num_non_Residential , g10_ice_num_non_Residential),
                  g11_num = c( g11_num_c1 , g11_ice_num_c1,g11_num_non_Residential , g11_ice_num_non_Residential),
                  g12_num = c( g12_num_c1 , g12_ice_num_c1,g12_num_non_Residential , g12_ice_num_non_Residential),
                  g13_num = c( g13_num_c1 , g13_ice_num_c1,g13_num_non_Residential , g13_ice_num_non_Residential)
                  
)
result <- rbind(result,AAN)
}

write.csv(result,file="01.output/2.intervention_model/02.PF.csv")
