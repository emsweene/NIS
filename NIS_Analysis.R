###############################################################################################
## Analysis of NIS patient data
## This code calculates the number of patients in each group for each year (1) AIS, (2) HIV + AIS, 
## (3) AIS, HIV -  and tPA (4) AIS  HIV +  and tPA.  A logisitic regression model is fit with hiv status,
## tpa treatment, and an interaction between hiv and tpa (adjusting for age, gender, and race)
## with death or discharge to hospice as an outcome.   
##
## Elizabeth Sweeney
## July 22nd, 2013
###############################################################################################


###############################################################################################
## Load patient records from all years 
###############################################################################################

setwd('/project/taki2/NIS/NIS_2006')
load('ais_stroke_2006.RData')

setwd('/project/taki2/NIS/NIS_2007')
load('ais_stroke_2007.RData')

setwd('/project/taki2/NIS/NIS_2008')
load('ais_stroke_2008.RData')

setwd('/project/taki2/NIS/NIS_2009')
load('ais_stroke_2009.RData')

setwd('/project/taki2/NIS/NIS_2010')
load('ais_stroke_2010.RData')

###############################################################################################
## combine data from all years and select patients over the age of 16
###############################################################################################

data <- rbind(data_2006, data_2007, data_2008, data_2009, data_2010)
dim(data)
## 944016     24

data_over_16 <- data[as.numeric(data$AGE) > 16,]
dim(data_over_16)
## 941087     24

###############################################################################################
## Information for table 1
###############################################################################################

##AIS Patients (by year)

ais_totals <- cbind(dim(data_over_16[data_over_16$Year == 2006,])[1], 
  dim(data_over_16[data_over_16$Year == 2007,])[1], dim(data_over_16[data_over_16$Year == 2008,])[1], 
  dim(data_over_16[data_over_16$Year == 2009,])[1], dim(data_over_16[data_over_16$Year == 2010,])[1])

print(ais_totals)

##[,1]   [,2]   [,3]   [,4]   [,5]
##[1,] 185874 181393 196398 187183 190239

sum(ais_totals)
## 941087

##AIS and HIV + Patients (by year)

hiv_ais_totals <- cbind(sum(data_over_16$hiv[data_over_16$Year == 2006]), 
  sum(data_over_16$hiv[data_over_16$Year == 2007]), sum(data_over_16$hiv[data_over_16$Year == 2008]), 
  sum(data_over_16$hiv[data_over_16$Year == 2009]), sum(data_over_16$hiv[data_over_16$Year == 2010]))

## [,1] [,2] [,3] [,4] [,5]
##[1,]  556  519  641  568  598

sum(hiv_ais_totals)
[1] 2882

## AIS, HIV -  and tPA

no_hiv_ais_tpa_totals <- cbind(dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2006,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2007,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2008,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2009,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2010,])[1])

print(no_hiv_ais_tpa_totals)
##[,1] [,2] [,3] [,4] [,5]
##[1,] 2702 2975 3714 4400 5549

sum(no_hiv_ais_tpa_totals)
##19340

## AIS, HIV -  and tPA

hiv_ais_tpa_totals <- cbind(dim(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2006,])[1], dim(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2007,])[1], dim(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2008,])[1], dim(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2009,])[1], dim(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2010,])[1])

print(hiv_ais_tpa_totals)

##     [,1] [,2] [,3] [,4] [,5]
##[1,]    6    6   10   16   25

sum(hiv_ais_tpa_totals)
## [1] 63

###############################################################################################
## The Outcome in our regression is death; dicharge to hospice is considered death, DISPUB92 
## code of 50 and 51
###############################################################################################

Outcome <- as.numeric(as.character(data_over_16$DIED))

sum(as.numeric(as.character(data_over_16$DISPUB92))== 50)
## 7170
sum(as.numeric(as.character(data_over_16$DISPUB92))== 51)
## 14305

Outcome[as.numeric(as.character(data_over_16$DISPUB92))== 50] <- 1
Outcome[as.numeric(as.character(data_over_16$DISPUB92))== 51] <- 1

###############################################################################################
## First, regression without adjusting for RACE 
###############################################################################################
regression_data <- data.frame(Outcome, RACE = as.numeric(as.character(data_over_16$RACE)), 
  FEMALE = as.numeric(as.character(data_over_16$FEMALE)), AGE = as.numeric(as.character(data_over_16$AGE)), 
  hiv = as.numeric(as.character(data_over_16$hiv)), tpa = as.numeric(as.character(data_over_16$tpa)))

##Missing data
sum(regression_data$FEMALE < 0)
## 239
sum(regression_data$Outcome < 0)
## 407
sum(regression_data$RACE < 0)
## 184148

###############################################################################################
## Missing gender is recorded as a -6, -8 or -9, Missing death is recorded as 
###############################################################################################

regression_data <- regression_data[regression_data$FEMALE >=0,]
regression_data <- regression_data[regression_data$Outcome >=0,]

fit <- glm(Outcome ~ AGE + hiv + tpa + hiv*tpa + FEMALE, data=regression_data, family=binomial())

##summary(fit)

##Call:
##  glm(formula = Outcome ~ AGE + hiv + tpa + hiv * tpa + FEMALE, 
##      family = binomial(), data = regression_data)

##Deviance Residuals: 
##  Min       1Q   Median       3Q      Max  
##-0.8768  -0.4346  -0.3753  -0.3079   4.0865  

##Coefficients:
##  Estimate Std. Error  z value Pr(>|z|)    
##(Intercept) -5.1278597  0.0262936 -195.023   <2e-16 ***
##  AGE          0.0339163  0.0003433   98.784   <2e-16 ***
##  hiv          1.1037888  0.0638800   17.279   <2e-16 ***
##  tpa          0.8760296  0.0209377   41.840   <2e-16 ***
##  FEMALE       0.1362565  0.0080441   16.939   <2e-16 ***
##  hiv:tpa     -0.3235492  0.3538926   -0.914    0.361    
##---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

##(Dispersion parameter for binomial family taken to be 1)

##Null deviance: 501876  on 940440  degrees of freedom
##Residual deviance: 488609  on 940435  degrees of freedom
##AIC: 488621

##Number of Fisher Scoring iterations: 5

###############################################################################################
## Remove subjects missing race, and adjust for race
###############################################################################################

regression_data <- regression_data[regression_data$RACE >=0,]

fit_1 <- glm(Outcome ~ AGE + hiv + tpa + hiv*tpa + FEMALE + as.factor(RACE), 
  data=regression_data, family=binomial())

summary(fit_1)

##Call:
##  glm(formula = Outcome ~ AGE + hiv + tpa + hiv * tpa + FEMALE + 
##        as.factor(RACE), family = binomial(), data = regression_data)

##Deviance Residuals: 
##  Min       1Q   Median       3Q      Max  
##-0.9043  -0.4343  -0.3747  -0.3070   4.1061  

##Coefficients:
##  Estimate Std. Error  z value Pr(>|z|)    
##(Intercept)      -5.1811875  0.0302949 -171.025  < 2e-16 ***
##  AGE               0.0341381  0.0003897   87.610  < 2e-16 ***
##  hiv               1.0349441  0.0685208   15.104  < 2e-16 ***
##  tpa               0.8711786  0.0231715   37.597  < 2e-16 ***
##  FEMALE            0.1309174  0.0089978   14.550  < 2e-16 ***
##  as.factor(RACE)2  0.1506781  0.0141683   10.635  < 2e-16 ***
##  as.factor(RACE)3  0.1356576  0.0184975    7.334 2.24e-13 ***
##  as.factor(RACE)4  0.0685120  0.0304355    2.251   0.0244 *  
##  as.factor(RACE)5  0.0346540  0.0591698    0.586   0.5581    
##as.factor(RACE)6  0.1886743  0.0281702    6.698 2.12e-11 ***
##  hiv:tpa          -0.4604526  0.3910606   -1.177   0.2390    
##---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

##(Dispersion parameter for binomial family taken to be 1)

##Null deviance: 402130  on 756581  degrees of freedom
##Residual deviance: 391673  on 756571  degrees of freedom
##AIC: 391695

##Number of Fisher Scoring iterations: 5

