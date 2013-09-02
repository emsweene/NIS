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

data_over_16 <- data[as.numeric(as.charachter(data$AGE)) > 16,]
dim(data_over_16)
## 940773     24

###############################################################################################
## Information for table 1
###############################################################################################

##AIS Patients (by year)

ais_totals <- cbind(dim(data_over_16[data_over_16$Year == 2006,])[1], 
  dim(data_over_16[data_over_16$Year == 2007,])[1], dim(data_over_16[data_over_16$Year == 2008,])[1], 
  dim(data_over_16[data_over_16$Year == 2009,])[1], dim(data_over_16[data_over_16$Year == 2010,])[1])

print(ais_totals)

##       [,1]   [,2]   [,3]   [,4]   [,5]
##[1,] 185865 181322 196360 187146 190080

sum(ais_totals)
## 940773

round(ais_totals/sum(ais_totals) *100, 1)

##     [,1] [,2] [,3] [,4] [,5]
## [1,] 19.8 19.3 20.9 19.9 20.2


##AIS and HIV + Patients (by year)

hiv_ais_totals <- cbind(sum(data_over_16$hiv[data_over_16$Year == 2006]), 
  sum(data_over_16$hiv[data_over_16$Year == 2007]), sum(data_over_16$hiv[data_over_16$Year == 2008]), 
  sum(data_over_16$hiv[data_over_16$Year == 2009]), sum(data_over_16$hiv[data_over_16$Year == 2010]))

     [,1] [,2] [,3] [,4] [,5]
[1,]  555  519  641  567  595

sum(hiv_ais_totals)
[1] 2877

round(hiv_ais_totals/sum(hiv_ais_totals) *100, 1)

## AIS, HIV -  and tPA

no_hiv_ais_tpa_totals <- cbind(dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2006,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2007,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2008,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2009,])[1], dim(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1 & 
  data_over_16$Year == 2010,])[1])

print(no_hiv_ais_tpa_totals)
##     [,1] [,2] [,3] [,4] [,5]
##[1,] 2700 2974 3714 4399 5548

sum(no_hiv_ais_tpa_totals)
##19335

round(no_hiv_ais_tpa_totals/sum(no_hiv_ais_tpa_totals) *100, 1)

##      [,1] [,2] [,3] [,4] [,5]
## [1,]   14 15.4 19.2 22.8 28.7

## AIS, HIV + and tPA

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
## [1] 62


round(hiv_ais_tpa_totals/sum(hiv_ais_tpa_totals) *100, 1)
##      [,1] [,2] [,3] [,4] [,5]
## [1,]  9.7  9.7 16.1 24.2 40.3


##total HIV - with ais
sum(ais_totals) - sum(hiv_ais_totals)

###############################################################################################
## Test for difference in IV_TPA use in the two groups (AIS HIV+ and AIS HIV-)
###############################################################################################
x <- matrix(c(sum(hiv_ais_tpa_totals), sum(hiv_ais_totals) , sum(no_hiv_ais_tpa_totals), 
sum(ais_totals) - sum(hiv_ais_totals)), 2,2)
chisq.test(x)
##p-value = 0.78

###############################################################################################
###############################################################################################
## Calculate the demographics for the two groups that recieved tPA  (AIS HIV+ TPA and AIS 
## HIV- TPA)
###############################################################################################
###############################################################################################

###############################################################################################
##HIV+ and TPA
###############################################################################################

median(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]$AGE)))
##52

range(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]$AGE)))
## [1] 27 78

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]$FEMALE)))
## 0  1
## 42 20

20/62
## 0.3225806

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]$RACE)))

## -9  1  2  3  5
##  7 12 35  6  2

12/(35 + 12 + 6 + 2)
## 0.2181818

###############################################################################################
##HIV- and TPA
###############################################################################################

median(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]$AGE)))
##72

range(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]$AGE)))
## [1] 17 102

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]$FEMALE)))
##  -9    0    1
##   2 9621 9712 

9712/(9621 + 9712)
##  0.5023535

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]$RACE)))

##    -9    -8     1     2     3     4     5     6 
##  3458     1 11821  2141  1013   406    72   423 

11821/(11821 + 2141 + 1013 + 406 + 72 + 423)
## 0.744583

###############################################################################################
## The regression
###############################################################################################
###############################################################################################
## The Outcome in our regression is death; dicharge to hospice is considered death, DISPUB92 
## code of 50 and 51
###############################################################################################

Outcome <- as.numeric(as.character(data_over_16$DIED))
table(Outcome)

##Outcome
##    -9     -8      0      1
##   205    144 891115  49309

sum(as.numeric(as.character(data_over_16$DISPUB92))== 50)
## 7213
sum(as.numeric(as.character(data_over_16$DISPUB92))== 51)
## 14391

table(Outcome[data_over_16$hiv == 0])

##     -9     -8      0      1 
##    205    144 888458  49089

table(Outcome[data_over_16$hiv == 1])
##   0    1
## 2657  220

x <- matrix(c(220, 2877, 49089, 937547), 2, 2) 
chisq.test(x)

##for the group with TPA##

table(Outcome[data_over_16$hiv == 0 &  data_over_16$tpa == 1])

##        -9    -8     0     1 
##        8     5  17221  2101 

table(Outcome[data_over_16$hiv == 1 &  data_over_16$tpa == 1])
##   0  1 
##  55  7


x <- matrix(c(7, 62, 2101 , 19322), 2, 2) 
chisq.test(x)


##add outcome of hosspice 


Outcome[as.numeric(as.character(data_over_16$DISPUB92))== 50] <- 1
Outcome[as.numeric(as.character(data_over_16$DISPUB92))== 51] <- 1

table(Outcome[data_over_16$hiv == 0])

##    -9     -8      0      1
##   205    144 866925  70622 

table(Outcome[data_over_16$hiv == 1])
##   0    1
## 2586  291 

x <- matrix(c(291, 2586 + 291, 70622, 70622 + 866925),2,2)
chisq.test(x)


##for the group with TPA##

table(Outcome[data_over_16$hiv == 0 &  data_over_16$tpa == 1])

##      -9    -8     0     1 
##    8     5 16458  2864 

table(Outcome[data_over_16$hiv == 1 &  data_over_16$tpa == 1])
##   0  1 
##  52 10 

x <- matrix(c(10, 62, 2864 , 2864 + 16458),2,2)
chisq.test(x)




###############################################################################################
## First, regression without adjusting for RACE 
###############################################################################################
regression_data <- data.frame(Outcome, RACE = as.numeric(as.character(data_over_16$RACE)), 
  FEMALE = as.numeric(as.character(data_over_16$FEMALE)), AGE = as.numeric(as.character(data_over_16$AGE)), 
  hiv = as.numeric(as.character(data_over_16$hiv)), tpa = as.numeric(as.character(data_over_16$tpa)))
dim(regression_data)
## 940773      6

sum(regression_data$hiv)
## [1] 2877

##Missing data
sum(regression_data$FEMALE < 0)
##  179
sum(regression_data$Outcome < 0)
## 349
sum(regression_data$RACE < 0)
## 184029

###############################################################################################
## Missing gender is recorded as a -6, -8 or -9, Missing death is recorded as 
###############################################################################################

regression_data <- regression_data[regression_data$FEMALE >=0,]
regression_data <- regression_data[regression_data$Outcome >=0,]

dim(regression_data)
## [1] 940245      6

fit <- glm(Outcome ~ AGE + hiv + tpa + hiv*tpa + FEMALE, data=regression_data, family=binomial())

summary(fit)

##Call:
##glm(formula = Outcome ~ AGE + hiv + tpa + hiv * tpa + FEMALE,
##    family = binomial(), data = regression_data)

##Deviance Residuals:
##    Min       1Q   Median       3Q      Max
##-0.9271  -0.4364  -0.3748  -0.3043   3.0454

##Coefficients:
##              Estimate Std. Error  z value Pr(>|z|)
##(Intercept) -5.2260511  0.0264411 -197.649   <2e-16 ***
##AGE          0.0352089  0.0003445  102.213   <2e-16 ***
##hiv          1.1328536  0.0639216   17.723   <2e-16 ***
##tpa          0.8780151  0.0209562   41.898   <2e-16 ***
##FEMALE       0.1347427  0.0080436   16.752   <2e-16 ***
##hiv:tpa     -0.3178793  0.3544404   -0.897     0.37
---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

##(Dispersion parameter for binomial family taken to be 1)

##    Null deviance: 502874  on 940244  degrees of freedom
##Residual deviance: 488844  on 940239  degrees of freedom
##AIC: 488856

##Number of Fisher Scoring iterations: 5

###############################################################################################
## Unadjusted odds ratios
###############################################################################################

