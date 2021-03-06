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

data_over_16 <- data[as.numeric(as.character(data$AGE)) > 16,]
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

##############################################################################################
## Abstract
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
## Demographic information for table 2
###############################################################################################

Hospice <- as.numeric(as.character(data_over_16$DIED))
Hospice[as.numeric(as.character(data_over_16$DISPUB92))== 50] <- 1
Hospice[as.numeric(as.character(data_over_16$DISPUB92))== 51] <- 1

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

table((as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]$DIED))))
##  0  1 
## 55  7

table((as.numeric(as.character(Hospice[data_over_16$hiv == 1 & data_over_16$tpa == 1]))))
##  0  1 
## 52 10

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

table((as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]$DIED))))
##    -9    -8     0     1 
##    8     5 17221  2101 

table((as.numeric(as.character(Hospice[data_over_16$hiv == 0 & data_over_16$tpa == 1]))))
##   -9    -8     0     1 
##    8     5 16458  2864 

###############################################################################################
##HIV +  and no TPA
###############################################################################################

median(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 0,]$AGE)))
##51

range(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 0,]$AGE)))
##(17, 86)

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 0,]$FEMALE)))
##   -6    0    1 
##    1 1933  881 

881/(881 + 1933) 
## 0.3130775

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 0,]$RACE)))

##    -9    1    2    3    4    5    6 
##    352  733 1360  273   16   10   71 


table((as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 0,]$DIED))))
##    0    1 
##  2602  213 

table((as.numeric(as.character(Hospice[data_over_16$hiv == 1 & data_over_16$tpa == 0]))))

##   0    1
##2534  281


###############################################################################################
##HIV -  and no TPA
###############################################################################################

median(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 0,]$AGE)))
## 74

range(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 0,]$AGE)))
## 17, 116

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 0,]$FEMALE)))
##    -9     -8     -6      0      1
##   152      1     23 446353 472032 

446353/(446353 + 472032) 
## 0.3130775

table(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 0,]$RACE)))

##      -9     -8      1      2      3      4      5      6 
##   180124     87 566436  88624  45945  15660   4519  17166 


table((as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 0,]$DIED))))
##   -9     -8      0      1
##   197    139 871237  46988 
   

table((as.numeric(as.character(Hospice[data_over_16$hiv == 0 & data_over_16$tpa == 0]))))
##    -9     -8      0      1
##   197    139 850467  67758 

 



 
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

## adjusted OR for hiv, tpa and hiv:tpa
round(exp(1.1328536 + 0.8780151 + -0.3178793), 2)
round(exp(1.1328536 + 0.8780151 + -0.3178793 + 1.96*(sqrt(0.0639216^2+0.0209562^2 +  0.3544404^2))),2)
round(exp(1.1328536 + 0.8780151 + -0.3178793 - 1.96*(sqrt(0.0639216^2+0.0209562^2 +  0.3544404^2))),2)

## adjusted OR for hiv + and tpa (compared to the group that is hiv - and tpa)
round(exp(1.1328536 + -0.3178793), 2)
round(exp(1.1328536 + -0.3178793 + 1.96*(sqrt(0.0639216^2 +  0.3544404^2))),2)
round(exp(1.1328536 + -0.3178793 - 1.96*(sqrt(0.0639216^2 +  0.3544404^2))),2)

## adjusted OR for hiv +  (compared to the group that is hiv - and tpa)
round(exp(1.1328536), 2)
round(exp(1.1328536 + 1.96*(sqrt(  0.3544404^2))),2)
round(exp(1.1328536  - 1.96*(sqrt( 0.3544404^2))),2)

## adjusted OR for hiv- and tpa 
round(exp( 0.8780151 ), 2)
round(exp( 0.8780151  + 1.96*(sqrt(0.0209562^2 ))),2)
round(exp(0.8780151 - 1.96*(sqrt(0.0209562^2 ))),2)


###############################################################################################
## Unadjusted odds ratios
###############################################################################################

unadj_fit_1 <- glm(Outcome ~ hiv + tpa + hiv*tpa, data=regression_data, family=binomial())
summary(unadj_fit_1)

##Call:
##glm(formula = Outcome ~ hiv + tpa + hiv * tpa, family = binomial(),
##    data = regression_data)

##Deviance Residuals:
##    Min       1Q   Median       3Q      Max
##-0.5931  -0.3916  -0.3916  -0.3916   2.2832

##Coefficients:
##             Estimate Std. Error  z value Pr(>|z|)
##(Intercept) -2.529764   0.003992 -633.706  < 2e-16 ***
##hiv          0.330959   0.063004    5.253  1.5e-07 ***
##tpa          0.781293   0.020637   37.860  < 2e-16 ***
##hiv:tpa     -0.231146   0.351582   -0.657    0.511
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##(Dispersion parameter for binomial family taken to be 1)

##    Null deviance: 502874  on 940244  degrees of freedom
##Residual deviance: 501641  on 940241  degrees of freedom
##AIC: 501649

## unadjusted OR for hiv, tpa and hiv:tpa
round(exp(0.330959 +  0.781293 + -0.231146), 2)
round(exp(0.330959 +  0.781293 + -0.231146 + 1.96*(sqrt(0.063004^2+0.020637^2 +  0.351582^2))),2)
round(exp(0.330959 +  0.781293 + -0.231146 - 1.96*(sqrt(0.063004^2+0.020637^2 +  0.351582^2))),2)

## unadjusted OR for hiv + and tpa 
round(exp(0.330959 + -0.231146), 2)
round(exp(0.330959 + -0.231146 + 1.96*(sqrt(0.063004^2 +  0.351582^2))),2)
round(exp(0.330959 + -0.231146 - 1.96*(sqrt(0.063004^2 +  0.351582^2))),2)

unadj_fit_2 <- glm(Outcome ~ hiv , data=regression_data, family=binomial())
summary(unadj_fit_2)

## Call:
## glm(formula = Outcome ~ hiv, family = binomial(), data = regression_data)

## Deviance Residuals:
##    Min       1Q   Median       3Q      Max
## -0.4619  -0.3958  -0.3958  -0.3958   2.2741

## Coefficients:
##             Estimate Std. Error  z value Pr(>|z|)
## (Intercept) -2.507528   0.003913 -640.744  < 2e-16 ***
## hiv          0.323370   0.061956    5.219  1.8e-07 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## (Dispersion parameter for binomial family taken to be 1)

##    Null deviance: 502874  on 940244  degrees of freedom
## Residual deviance: 502849  on 940243  degrees of freedom
## AIC: 502853


## unadjusted OR for hiv +  (compared to the group that is hiv - )
round(exp(0.323370), 2)
round(exp(0.323370  + 1.96*(sqrt(0.061956^2))),2)
round(exp(0.323370  - 1.96*(sqrt(0.061956^2))),2)


unadj_fit_3 <- glm(Outcome ~ tpa , data=regression_data, family=binomial())
summary(unadj_fit_3)

## Call:
## glm(formula = Outcome ~ tpa, family = binomial(), data = regression_data)

## Deviance Residuals:
##    Min       1Q   Median       3Q      Max
## -0.5666  -0.3918  -0.3918  -0.3918   2.2827

## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -2.528599   0.003984 -634.70   <2e-16 ***
## tpa          0.780459   0.020601   37.88   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 502874  on 940244  degrees of freedom
## Residual deviance: 501666  on 940243  degrees of freedom
## AIC: 501670

## Number of Fisher Scoring iterations: 5


## unadjusted OR for tpa (compared to the group that is hiv - )
round(exp(0.780459), 2)
round(exp(0.780459  + 1.96*(sqrt(0.020601^2))),2)
round(exp(0.780459  - 1.96*(sqrt(0.020601^2))),2)


###############################################################################################
## Comparing Mean Length of Stay
###############################################################################################
mean(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]$LOS)))
mean(as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]$LOS)))

t.test(as.numeric(as.character(data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]$LOS)),
as.numeric(as.character(data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]$LOS)))

###############################################################################################
## LOS Sandbox
###############################################################################################

##Patients who have HIV and TPA 

hiv_tpa <- data_over_16[data_over_16$hiv == 1 & data_over_16$tpa == 1,]
death <- as.numeric(as.character(hiv_tpa$DIED))
death[as.numeric(as.character(hiv_tpa$DISPUB92))== 50] <- 1
death[as.numeric(as.character(hiv_tpa$DISPUB92))== 51] <- 1

discharged_day <- ((death - 1 )* -1)

discharged_day <- discharged_day * as.numeric(as.character(hiv_tpa$LOS))
discharged_day[discharged_day == 0] <- 999
death_day <- death * as.numeric(as.character(hiv_tpa$LOS))
death_day[death_day == 0] <- 999

hospital <- rep(dim(hiv_tpa)[1], 90)
died <- rep(0, 90)
discharged <- rep (0, 90)

for(i in 1:90){
died[i] <- sum(death_day < i) 
discharged[i] <- sum(discharged_day < i) 
}

hospital <- c(hospital - (died + discharged))

n <- 62
pdf("HIV_Pos_LOS.pdf")
plot( 1:90, hospital, type = 'l', ylim = c(0,1), xlab = "Days", ylab = "Proportion of Patients in State (n = 62)", main = "HIV Positive Diagnosis with IV-TPA", xlim = c(0, 90))
points( 1:90, c(died + hospital) , type = 'l')
polygon(c(0, 0, 100, 100), c(0, 1, 1, 0),  col = "gray85")
polygon(c(0, 0, 1:90, 300), c(0, 1, c(died + hospital)/n , 0), col = "dark gray")
polygon(c(0, 0, 1:90, 300), c(0, 1, hospital/n, 0), col = "black")
legend("topright", inset=.05, title="State", c("Discharged","Died / Hospice","In Hospital"), fill=(c("gray85", "dark gray", "black")), horiz=FALSE, bg = "white")
dev.off()

hospital_1 <- hospital
died_1 <- died
discharged_1 <- discharged 



##Patients who have No HIV and TPA 

hiv_tpa <- data_over_16[data_over_16$hiv == 0 & data_over_16$tpa == 1,]
hiv_tpa$death <- as.numeric(as.character(hiv_tpa$DIED))
hiv_tpa$death[as.numeric(as.character(hiv_tpa$DISPUB92))== 50] <- 1
hiv_tpa$death[as.numeric(as.character(hiv_tpa$DISPUB92))== 51] <- 1

hiv_tpa<- hiv_tpa[hiv_tpa$death >= 0]

death <- hiv_tpa$death

discharged_day <- ((death - 1 )* -1)

discharged_day <- discharged_day * as.numeric(as.character(hiv_tpa$LOS))
discharged_day[discharged_day == 0] <- 999
death_day <- death * as.numeric(as.character(hiv_tpa$LOS))
death_day[death_day == 0] <- 999

hospital <- rep(dim(hiv_tpa)[1], 90)
died <- rep(0, 90)
discharged <- rep (0, 90)

for(i in 1:90){
died[i] <- sum(death_day < i) 
discharged[i] <- sum(discharged_day < i) 
}

hospital <- c(hospital - (died + discharged))

n <- 19322
pdf("HIV_Neg_LOS.pdf")
plot( 1:90, hospital, type = 'l', ylim = c(0,1), xlab = "Days", ylab = "Proportion of Patients in State (n = 19322)", main = "No HIV Positive Diagnosis with IV-TPA", xlim = c(0, 90))
points( 1:90, c(died + hospital) , type = 'l')
polygon(c(0, 0, 100, 100), c(0, 1, 1, 0),  col = "gray85")
polygon(c(0, 0, 1:90, 300), c(0, 1, c(died + hospital)/n , 0), col = "dark gray")
polygon(c(0, 0, 1:90, 300), c(0, 1, hospital/n, 0), col = "black")
legend("topright", inset=.05, title="State", c("Discharged","Died / Hospice","In Hospital"), fill=(c("gray85", "dark gray", "black")), horiz=FALSE, bg = "white")
dev.off()


hospital_2 <- hospital
died_2 <- died
discharged_2 <- discharged 


###############################################################################################
## Test for differences at days 
###############################################################################################



n <- 10
M <- as.table(rbind(c(hospital_1[n], discharged_1[n] , died_1[n]), c(hospital_2[n], discharged_2[n] , died_2[n])))
M
##
##          Hospital Discharged  Dead
##  HIV +        23         34     5
##  HIV -      4505      12619  2211

dimnames(M) <- list(gender = c("HIV + ","HIV -"),
                    party = c("Hospital","Discharged", "Dead"))
(Xsq <- chisq.test(M, correct = TRUE)) 


n <- 30
M <- as.table(rbind(c(hospital_1[n], discharged_1[n] , died_1[n]), c(hospital_2[n], discharged_2[n] , died_2[n])))
M

##         Hospital Discharged  Dead
##  HIV +         7         47     8
##  HIV -       597      15966  2772

dimnames(M) <- list(gender = c("HIV + ","HIV -"),
                    party = c("Hospital","Discharged", "Dead"))
(Xsq <- chisq.test(M, correct = TRUE)) 



###############################################################################################
###############################################################################################
## Revisions for The European Journal of Neurology April 20, 2014 
###############################################################################################
###############################################################################################


###############################################################################################
## ICD-9 Codes for selected opporuntistic infections 
###############################################################################################

op.inf<- c(10:18, 5472, 70, 707, 942, 1175, 1173, 320, 321, 3212, 3218, 322, 323, 324, 421)

###############################################################################################
## ICD-9 Codes for post-thrombolysis intracranial hemorrhage  
###############################################################################################

post.hem <- c(430, 431, 432)

###############################################################################################
## Trend across year for IV-TPA use in the HIV and non-HIV groups 
###############################################################################################

hiv.tpa.percentage <- c(hiv_ais_tpa_totals/hiv_ais_totals) * 100 
no.hiv.tpa.percentage <- c(no_hiv_ais_tpa_totals/c(ais_totals - hiv_ais_totals)) * 100
year <- c(2006, 2007, 2008, 2009, 2010)

fit.no.hiv <- lm(no.hiv.tpa.percentage ~ year)
fit.hiv <- lm(hiv.tpa.percentage ~ year)

summary(fit.no.hiv)

## Call:
## lm(formula = no.hiv.tpa.percentage ~ year)

## Residuals:
##       1        2        3        4        5 
## 0.13092 -0.04668 -0.15942 -0.06479  0.13997 

## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
##(Intercept) -731.80005   95.96513  -7.626  0.00468 **
##year           0.36547    0.04779   7.647  0.00464 **
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

##Residual standard error: 0.1511 on 3 degrees of freedom
##Multiple R-squared: 0.9512,	Adjusted R-squared: 0.9349 
##F-statistic: 58.48 on 1 and 3 DF,  p-value: 0.004644 


summary(fit.hiv)

## Call:
## lm(formula = hiv.tpa.percentage ~ year)

## Residuals:
##      1       2       3       4       5 
## 0.4983 -0.1997 -0.5688 -0.2564  0.5267 

## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -1550.1821   358.2523  -4.327   0.0228 *
## year            0.7731     0.1784   4.333   0.0227 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 0.5642 on 3 degrees of freedom
## Multiple R-squared: 0.8622,	Adjusted R-squared: 0.8163 
## F-statistic: 18.77 on 1 and 3 DF,  p-value: 0.02267 

###############################################################################################
## ICD-9 Codes for post-thrombolysis intracranial hemorrhage  and oportunistic infetions
###############################################################################################



post.hem.status.430.index <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '430  '))
post.hem.status.430.index[post.hem.status.430.index >= 1] <- 1
ind.430 <- post.hem.status.430.index
ind.430.data <- data_over_16[ind.430 == 1,]

table(ind.430.data$tpa, ind.430.data$hiv)
   
##       0    1
##  0 3165   14
##  1  278    1
  
chisq.test(table(ind.430.data$tpa, ind.430.data$hiv))

post.hem.status.431.index <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '431  '))
post.hem.status.431.index[post.hem.status.431.index >= 1] <- 1
ind.431 <- post.hem.status.431.index
ind.431.data <- data_over_16[ind.431 == 1,]


table(ind.431.data$tpa, ind.431.data$hiv)
   
##       0    1
##  0 6697   16
##  1  976    1
chisq.test(table(ind.431.data$tpa, ind.431.data$hiv))

post.hem.status.432.index <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '432  '))
post.hem.status.4322.index <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '4321 '))
post.hem.status.4329.index <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '4329 '))

ind.432 <- c(post.hem.status.432.index + post.hem.status.4322.index  + post.hem.status.4329.index )
ind.432 [ind.432  >= 1] <- 1
ind.432.data <- data_over_16[ind.432 == 1,]


table(ind.432.data$tpa, ind.432.data$hiv)

##       0    1
##  0 2856    7
##  1  140    0

chisq.test(table(ind.432.data$tpa, ind.432.data$hiv))


ind <- ind.432 + ind.431 + ind.430
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)
   
##        0     1
##  0 12271    37
##  1  1351     2
  


table(data_over_16$hiv[data_over_16$tpa == 1], ind[data_over_16$tpa == 1])
   
##                 0     1
##  0 17984  1351
##  1    60     2

chisq.test(table(data_over_16$hiv[data_over_16$tpa == 1], ind[data_over_16$tpa == 1]))

##		Pearson's Chi-squared test with Yates' continuity correction

##data:  table(data_over_16$hiv[data_over_16$tpa == 1], ind[data_over_16$tpa ==      ##1]) 
##X-squared = 0.8303, df = 1, p-value = 0.3622

##Warning message:
##In chisq.test(table(data_over_16$hiv[data_over_16$tpa == 1], ind[data_over_16$tpa ##==  :
##  Chi-squared approximation may be incorrect


## 4210 : AC/SUBAC BACT ENDOCARD ## 

ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '4210 '))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

ind.4210 <- ind 

##        0    1
##  0 3826   51
##  1   57    3


##TB## 
tub <- c('01300', '01301', '01302',  '01303' ,  '01304' ,  '01305',  '01306',  '01310',   '01311',  '01312',  '01313',
 '01314',  '01315',  '01316',  '01320',  '01321',  '01322',  '01323',  '01324',  '01325',  '01326',  '01330',  '01331',  '01332',  '01333',  '01334',  '01335',  '01336' )

ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% tub))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

##     0  1
##  0 53 13

ind.tub <- ind


##MENEGITIS## 

meng <- c('3200 ', '3201 ', '3202 ', '3203 ', '3207 ', '3208 ', '32081','32082', '32089', '3209 ')
 
ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% meng))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

ind.meng <- ind

##      0   1
##  0 671  19
##  1   7   0



meng2 <- c('3210 ', '3211 ', '3212 ')
 
ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% meng2))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

ind.meng2 <- ind


##      0  1
##  0 88 75
##  1  1  0




 ## HERPES ZOSTER MENINGITIS##
ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '0530 '))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

##     0 1
##  0 6 1

ind.0530<- ind



##ASPERGILLOSIS##
ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '1173 '))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

##       0   1
##  0 201   7
##  1   2   0
ind.1173<- ind



##ENCEPHALITIS NOS##
ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '3239 '))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

ind.3239<- ind

##      0   1
##  0 542  35
##  1  10   0
  
##INTRACRANIAL ABSCESS##
ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '3240 '))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)


##      0   1
##  0 321   7
##  1   5   0
  
ind.3240<- ind






##TOXOPLASM MENINGOENCEPH##
ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% '1300 '))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

##     0  1
##  0  2 12
  
ind.1300<- ind

##Syph##
syph <- c('0942 ', '0943 ', '09481')

ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% syph))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

ind.syph <- ind

     0  1
  0 13  7
  
##Hep carrier## 

ind <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) sum(as.character(factor(x))  %in% 'V0262'))
ind[ind >= 1] <- 1
ind.data <- data_over_16[ind == 1,]
table(ind.data$tpa, ind.data$hiv)

ind.V0262 <- ind

##       0   1
##  0 174  21
##  1   4   3


##TOTAL 
total.ind <- c(ind.4210  +  ind.tub + ind.meng  + ind.meng2  +  ind.0530 + ind.1173 + ind.3239 + ind.3240 + ind.1300 + ind.syph  + ind.V0262)
total.ind[total.ind >= 1] <- 1
ind.data <- data_over_16[total.ind == 1,]

table(ind.data$tpa, ind.data$hiv)

##       0    1
##  0 5644  237
##  1   83    6
