#########################################################################################################################
## Load in the patient data
#########################################################################################################################

setwd('/project/taki2/NIS/NIS_2006')

data <- read.table("/project/taki2/NIS/NIS_2006/NIS_2006_Core.ASC",  sep = "\t", header = FALSE)

#########################################################################################################################
## Load in indices for the patients of interest (Acute Ischemic Stroke (AIS), HIV +, and tPA treatement)
#########################################################################################################################

load('5_2006_output.RData')
index_it_5 <- index_it_5_2006
load('433_2006_output.RData')
index_433 <- index_it_109_2006 
load('434_2006_output.RData')
index_434 <- index_it_109_2006
load('436_2006_output.RData')
index_436 <- index_it_109_2006 
load('9910_2006_output.RData')
index_it_9910 <-  index_it_99_2006

#########################################################################################################################
## Pull patient records for patients with AIS
#########################################################################################################################

ais_stroke_index<- as.matrix(unique(c(index_433, index_434, index_436)))
ais_stroke <- as.matrix(data[ais_stroke_index,1])

#########################################################################################################################
## Create variable for indicator of HIV in AIS population
#########################################################################################################################

index_it <- duplicated(c(ais_stroke_index, index_it_5), fromLast = TRUE) 
hiv <- rep(0, length(ais_stroke))
hiv[index_it[1:length(ais_stroke)] ]  <- 1

#########################################################################################################################
## Create variable for indicator of tPA in AIS population
#########################################################################################################################

index_it <- duplicated(c(ais_stroke_index, index_it_9910), fromLast = TRUE) 
tpa <- rep(0, length(ais_stroke))
tpa[index_it[1:length(ais_stroke)] ]  <- 1

#########################################################################################################################
## Pull Patient Comorbidies from records from AIS patients
#########################################################################################################################

Other_Diagnosis_1 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (1-1)*5, stop = 70 + (1-1)*5))
Other_Diagnosis_2 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (2-1)*5, stop = 70 + (2-1)*5))
Other_Diagnosis_3 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (3-1)*5, stop = 70 + (3-1)*5))
Other_Diagnosis_4 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (4-1)*5, stop = 70 + (4-1)*5))
Other_Diagnosis_5 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (5-1)*5, stop = 70 + (5-1)*5))
Other_Diagnosis_6 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (6-1)*5, stop = 70 + (6-1)*5))
Other_Diagnosis_7 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (7-1)*5, stop = 70 + (7-1)*5))
Other_Diagnosis_8 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (8-1)*5, stop = 70 + (8-1)*5))
Other_Diagnosis_9 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (9-1)*5, stop = 70 + (9-1)*5))
Other_Diagnosis_10 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (10-1)*5, stop = 70 + (10-1)*5))
Other_Diagnosis_11 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (11-1)*5, stop = 70 + (11-1)*5))
Other_Diagnosis_12 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (12-1)*5, stop = 70 + (12-1)*5))
Other_Diagnosis_13 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (13-1)*5, stop = 70 + (13-1)*5))
Other_Diagnosis_14 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (14-1)*5, stop = 70 + (14-1)*5))
Other_Diagnosis_15 <- apply(ais_stroke, 2 , function(x) substr(x, start = 66 + (15-1)*5, stop = 70 + (15-1)*5))

#########################################################################################################################
## Pull other covariates of interest from records of AIS patients
#########################################################################################################################

RACE <- apply(ais_stroke, 2 , function(x) substr(x, start = 478, stop = 479))
DISPUB92  <- apply(ais_stroke, 2 , function(x) substr(x, start = 37, stop = 38))
AGE <- apply(ais_stroke, 2 , function(x) substr(x, start = 1, stop = 3))
DIED <- apply(ais_stroke, 2 , function(x) substr(x, start = 24, stop = 25))
FEMALE <- apply(ais_stroke, 2 , function(x) substr(x, start = 239, stop = 240))
LOS <- apply(ais_stroke, 2 ,  function(x) substr(x, start = 264, stop = 268))
Year <- rep(2006, length(ais_stroke))

#########################################################################################################################
## Create and save dataframe 
#########################################################################################################################

data_2006 <- data.frame(hiv, tpa, Other_Diagnosis_1, Other_Diagnosis_2, Other_Diagnosis_3, Other_Diagnosis_4,
Other_Diagnosis_5, Other_Diagnosis_6, Other_Diagnosis_7, Other_Diagnosis_8, Other_Diagnosis_9, 
Other_Diagnosis_10, Other_Diagnosis_11, Other_Diagnosis_12, Other_Diagnosis_13, Other_Diagnosis_14, 
Other_Diagnosis_15, RACE, DISPUB92, AGE, DIED, FEMALE, LOS, Year ) 

save(data_2006, file = 'ais_stroke_2007.RData') 