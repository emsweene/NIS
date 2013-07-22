#########################################################################################################################
## Load in the patient data
#########################################################################################################################

setwd('/project/taki2/NIS/NIS_2009')

data <- read.table("/project/taki2/NIS/NIS_2009/NIS_2009_Core.ASC",  sep = "\t", header = FALSE)

#########################################################################################################################
## Load in indices for the patients of interest (Acute Ischemic Stroke (AIS), HIV +, and tPA treatement)
#########################################################################################################################

load('5_2009_output.RData')
index_it_5 <- index_it_5_2009
load('433_2009_output.RData')
index_433 <- index_it_109_2009 
load('434_2009_output.RData')
index_434 <- index_it_109_2009
load('436_2009_output.RData')
index_436 <- index_it_109_2009 
load('9910_2009_output.RData')
index_it_9910 <-  index_it_99_2009

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

Other_Diagnosis_1 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (1-1)*5, stop = 72 + (1-1)*5))
Other_Diagnosis_2 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (2-1)*5, stop = 72 + (2-1)*5))
Other_Diagnosis_3 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (3-1)*5, stop = 72 + (3-1)*5))
Other_Diagnosis_4 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (4-1)*5, stop = 72 + (4-1)*5))
Other_Diagnosis_5 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (5-1)*5, stop = 72 + (5-1)*5))
Other_Diagnosis_6 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (6-1)*5, stop = 72 + (6-1)*5))
Other_Diagnosis_7 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (7-1)*5, stop = 72 + (7-1)*5))
Other_Diagnosis_8 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (8-1)*5, stop = 72 + (8-1)*5))
Other_Diagnosis_9 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (9-1)*5, stop = 72 + (9-1)*5))
Other_Diagnosis_10 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (10-1)*5, stop = 72 + (10-1)*5))
Other_Diagnosis_11 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (11-1)*5, stop = 72 + (11-1)*5))
Other_Diagnosis_12 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (12-1)*5, stop = 72 + (12-1)*5))
Other_Diagnosis_13 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (13-1)*5, stop = 72 + (13-1)*5))
Other_Diagnosis_14 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (14-1)*5, stop = 72 + (14-1)*5))
Other_Diagnosis_15 <- apply(ais_stroke, 2 , function(x) substr(x, start = 68 + (15-1)*5, stop = 72 + (15-1)*5))

#########################################################################################################################
## Pull other covariates of interest from records of AIS patients
#########################################################################################################################

RACE <- apply(ais_stroke, 2 , function(x) substr(x, start = 564, stop = 565))
DISPUB92  <- apply(ais_stroke, 2 , function(x) substr(x, start = 32, stop = 33))
AGE <- apply(ais_stroke, 2 , function(x) substr(x, start = 1, stop = 3))
DIED <- apply(ais_stroke, 2 , function(x) substr(x, start = 19, stop = 20))
FEMALE <- apply(ais_stroke, 2 , function(x) substr(x, start = 306, stop = 307))
LOS <- apply(ais_stroke, 2 , function(x) substr(x, start = 334, stop = 338))
Year <- rep(2009, length(ais_stroke))

#########################################################################################################################
## Create and save dataframe 
#########################################################################################################################

data_2009 <- data.frame(hiv, tpa, Other_Diagnosis_1, Other_Diagnosis_2, Other_Diagnosis_3, Other_Diagnosis_4,
Other_Diagnosis_5, Other_Diagnosis_6, Other_Diagnosis_7, Other_Diagnosis_8, Other_Diagnosis_9, 
Other_Diagnosis_10, Other_Diagnosis_11, Other_Diagnosis_12, Other_Diagnosis_13, Other_Diagnosis_14, 
Other_Diagnosis_15, RACE, DISPUB92, AGE, DIED, FEMALE, LOS, Year ) 

save(data_2009, file = 'ais_stroke_2009.RData') 