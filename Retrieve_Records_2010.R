#########################################################################################################################
## Load in the patient data
#########################################################################################################################

setwd('/project/taki2/NIS/NIS_2010')

data <- read.table("/project/taki2/NIS/NIS_2010/NIS_2010_Core.ASC",  sep = "\t", header = FALSE)

#########################################################################################################################
## Load in indices for the patients of interest (Acute Ischemic Stroke (AIS), HIV +, and tPA treatement)
#########################################################################################################################

load('5_2010_output.RData')
index_it_5 <- index_it_5_2010
load('433_2010_output.RData')
index_433 <- index_it_109_2010 
load('434_2010_output.RData')
index_434 <- index_it_109_2010 
load('436_2010_output.RData')
index_436 <- index_it_109_2010 
load('9910_2010_output.RData')
index_it_9910 <-  index_it_99_2010

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

Other_Diagnosis_1 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (1-1)*5, stop = 73 + (1-1)*5))
Other_Diagnosis_2 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (2-1)*5, stop = 73 + (2-1)*5))
Other_Diagnosis_3 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (3-1)*5, stop = 73 + (3-1)*5))
Other_Diagnosis_4 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (4-1)*5, stop = 73 + (4-1)*5))
Other_Diagnosis_5 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (5-1)*5, stop = 73 + (5-1)*5))
Other_Diagnosis_6 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (6-1)*5, stop = 73 + (6-1)*5))
Other_Diagnosis_7 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (7-1)*5, stop = 73 + (7-1)*5))
Other_Diagnosis_8 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (8-1)*5, stop = 73 + (8-1)*5))
Other_Diagnosis_9 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (9-1)*5, stop = 73 + (9-1)*5))
Other_Diagnosis_10 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (10-1)*5, stop = 73 + (10-1)*5))
Other_Diagnosis_11 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (11-1)*5, stop = 73 + (11-1)*5))
Other_Diagnosis_12 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (12-1)*5, stop = 73 + (12-1)*5))
Other_Diagnosis_13 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (13-1)*5, stop = 73 + (13-1)*5))
Other_Diagnosis_14 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (14-1)*5, stop = 73 + (14-1)*5))
Other_Diagnosis_15 <- apply(ais_stroke, 2 , function(x) substr(x, start = 69 + (15-1)*5, stop = 73 + (15-1)*5))

#########################################################################################################################
## Pull other covariates of interest from records of AIS patients
#########################################################################################################################

RACE <- apply(ais_stroke, 2 , function(x) substr(x, start = 555, stop = 556))
DISPUB92  <- apply(ais_stroke, 2 , function(x) substr(x, start = 33, stop = 34))
AGE <- apply(ais_stroke, 2 , function(x) substr(x, start = 1, stop = 3))
DIED <- apply(ais_stroke, 2 , function(x) substr(x, start = 20, stop = 21))
FEMALE <- apply(ais_stroke, 2 , function(x) substr(x, start = 307, stop = 308))
LOS <- apply(ais_stroke, 2 , function(x) substr(x, start = 335, stop = 339))
Year <- rep(2010, length(ais_stroke))

#########################################################################################################################
## Create and save dataframe 
#########################################################################################################################

data_2010 <- data.frame(hiv, tpa, Other_Diagnosis_1, Other_Diagnosis_2, Other_Diagnosis_3, Other_Diagnosis_4,
Other_Diagnosis_5, Other_Diagnosis_6, Other_Diagnosis_7, Other_Diagnosis_8, Other_Diagnosis_9, 
Other_Diagnosis_10, Other_Diagnosis_11, Other_Diagnosis_12, Other_Diagnosis_13, Other_Diagnosis_14, 
Other_Diagnosis_15, RACE, DISPUB92, AGE, DIED, FEMALE, LOS, Year ) 

save(data_2010, file = 'ais_stroke_2010.RData') 