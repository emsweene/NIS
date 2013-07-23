#################################
## wc -l NIS_2006_Core.ASC
## 8074825 NIS_2006_Core.ASC
#################################

library(MiscPsycho)

############################################################################################################################
##codes of interest ##                                        
############################################################################################################################

HIV_codes <- c("5")

n <- 8074825
DXCC_1_15_difference_2006_5 <- matrix(NA, n, 15)
index <- seq(1, n, by =1) 

############################################################################################################################
##read in part of the CORE file##                                        
############################################################################################################################

data <- read.table("/project/taki2/NIS/NIS_2006/NIS_2006_Core.ASC",  sep = "\t", header = FALSE)
print("data has been read in")

############################################################################################################################
##Look for matching codes in DXCC variable for HIV (code = 5)##                                  
############################################################################################################################

for(i in 1:15) {
DXCC_1_15_difference_2006_5[,i] <- apply(data, 1, function(x) stringMatch(HIV_codes, as.numeric(substr(x, start = 141 + (i-1)*4, stop = 144 + (i-1)*4)), normalize = 'n', case.sensitive = T))
}

vector <- apply(DXCC_1_15_difference_2006_5, 1, function(x) min(x))
rm(DXCC_1_15_difference_2006_5)
index_it_5_2006 <- index[vector == 0]

print("HIV patients found")

save(index_it_5_2006, file= '5_2006_output.RData')

