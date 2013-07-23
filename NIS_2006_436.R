###########################
## wc -l NIS_2006_Core.ASC
## 8074825 NIS_2006_Core.ASC
#################################

library(MiscPsycho)

Stroke_code <- c("436")
 
## "434", "435", "436", "437")

############################################################################################################################
##create vectors to store data##                                        
############################################################################################################################
data <- read.table("/project/taki2/NIS/NIS_2008/NIS_2008_Core.ASC",  sep = "\t", header = FALSE)
print("data has been read in")


n <- 8158381
DXCC_1_15_difference_2008_109 <- matrix(NA, n, 15)
index <- seq(1, n, by =1) 

############################################################################################################################
##Look for matching codes in DXCC variable for HIV (code = 5)##                                  
############################################################################################################################

for(i in 1:15) {
DXCC_1_15_difference_2008_109[,i] <- apply(data, 1,  function(x) stringMatch(Stroke_code , substr(x, start = 68 + (i-1)*5, stop = 70 + (i-1)*5), normalize = 'n', case.sensitive = T))
}

print("Stroke patients found")

vector <- apply(DXCC_1_15_difference_2008_109, 1, function(x) min(x))
rm(DXCC_1_15_difference_2008_109) 

index_it_109_2008 <- index[vector == 0]

save(index_it_109_2008,  file= '436_2008_output.RData')
