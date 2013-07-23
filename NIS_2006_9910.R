
library(MiscPsycho)

tPA_codes <- ("9910")

n <- 8158381
PR_difference_2008_9910 <- matrix(NA, n, 15)
index <- seq(1, n, by =1) 

############################################################################################################################
##read in part of the CORE file##                                        
############################################################################################################################

data <- read.table("/project/taki2/NIS/NIS_2008/NIS_2008_Core.ASC",  sep = "\t", header = FALSE)
print("data has been read in")


for(i in 1:15) {
PR_difference_2008_9910[,i] <- apply(data, 1, function(x) stringMatch(tPA_codes , substr(x, start = 323 + (i-1)*4, stop = 326 + (i-1)*4), normalize = 'n', case.sensitive = T))
}

print("tPA patients found")

vector <- apply(PR_difference_2008_9910, 1, function(x) min(x))
rm(PR_difference_2008_9910)
index_it_99_2008 <- index[vector == 0]

save(index_it_99_2008,  file= '9910_2008_output.RData')
