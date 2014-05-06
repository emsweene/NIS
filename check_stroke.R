op.index <- apply(as.matrix(data_over_16[,3:17]), 1,  function(x) as.numeric(x) == "430") 
op.index[is.na(op.index)] <- FALSE
op.index <- colSums(op.index)
op.index[op.index >= 1] <- 1
op.index.01003 <- op.index


x <- c(4330, 43300, 43301, 4331, 43310,  43311, 4332, 43320, 43321, 4333, 43330, 43331, 4338, 43380, 43381, 4339 , 43390, 43391, 4340 , 43400, 43401, 4341 , 43410, 43411, 4349 , 43490, 43491, 436)

data.3 <- data[,3] %in% x
data.4 <- data[,4] %in% x
data.5 <- data[,5] %in% x
data.6 <- data[,6] %in% x
data.7 <- data[,7] %in% x
data.8 <- data[,8] %in% x
data.9 <- data[,9] %in% x
data.10 <- data[,10] %in% x
data.11 <- data[,11] %in% x
data.12 <- data[,12] %in% x
data.13 <- data[,13] %in% x
data.14 <- data[,14] %in% x
data.15 <- data[,15] %in% x
data.16 <- data[,16] %in% x
data.17 <- data[,17] %in% x

y <- c(data.3 + data.4 + data.5 + data.6 + data.7 + data.8 + data.9 + data.10 + data.11 + data.12 + data.13 + data.14 + data.15 + data.16 + data.17)

z <- c(1:length(y))
z <- z[y ==0] 
nos <- data[z,3:17]

nos[,1] == '436'