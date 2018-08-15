nRow <- 100000
nCol <- 200

myMat <- matrix(1:(nRow*nCol), nRow, nCol)

a <- Sys.time()
out <- matrix(NA, nRow, 1)
for(i in 1:nRow)
  {
    out[i] = mean(myMat[i, ])
  }
b <- Sys.time()-a


a1 <- Sys.time()
out1 <- apply(myMat, 1, mean)
b1 <- Sys.time()-a1


myAry <- array(1:24, c(2, 3, 4))
