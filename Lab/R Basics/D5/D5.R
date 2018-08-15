###----------------------------------------------------------------------------
### Question 1
###----------------------------------------------------------------------------
setwd('~/Dropbox/Teaching/RCourse2012/Lab/D5/')
## 1.1
library() or installed.packages()

## 1.2
## First we use search() function to find a list of attached packages, and R
## objects. then we check the loaded namespaces. The attached packages are
## those in the search list but also available in the loaded namespaces.

search()[search()%in%paste("package:",loadedNamespaces(),sep="")]

## 1.4
## There are many,  you may try mvtnorm
installed.packages("mvtnorm")

## 1.5
library("mvtnorm")
help(package = "mvtnorm")

## 1.6
example("qmvt")

## 1.7
detach(package:mvtnorm)

## 1.8
remove.packages("mvtnorm")

## 1.9
stats::mad(1:10)

###----------------------------------------------------------------------------
### Question 2
###----------------------------------------------------------------------------

## 2.1
AppleNew <- read.csv("AppleWithDate.csv", sep = ";")
X0 <- as.matrix(AppleNew[, c("Open", "High", "Low")])
X <- matrix(1, dim(X0)[1], 4)
X[, 2:4] <- as.matrix(X0)
y <- matrix(AppleNew[, "Close"])
Date <- as.Date(AppleNew[, "Date"], "%Y-%m-%d")

## 2.2

XT <- t(X)
XTX <- XT%*%X
eig <- eigen(XTX)
XTXInv <- solve(t(X)%*%X)
alphaHat <- XTXInv%*%XT%*%y

lm(y~X0)

yHat <- X%*%alphaHat
eHat <- y-yHat

## 2.3
Idx <- 1:50
par(mfrow=c(2, 1))
plot(Date[Idx], y[Idx], xlab = "Time", ylab = "Close Prices")
lines(Date[Idx], yHat[Idx], type = "l", col = "blue")

plot(Date[Idx], eHat[Idx], type = "l", col = "blue", xlab = "Time", ylab =
     "Residuals") 
###----------------------------------------------------------------------------
### Question 3
###----------------------------------------------------------------------------

## 3.1
myStr <- "Programming-in-R"
nchar(myStr)

## 3.2
tolower(myStr)

## 3.3
substr(myStr, 1, 7)

## 3.4
myStr2 <- gsub("-", " ", myStr)

## 3.5
Name <- "Kate"
Grade <- "C"
Year <- 2012

paste(Name, " got a ", Grade, " in the course ", myStr2,
      " in ",  Year, ".", sep = "")

###----------------------------------------------------------------------------
### Question 4
###----------------------------------------------------------------------------

## 4.1
install.packages("zoo")
library("zoo")

## 4.2
appleTS <- zoo(as.matrix(AppleNew[, 2:5]), Date)

## 4.3
plot(appleTS, lty = c("solid","dashed","dotted","longdash"),
     lwd = c(1, 1.5, 2, 2.5), col = c("red","blue","purple","grey"), main =
     "Apple Stock Prices", xlab = "Time")

## 4.4
legend("bottomleft", lty = c("solid","dashed","dotted","longdash"),
     lwd = c(1, 1.5, 2, 2.5), col = c("red","blue","purple","grey"),
       legend = names(AppleNew[, 2:5])) 
dev.copy2pdf(file = "Apple.pdf")


###----------------------------------------------------------------------------
### Question 5
###----------------------------------------------------------------------------

m <- 4
n <- 6

## 5.1
myVec <- runif(m*n)

## 5.2
X <- matrix(myVec, m, n, byrow=TRUE)

## 5.3
d <- 1:n
D <- diag(d)

## 5.4
outDirect <- X%*%D

## 5.5
D2 <- matrix(d, m, n, byrow = TRUE)

## 5.6
outInDirect <- X*D2

## 5.7
all.equal(outDirect, outInDirect)

## 5.8
m <- 400
n <- 6000
myVec <- runif(m*n)
X <- matrix(myVec, m, n, byrow=TRUE)
d <- 1:n
D <- diag(d)
system.time(outDirect <- X%*%D)
D2 <- matrix(d, m, n, byrow = TRUE)
system.time(outInDirect <- X*D2)
all.equal(outDirect, outInDirect)



