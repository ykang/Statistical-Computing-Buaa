###----------------------------------------------------------------------------
### Question 1
###----------------------------------------------------------------------------
runif(30, min = 1, max = 10) #1.1

myRNormVec <- rnorm(200, mean = 1, sd = sqrt(2)) #1.2

hist(myRNormVec) #1.3
boxplot(myRNormVec)

rpois(30, lambda = 3) #1.4

rt(30, df = 5) + 1 #1.5. Note that the ncp parameter is NOT the mean, but for this distributions (and many others) we can just add the mean to the vector. This has the effect of shifting the density one unit to the right, which makes the mean equal to one.

sample(1:10, 5) #1.6

friends <- c("Feng", "Karl", "Mattias", "Bertil") # 1.7

# The simple version:
sample(friends, size = 2, replace = TRUE)

# The fancy version ...
matrix(sample(friends, size = 2, replace = TRUE),
           dimnames=list(c("Job1", "Job2"), "who"))


set.seed(1234) #1.8
myRNormVec1 <- rnorm(200, mean = 1, sd = sqrt(2))
set.seed(1234)
myRNormVec2 <- rnorm(200, mean = 1, sd = sqrt(2))
all.equal(myRNormVec1, myRNormVec2) # they are the same.

###----------------------------------------------------------------------------
### Question 2
###----------------------------------------------------------------------------
x <- 2

dnorm(x = x, mean = 1, sd = sqrt(2)) # 2.1

pnorm(q = x, mean = 1, sd = sqrt(2)) # 2.2

x <- seq(-10, 10, 0.1) # 2.3
plot(x, pnorm(q = x, mean = 1, sd = sqrt(2)), type = "l")

dev.copy2pdf(file="NormCDFCurve.pdf") #2.4

qnorm(0.95, mean = 1, sd = sqrt(2)) # 2.5

###----------------------------------------------------------------------------
### Question 3
###----------------------------------------------------------------------------

Apple <- read.csv("Apple.csv", sep = ";") # 3.1
appleHigh <- Apple[, "High"] # Alternative: appleHigh <- Apple[, 2]. Note that appleHigh <- Apple["High"] or appleHigh <- Apple[2] does not work 					since Apple[2] is a list.

applemu <- mean(appleHigh) # 3.2
applesd <- sd(appleHigh) # same as sum((x-mean(x))^2)/(n-1)

t.test(appleHigh, mu = applemu) # 3.3

###----------------------------------------------------------------------------
### Question 4
###----------------------------------------------------------------------------
Google <- read.csv("Google.csv", sep = ";")

appleClose <- Apple[, "Close"] # 4.1
googleClose <- Google[, "Close"]
time <- length(appleClose):1 

# 4.2
plot(time, appleClose, xlab = "time", ylab = "Prices", main = "Stock prices")

# 4.3
plot(time, appleClose, type = "l", col = "blue",
     xlab = "Time",  ylab = "Prices", main = "Stock prices",
     ylim = c(0, max(appleClose, googleClose)))

# 4.4
points(time, googleClose, type = "l", col = "red")

# 4.5
legend("topleft", lty = c("solid", "solid"), col = c("red", "blue"),
       legend = c("Google", "Apple"))

# 4.6
savePlot(filename = "Apple-Google-Close.png")

# 4.7
par(mfrow = c(1, 2))

#######
# redo4.2
plot(time, appleClose, xlab = "time", ylab = "Prices", main = "Stock prices")

# redo4.3
plot(time, appleClose, type = "l", col = "blue",
     xlab = "Time",  ylab = "Prices", main = "Stock prices",
     ylim = c(0, max(appleClose, googleClose)))

# redo4.4
points(time, googleClose, type = "l", col = "red")

# redo4.5
legend("topleft", lty = c("solid", "solid"), col = c("red", "blue"),
       legend = c("Google", "Apple"))




###----------------------------------------------------------------------------
### Question 5
###----------------------------------------------------------------------------
# 5.1
appleModel <- lm(appleClose ~ appleHigh)

# 5.2
appleModel           
summary(appleModel)
hist(appleModel$residuals)
par(mfrow=c(2,2))
plot(appleModel)

# 5.3
appleOpen <- Apple$Open
appleModel2 <- lm(appleClose ~ appleHigh + appleOpen)

# 5.4
summary(appleModel2)
hist(appleModel2$residuals)
plot(appleModel2)

# 5.5
appleModel2 <- lm(appleClose ~ appleHigh + appleOpen, subset=1:100)

# 5.6
appleLow <- Apple$Low
minModel <- lm(appleClose ~ 1)
fwdModel <- step(minModel, direction="forward",
                 scope=(~ appleHigh+appleOpen+appleLow))
summary(fwdModel)

#5.7
fullModel <- lm(appleClose ~ appleHigh+appleOpen+appleLow)
reducedModel <- step(fullModel, direction="backward")
summary(reducedModel)
