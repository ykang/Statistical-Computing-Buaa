###----------------------------------------------------------------------------
### Question: 1
###----------------------------------------------------------------------------

s1.1a <- seq(from = 1, to = 10) # 1.1(a)
s1.1b <- seq(from = 10, to = 1) # 1.1(b)
s1.1c <- seq(from = 1, to = 15, by = 2) # 1.1(c)
s1.2 <- seq(from = 0, to = 1, length.out = 12) # 1.2
s1.3 <- seq(from = 0, to = 1, by = 0.03) # 1.3
s1.4 <- seq(from = 1, length.out = 7, by = 1.3) # 1.4
s1.5 <- seq(from = 1, length.out = 7, by = -1.3) # 1.5
s1.5 <- seq(to = 20, length.out = 8, by = 2.2) # 1.6
s1.6 <- seq(to = 20, length.out = 8, by = -2.2) # 1.7

## 1.8
## We take Q1.6 as an example
mean(Q1.6)
var(Q1.6)
sd(Q1.6)

rep("hello", 3) # 1.9(a)
rep(TRUE, 5) # 1.9(b)
rep(2, 10) # 1.9(c)

s1.10a <- rep(1:5, 3) # 1.10(a)
s1.10b <- seq(1, 9)^2 # 1.10(b)

rep(seq(5, 10, 1.2), each = 3) # 1.11

###----------------------------------------------------------------------------
### Question 2
###----------------------------------------------------------------------------

## 2.1
s2.1a <- (s1.10a>2)
s2.1b <- (s1.10a>2)
s2.1c <- (s1.10a >= 2)
s2.1d <- (s1.10a <= 3)
s2.1e <- (s1.10a  == 2)
s2.1f <- (s1.10a > 2 & s1.10a <4)
s2.1g <- (s1.10a > 3 | s1.10a < 2)

## 2.2
## We just take 2.1.f as example
which(s2.1f)

## 2.3
## WE just take 2.1.f as an example
s1.10a[s2.1f]

## 2.4
s1.10b[-1] # 2.4(a)
s1.10b[-length(s1.10b)] # 2.4(b)
s1.10b[-((length(s1.10b)-3):length(s1.10b))] # 2.4(c)
s1.10b[seq(2, length(s1.10b), by = 2)] # 2.4(d)
s1.10b[s1.10b%%2 == 1] # 2.4(e)
s1.10b[s1.10b > mean(s1.10b)] # 2.4(f)

###----------------------------------------------------------------------------
### Question 3
###----------------------------------------------------------------------------

## 3.1 You will get NA
a <-  c(seq(1, 10, 2.2), rep(NA, 3), seq(10, 20, 1.5), rep(NaN, 4))
mean(a)
var(a)

## 3.2
mean(a, na.rm = TRUE)
var(a, na.rm = TRUE)

## 3.3
is.nan(a) # 3.3 Just remove NaN
is.na(a) # Note that is.na will check both NA and NaN.

## 3.4
a[!is.nan(a)] # Just remove NaN
a[!is.na(a) | is.nan(a)]

## 3.5
mean(a[!is.na(a)])
var(a[!is.na(a)])

###----------------------------------------------------------------------------
### Question 4
###----------------------------------------------------------------------------

## 4.1
study <- c("Monday", "Tuesday", "Thursday")
study <- append(study, "Wednesday", 2)

## 4.2
job <- "Friday"
fun <- c("Saturday","Sunday")
weekdays <- c(study, job, fun)

## 4.3
hours <- c(rep(4, 4), 6, 0, 0)

## 4.4
tasks <- c(rep("study", 4), "job", rep("fun", 2))
tasks <- as.factor(tasks)

## 4.5
class(tasks)

## 4.6
weekPlan <- list(weekdays, hours, tasks) # Method A: no name attributed

weekPlan <- list(weekdays = weekdays, hours = hours, tasks = taks) # Method B: with names

## 4.7
names(weekPlan) <- c("weekdays", "hours", "tasks")

## 4.8
class(weekPlan)
object.size(weekPlan)
str(weekPlan)

## 4.9
weekPlan$note <- "plan for week 10"
weekPlan[["note"]] <- "plan for week 10"
c(weekPlan, note  =  "plan for week 10")

## 4.10
weekPlan[["weekdays"]]
weekPlan$weekdays

## 4.11
weekPlan[[1]]

## 4.12
weekPlan2 <- weekPlan[c("weekdays", "tasks")]

## 4.13
weekPlan3 <- weekPlan[c(1, 2)]

## 4.14
weekPlan[["note"]] <- NULL

## 4.15
weekPlan$weekdays[weekPlan$hours>4]

###----------------------------------------------------------------------------
### Question 5
###----------------------------------------------------------------------------

## 5.1
weekPlanNew <- data.frame(weekdays, hours, tasks)

## 5.2
str(weekPlanNew)
nrow(weekPlanNew)
ncol(weelPlanNew)
dim(weekPlanNew)
rownames(weekPlanNew)
colnames(weekPlanNew)

## 5.3
costs <- c(70, 75, 58, 62, 140, 90, 70)
weekPlanNew <- cbind(weekPlanNew, costs)

## 5.4
weekPlanNew[1, c("tasks", "hours")] # 5.4.a
weekPlanNew[5:7, "tasks"]  # 5.4.b
sum(weekPlanNew[6:7, "costs"]) # 5.4.c
sum(weekPlanNew[, "costs"]) # 5.4.c
sum(weekPlanNew[, "hours"][weekPlanNew[, "tasks"] == "job"]) # 5.4.d
weekPlanNew[, "weekdays"][weekPlanNew[, "hours"] < 5] # 5.4.e
weekPlanNew[, "weekdays"][weekPlanNew[, "costs"] >100] # 5.4.f
weekPlanNew[2, "tasks"] <- "job" # 5.4.g
dim(weekPlanNew[, 1]) # 5.4.h
dim(weekPlanNew[, c(1, 3)])
dim(weekPlanNew[, 1, drop = FALSE]) # 5.4.i
dim(weekPlanNew[, c(1, 3), drop = FALSE])

## 5.5
weekPlanNew <- rbind(weekPlanNew, weekPlanNew)

## 5.6
weekPlanNew[8:14, "costs"] <- min(weekPlanNew[1:7, "costs"])


###----------------------------------------------------------------------------
### Question 6
###----------------------------------------------------------------------------

mySummary <- function(x)
  {
    xMean <- mean(x)
    xVar <- var(x)
    xLen <- length(x)
    xSize <- object.size(x)
    xMax <- max(x)
    xMin <- min(x)
    xClass <- class(x)


    out <- list(mean = xMean, var = xVar, length = xLen,size = xSize,
                min = xMin, max = xMax, class = xClass)
    return(out)
  }
mySummary(s1.10a)

mySummary(weekPlanNew) # error


###----------------------------------------------------------------------------
### Question 7
###----------------------------------------------------------------------------

quaroot <- function(a, b, c)
  {
    x1 <- (-b+sqrt(b^2-4*a*c))/(2*a)
    x2 <- (-b-sqrt(b^2-4*a*c))/(2*a)

    out <- c(x1, x2)
    return(out)
  }

quaroot(1, 4, -1)

quaroot(5, 2, 1) # warning with NaN
quaroot <- function(a, b, c)
  {
    d <- b^2-4*a*c

    if(d<0)
      {
        x1 <- NA
        x2 <- NA
      }
    else
      {
        x1 <- (-b+sqrt(d))/(2*a)
        x2 <- (-b-sqrt(d))/(2*a)
      }
    
    out <- c(x1, x2)
    return(out)
  }

quaroot(5, 2, 1) # NA without warnings
