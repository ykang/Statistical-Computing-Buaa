###----------------------------------------------------------------------------
### Question 1
###----------------------------------------------------------------------------

# 1.1
isLeapday <- function(year)
  {
   mod4 <- year%%4
   mod100 <- year%%100
   mod400 <- year%%400
   
   if((mod4  == 0 & mod100 != 0) |  mod400  == 0)
     {
       out <- TRUE
     }
   else
     {
       out <- FALSE
     }
   return(out)
  }

# 1.2
isLeapday(1998)
isLeapday(2000)
isLeapday(2002)
isLeapday(2100)



whichLeapday <- function(year)
  {
    if(!is.numeric(year))
      {
        stop("You must specify a numerical input.")
      }
    
   mod4 <- year%%4
   mod100 <- year%%100
   mod400 <- year%%400
   
   leapdayIndex <- ((mod4  == 0 & mod100 != 0) | mod400  == 0)

    out <- year[leapdayIndex]
    
    return(out)
  }

whichLeapday(c(1998:2100))

###----------------------------------------------------------------------------
### Question 2
###----------------------------------------------------------------------------

## For version
myfactorial <- function(n)
  {
    out <- 1
    for (i in 1:n)
      {
        out <- out*i        
      }
    
    return(out)
  }


mylogfactorial = function(n)
{
   out = 0
   for (i in 1:n)
   {
        out = out + log(i)
   }
   return(out)
}

## While version
myfactorial2 <- function(n)
  {
    out <- 1
    i <- 1
    while (i < n)
      {
        i <- i +1
        out <- out*i        
      }
    
    return(out)
  }

## Alternative while version

myfactorial2Alt <- function(n)
  {
    out <- 1
    i <- 1
    while (i <= n)
      {
        out <- out*i
        i <- i +1        
      }
    
    return(out)
  }

## The no-loop version
myfactorial3 <- function(n)
  {
    out <- prod(1:n)
    return(out)
  }

## Replication and timing 
system.time(replicate(n = 5000, myfactorial(150)))

## Overflow
myfactorial(500) # Inf

## Factorial loop with log option
myfactorial <- function(n, log)
  {
    log.out <- log(1)
    for (i in 1:n)
      {
        log.out <- log.out +  log(i)        
      }

    if(log == TRUE)
      {
        return(log.out)
      }
    else
      {
        out <- exp(log.out)
        return(out)
      }
}

## Factorial (no loops) with log option
myfactorialNew <- function(n, log)
  {
    log.out <- sum(log(1:n))
    if(log == TRUE)
      {
        return(log.out)
      }
    else
      {
        out <- exp(log.out)
        return(out)
      }
  }



###----------------------------------------------------------------------------
### Question 3.1
###----------------------------------------------------------------------------

# 3.1
Apple <- read.csv("Apple.csv", sep = ";")
Google <- read.csv("Google.csv", sep = ";")

# 3.2
apply(Apple, 2, mean)
colMeans(Apple)
apply(Apple, 2, sd)

# 3.3
mad <- function(x)
  {
    return(mean(abs(x-mean(x))))
  }

# 3.4
apply(Apple, 2, mad)

# 3.5
AppleLst <- as.list(Apple)
GoogleLst <- as.list(Google)

# 3.6
extremeVal <- function(x)
  {
    is.extreme <- ((x >  700) | (x < 100))
    out <- x[is.extreme]
    return(out)
}

# 3.7
lapply(Google, extremeVal)


###----------------------------------------------------------------------------
### Question 4
###----------------------------------------------------------------------------

logLikeNorm <- function(x, mu, sigma)
  {
   logDens <- -log(2*pi)/2-log(sigma)-(x-mu)^2/(2*sigma^2)
   out <- sum(logDens)
   return(out)
  }

###----------------------------------------------------------------------------
### Question 5
###----------------------------------------------------------------------------

## The first try
isIn <- function(a, b)
  { 
    j <- 1
    while(j <= length(b))
      {
        if(a == b[j])
         {out <- TRUE}
        else
          {out <- FALSE}
        j <- j + 1
      }
    return(out)
  }


debug(isIn)
isIn(3, 1:5)


## Hint: the while loop overwrites the "out" variable
isIn <- function(a, b)
  { 
    j <- 1
    while(j <= length(b))
      {
        if(a == b[j])
         {
           out <- TRUE
           break # Alternative 1: return(out) # Alternative 2: j <- length(b)+1
        }
        else
          {
            out <- FALSE
            j <- j + 1
          }
      }
    return(out)
  }



## The second try
isIn <- function(a, b)
  { 
    out <- rep(NA, length(a))    
    j <- 0
    while(j <= length(b))
      {
        j <- j + 1
        for ( i in 1:length(a))
          {out[i] <- (a[i] == b[j])}
      }
    return(out)
  }


## Mattias solution for the second try:
isIn <- function(a, b)
{
  out <- rep(FALSE, length(a))
  i <- 1
  while(i <= length(a))
  {
    for ( j in 1:length(b))
    {if (a[i] == b[j]) out[i] <- TRUE }
    i <- i + 1
  }
  return(out)
}
