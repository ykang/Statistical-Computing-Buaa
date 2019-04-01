
#Lab 7:find the maximum likelihood estimate for the coefficients in a logistic regression

#1: Write down likelihood function.

  ## yi~B(1,pi),pi = exp(gi)/(1+exp(gi)) = P(Yi = 1)
  ## g(xi) = beta0 + beta1*xi1 + ?????? + betaq*xiq
  ## P(yi)=pi^(yi)*(1-pi)^(1-yi)
  ## logL(beta)=sum{logP(Yi)}
  ##           =sum{yi*log(pi)+(1-yi)*log(1-pi)}
  ##           =sum{yi*gi-log(1 + exp(gi))}

#2: Find the gradient and Hessian matrix.

## gradient vector (q+1)*1

  ## matrix(c(sum(y-exp(g)/(1+exp(g))),sum(y*x,1-x,1*exp(g)/(1+exp(g))),sum(y*x,2-x,2*exp(g)/(1+exp(g))),...),q+1,1)

## Hessian matrix (q+1)*(q+1)

  ## {sum(-exp(g)/(1+exp(g))^2),sum(-x,1*exp(g)/(1+exp(g))^2),sum(-x,2*exp(g)/(1+exp(g))^2),...
  ##  sum(-x,1*exp(g)/(1+exp(g))^2),sum(-x,1*x,1*exp(g)/(1+exp(g))^2),sum(-x,1*x,2*exp(g)/(1+exp(g))^2),...
  ##  ...
  ##  sum(-x,q*exp(g)/(1+exp(g))^2),sum(-x,q*x,1*exp(g)/(1+exp(g))^2),...,sum(-x,q*x,q*exp(g)/(1+exp(g))^2)}

#3: Code these up in R.

Multinewtonrephson <- function(fun,vari,grad=NULL,hess=NULL,e){
  #Newton??s algorithm for multidimensional optimization
  #"fun"  : expression of function
  #"vari" :  initial value of variables
  #"grad" : function of gradient vector ."hess" : function of hessian matrix 
  #"e" : accuracy
   
  n = 1
  while (n > 0) {
    if (is.null(grad) | is.null(hess)){
      #if the input of grad and hess are null,we need to compute them use function "DF"
      grad1 <- DF(fun,vari)$grad
      hess1 <- DF(fun,vari)$hess
    }  
    else {
      grad1 <- grad(vari)
      hess1 <- hess(vari)
    }
    vari <- vari-solve(hess1,grad1)
    if (all(abs(grad1) < e)){
      if (all(eigen(hess1)$value>0)){
        #judge wheather the matrix is positive definite matrix
        print("minimum")
      }
      else if (all(eigen(hess1)$value<0)){
        print("maximum")
      }
      else {print("Result needs further testing")}
      break
    }
    else 
    n <- n+1
  }
    
  return(list(value = vari,
              gradient = grad,
              Hessian = hess,
              n = n,
              grad1 = grad1))
}

DF <- function(fun,vari){
  #compute the gradient vector and hessian matrix
  #"fun" : expression of function
  #"vari" : initial value of variables
  p <- length(vari)
  df <- deriv(fun,c(letters[1:p]),hessian = T,func = T)
  dfx <- do.call(df,as.list(vari))
  hess = matrix(as.vector(attr(dfx,"hessian")),nrow = 2)
  grad = matrix(attr(dfx,"gradient"),nrow = 2)
  return(list(grad,hess))
}


fun <- function(beta){
  g<-beta[1]+beta[2]*x
  logl <- sum(y*g-log(1 + exp(g)))
  # if(is.infinite(logl) | is.na(logl)){
  #   logl <- (-999999)
  # }
  return(logl)
}

grad <- function(beta){
  g<-beta[1]+beta[2]*x
  matrix(c(sum(y-1/(1+exp(-g))),sum(y*x-x/(1+exp(-g)))),2)
}

hess <- function(beta){
  g<-beta[1]+beta[2]*x
  matrix(c(sum(-1/(1+exp(-g))^2),
           sum(-x/(1+exp(-g))^2),
           sum(-x/(1+exp(-g))^2),
         sum(-x*x/(1+exp(-g))^2)),2,2)
}

#4: Simulate some data from a logistic regression model and test your code.

## generate some data
# beta<-c(1,2)
# g<-rlogis(100)#
# p<-exp(g)/(1+exp(g))
# x<-(g-beta[1])/beta[2]
# y<-rbinom(100,1,p)

beta<-c(1,2)
x <- rnorm(1000)
g<-beta[1]+beta[2]*x
p<-exp(g)/(1+exp(g))
y<-rbinom(1000,1,p)


## test my code
optim <- Multinewtonrephson(fun,c(-3,4),grad,hess,1e-10)
beta0Hat <- optim$value[1]
beta1Hat <- optim$value[2]
ghat<-beta0Hat + beta1Hat*x 
phat<-1/(1+exp(-ghat))

### Mistake: When the initial beta I specify less than the true value or one of them is nagetive,I can't get the right answer.

## plot  
plot(x, p, pch = 20, col = "blue")
points(sort(x), phat[order(x)], type = "l", col = "red", lwd = 2)

## compare with GLM
logit <- glm(y~x,family = binomial)
betahat <- logit$coefficients
logphat <- 1/(1+exp(-betahat[1]-betahat[2]*x))
plot(x, p, pch = 20, col = "blue")
points(sort(x), logphat[order(x)], type = "l", col = "green",lty = "dashed" ,lwd = 2)


####################################

#Lab8 :Use optim() to carry out maximum likelihood for the Logistic regression model

optim(c(3,4),fun,control=list(fnscale=-1))
