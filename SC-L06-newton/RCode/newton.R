newton <- function(f3, x0, df = NULL, tol = 1e-9, n.max = 100) {
  # Newton's method for optimisation, starting at x0
  # f3 is a function that given x returns the list
  # {f(x), grad f(x), Hessian f(x)}
  
  x <- x0
  f3.x <- f3(x)
  n <- 0
  while((max(abs(f3.x[[2]])) > tol) & (n < n.max)){
    x <- x - solve(f3.x[[3]], f3.x[[2]])
    f3.x <- f3(x)
    n <- n +1
  }
  if (n == n.max){
    cat('newton failed to converge \n')
  } else{
    return(x)
  }
}

df <- deriv(z ~ x1^2 -x1 * x2 + x2^2 + exp(x2),
            c('x1', 'x2'), func = TRUE, hessian = TRUE)
f3 <- function(x){
  dfx <- df(x[1], x[2])
  f <- dfx[1]
  gradf <- attr(dfx, 'gradient')[1,]
  hessf <- attr(dfx, 'hessian')[1,,]
  return(c(f, gradf, hessf))
}

newton(f3, c(1, 1))

