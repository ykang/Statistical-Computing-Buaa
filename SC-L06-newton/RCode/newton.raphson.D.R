newton.raphson <- function(f, init.value, df = NULL, tol = 1e-5, maxiter = 1000) {
  if (is.null(df)) {
    df <- D(f, 'x')
  }
  niter <- 0
  diff <- tol + 1
  x <- init.value
  while (diff >= tol && niter <= maxiter) {
    niter <- niter + 1
    fx <- eval(f)
    dfx <- eval(df)
    if (dfx == 0) {
      warning("Slope is zero: no further improvement possible.")
      break
    }
    diff <- -fx/dfx
    x <- x + diff
    diff <- abs(diff)
  }
  if (niter > maxiter) {
    warning("Maximum number of iterations 'maxiter' was reached.")
  }
  return(list(root = x, f.root = fx, niter = niter, estim.prec = diff))
}

f <- expression(x^2 - 5)
# f <- expression(abs(x) ^ 0.5)
# f <- expression(x*exp(-x^2) - 0.4*(1/(exp(x)+1)) - 0.2)
# df <- expression(ifelse((x > 0), 0.5 * (x ^ (-0.5)), -0.5 * ((-x) ^ (-0.5))))
newton.raphson(f, 0)

f <- function(x){x^2 - 5}
curve(f,-10,10)

