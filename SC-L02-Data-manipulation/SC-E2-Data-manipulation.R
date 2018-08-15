## Create a sequence

## Sequence with special patterns

## 1, 2, 3, ..., n

rep()


## seq(-5, 5, length.out = 8)
## seq(-5, 5, length = 8)
## seq(-5, 5, length.o = 8)

rep(1:5, each = 3)

cbind(rep(1:5, 5), rep(1:5, each = 5))

cbind(1:3, 2:4, 4:6) # combine vectors

mycbind <- function(...)
    {
        args <- list(...)

        print(args)

        nargs <- length(args)
        out <- NULL

        for(i in 1:nargs)
            {
                out <- cbind(out, args[[i]])
            }

        return(out)
    }


A <- matrix(1:24, 4, 6)


B <- array(1:24, c(2, 3, 4))



## Matrices

A  = matrix(1:24, 4, 12)



## Array
