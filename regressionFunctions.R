g <- function(z) {
    return(1/(1+exp(-z)))
}

Jtest <- function(X, y, theta) {
    #X <- matrix(c(c(1, 2, 3), c(1,2,3), c(1,2,3)), 3)
    #theta <- c(1, 2, 3)
    #y <- c(10, 20, 30)
    m <- length(y)
    return(sum(((X%*%theta)-y)^2)/(2*m))
}
