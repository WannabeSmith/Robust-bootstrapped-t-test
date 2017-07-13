library(parallel)
library(data.table)

get.t.stat <- function(x, y, var.equal = FALSE)
{
  n1 <- length(x)
  n2 <- length(y)
  
  if (var(x) == 0 && var(y) == 0)
  {
    stop("Data are essentially constant")
  }
  
  if(var.equal)
  {
    s_p <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
    return((mean(x) - mean(y)) / (s_p * sqrt(1/n1 + 1/n2)))
  }
  else
  {
    s_d = sqrt(var(x)/n1 + var(y)/n2)
    return((mean(x) - mean(y)) / s_d)
  }
}

robust.boot.t.1 <- function(x, y, n.boot, n.cores = 1)
{
  arguments <- as.list(match.call())
  
  x.star <- x / mean(x)
  y.star <- y / mean(y)
  
  output.list <- mclapply(seq(1:n.boot), function(i) 
  {
    boot.x <- sample(x.star, size = length(x.star), replace = TRUE)
    boot.y <- sample(y.star, size = length(y.star), replace = TRUE)
    
    t.boot <- get.t.stat(boot.x, boot.y, var.equal = TRUE)
    return(t.boot)
  }, mc.cores = n.cores)
  
  output <- unlist(output.list)
  
  t <- get.t.stat(x, y, var.equal = TRUE)
  
  p.val <- sum(abs(output) > abs(t)) / n.boot
  
  return(p.val)
}

robust.boot.t.2 <- function(x, y, n.boot, n.cores = 1)
{
  arguments <- as.list(match.call())

  x.star <- x / mean(x)
  y.star <- y / mean(y)
  
  x.y.star <- c(x.star, y.star)
  
  output.list <- mclapply(seq(1:n.boot), function(i) 
  {
    boot.x <- sample(x.y.star, size = length(x.star), replace = TRUE)
    boot.y <- sample(x.y.star, size = length(y.star), replace = TRUE)
    
    t.boot <- get.t.stat(boot.x, boot.y, var.equal = FALSE)
    return(t.boot)
  }, mc.cores = n.cores)
  
  output <- unlist(output.list)
  
  t <- get.t.stat(x, y, var.equal = FALSE)
  
  if (t > 0)
  {
    p.val <- 2 * (sum(output > t) / n.boot)
  }
  else
  {
    p.val <- 2 * (sum(output < t) / n.boot)
  }
  
  return(p.val)
}

robust.boot.t.combined <- function(x, y, n.boot, n.cores = 1)
{
  arguments <- as.list(match.call())
  
  x.star <- x / mean(x)
  y.star <- y / mean(y)
  
  x.y.star <- c(x.star, y.star)
  
  output.list <- mclapply(seq(1:n.boot), function(i) 
  {
    boot.x.1 <- sample(x.star, size = length(x.star), replace = TRUE)
    boot.y.1 <- sample(y.star, size = length(y.star), replace = TRUE)
    boot.x.2 <- sample(x.y.star, size = length(x.star), replace = TRUE)
    boot.y.2 <- sample(x.y.star, size = length(y.star), replace = TRUE)
    
    t.boot.1 <- get.t.stat(boot.x.1, boot.y.1, var.equal = TRUE)
    t.boot.2 <- get.t.stat(boot.x.2, boot.y.2, var.equal = FALSE)
    
    return(c(t.boot.1, t.boot.2))
  }, mc.cores = n.cores)
  
  output <- do.call(rbind, output.list)
  
  t.1 <- get.t.stat(x, y, var.equal = TRUE)
  t.2 <- get.t.stat(x, y, var.equal = FALSE)
  
  p.val.1 <- p.val <- sum(abs(output[,1]) > abs(t.1)) / n.boot
  
  if (t.2 > 0)
  {
    p.val.2 <- 2 * (sum(output[,2] > t.2) / n.boot)
  }
  else
  {
    p.val.2 <- 2 * (sum(output[,2] < t.2) / n.boot)
  }
  
  p.val.table <- c("t1" = p.val.1, "t2" = p.val.2)
  
  return(p.val.table)
}

#' Perform a robust bootstrapped t-test
#'
#' @importFrom parallel mclapply
#' @importFrom stats var
#' @param x a vector of numeric data points
#' @param y a vector of numeric data points
#' @param n.boot number of bootstrap resamples to perform
#' @param n.cores number of cores to use for parallelization. Defaults to 1. If using Windows, set n.cores = 1.
#' @param algorithm Which robust bootstrapped t-test to perform. Set 'algorithm = 1' for method 1, 'algorithm = 1' for method 2, and 'algorithm = "both"' to perform methods 1 and 2 simultaneously.
#' @return p-value of the test
#' @export
#' @examples
#' robust.boot.t(rnorm(20, 0, 1), rnorm(30, 0, 1), n.boot = 999)
#' robust.boot.t(rnorm(20, 0, 1), rnorm(30, 5, 1), n.boot = 999, n.cores = 2)
robust.boot.t <- function(x, y, n.boot, n.cores = 1, algorithm = "combined")
{
  if (Sys.info()['sysname'] == "Windows" && n.cores > 1)
  {
    warning("Multi-core processing is not supported on Windows. Setting n.cores to 1.")
    n.cores <- 1
  }
  
  if (algorithm == "combined")
  {
    return(robust.boot.t.combined(x, y, n.boot, n.cores))
  }
  else if (algorithm == 1)
  {
    return(robust.boot.t.1(x, y, n.boot, n.cores))
  }
  else if (algorithm == 2)
  {
    return(robust.boot.t.2(x, y, n.boot, n.cores))
  }
  else 
  {
    stop("Invalid algorithm specification.\n
         Use \"algorithm = 1\" or \"algorithm = 2\" or \"algorithm = \'both\'\"")  
  }
}

