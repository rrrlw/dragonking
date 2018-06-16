#####DK TEST (Pisarenko 2012)#####
dk_test <- function(vals, r) {
  ## make sure all parameters look okay
  # make sure vals and r can be converted to numeric and integer, respectively
  vals <- as.numeric(vals)
  r <- as.integer(r)
  if (sum(is.na(vals)) > 0) {
    stop("vals must be of class numeric")
  }
  if (sum(is.na(r)) > 0) {
    stop("r must be of class integer")
  }

  # vals must have at least 3 elements
  n <- length(vals)
  if (n < 3) {
    stop(paste("vals parameter must have at least 3 elements,",
               "but only has", n))
  }

  # r must have exactly 1 element
  if (length(r) != 1) {
    stop(paste("r parameter must have exactly 1 element,",
               "but instead has", length(r)))
  }

  # integer value of all r elements must satisfy 0 < r < n
  if (r < 0 || r >= n) {
    stop(paste("r must be between 0 and", n, "(non-inclusive),",
               "but instead is equal to", r))
  }

  ## calculate test statistic (Pisarenko & Sornette, 2012)
  # preliminary calculations
  x <- sort(vals, decreasing = TRUE)
  y <- vapply(X = 1:n,
              FUN.VALUE = numeric(1),
              FUN = function(i) {
                if (i == n) {
                  return(x[i])
                } else {
                  return(x[i] - x[i + 1])
                }
              })
  z <- (1:n) * y

  # formula given in Pisarenko & Sornette 2012, p. 3 (of article)
  num <- 1 / r * sum(z[1:r])
  den <- 1 / (n - r) * sum(z[(r + 1):n])
  test.stat <- num / den

  ## calculate p-value
  p.val <- pf(q = test.stat,
              df1 = 2 * r, df2 = 2 * (n - r),
              lower.tail = FALSE)

  ## return test statistic, p-value
  ans <- c(test.stat, p.val)
  names(ans) <- c("Test Statistic", "p-value")
  return(ans)
}
