#####DK TEST (Pisarenko 2012)#####
dk_test <- function(vals, r) {
  ## make sure all parameters look okay
  # should throw stop() ERRORs if something's not right
  check_param_vals(vals)
  check_param_r(r)

  vals <- as.numeric(vals)
  r <- as.integer(r)
  n <- length(vals)

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
