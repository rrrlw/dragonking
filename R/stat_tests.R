#####DK TEST (Pisarenko 2012)#####
#' Statistical test to identify dragon kings (DKs)
#'
#' \code{dk_test} runs the DK test on the user parameters and returns a
#' test statistic and corresponding p-value to aid in determining whether
#' there is significant support for the existence of \code{r} DKs in
#' \code{vals}.
#' 
#' @references Wheatley S, Sornette D (2015). Multiple outlier detection in samples with exponential & pareto tails: Redeeming the inward approach & detecting dragon kings. Swiss Finance Institute Research Paper Series No. 15-28. <doi:10.2139/ssrn.2645709>
#' @references Pisarenko VF, Sornette D (2012). Robust statistical tests of dragon-kings beyond power law distributions. \emph{Eur Phys J Special Topics}, \strong{205}: 95-115. <doi:10.1140/epjst/e2012-01564-8>
#'
#' @param vals  numeric vector with at least 3 elements
#' @param r integer indicating number of DKs in \code{vals}
#' @return DK test statistic and p-value (F distribution)
#' @export
#' @examples
#' # generate a numeric vector with DKs
#' temp <- c(rexp(100),   # exponentially distributed RV
#'           15, 15, 15)  # DK elements
#'
#' # test for DKs, where r is number of DKs thought to be in temp
#' results <- dk_test(temp, r = 3)
#'
#' # print out test statistic (should be large) and p-value (should be small)
#' print(paste("Test statistic =", results["Test Statistic"]))
#' print(paste("p-value =", results["p-value"]))
dk_test <- function(vals, r) {
  ## make sure all parameters look okay
  # should throw stop() ERRORs if something's not right
  check_param_vals(vals)
  check_param_r(r, length(vals))

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
  p.val <- stats::pf(q = test.stat,
                     df1 = 2 * r, df2 = 2 * (n - r),
                     lower.tail = FALSE)

  ## return test statistic, p-value
  ans <- c(test.stat, p.val)
  names(ans) <- c("Test Statistic", "p-value")
  return(ans)
}
