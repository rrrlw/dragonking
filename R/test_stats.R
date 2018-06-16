#####SUM-SUM (SS) TEST STATISTIC#####
#' Sum-sum (SS) test statistic to identify dragon kings (DKs)
#'
#' \code{ss_stat} calculates the SS test statistic to determine whether
#' there is significant support for the existence of \code{r} DKs in
#' \code{vals}. This test is susceptible to swamping.
#'
#' For more information, see:
#'
#' Wheatley S, Sornette D (2015). Multiple outlier detection in samples with
#' exponential & pareto tails: Redeeming the inward approach & detecting
#' dragon kings. Swiss Finance Institute Research Paper Series No. 15-28. doi:
#' 10.2139/ssrn.2645709
#'
#' Balakrishnan K (1996). Exponential distribution: Theory, methods and
#' applications. \emph{CRC Press}. pp. 228-30. ISBN: 9782884491921
#'
#' Chikkagoudar MS, Kunchur SH (1983). Distributions of test statistics for
#' multiple outliers in exponential samples. \emph{Commun Stat Theory Methods}, \strong{12}:
#' 2127-42. doi: 10.1080/03610928308828596
#'
#' Lewis T, Fieller NRJ (1979). A recursive algorithm for null distributions
#' for outliers: I gamma samples. \emph{Technometrics}, \strong{21}(3): 371-6. doi:
#' 10.2307/1267762
#'
#' @param vals  numeric vector with at least 3 elements
#' @param r integer indicating number of DKs in \code{vals}
#' @return SS test statistic
#' @export
#' @examples
#' # generate a numeric vector with DKs
#' temp <- c(rexp(100),   # exponentially distributed RV
#'           15, 15, 15)  # DK elements
#'
#' # calculate test statistic for DKs
#' ss_stat(temp, r = 3)
ss_stat <- function(vals, r) {
  # make sure all parameters look okay
  check_param_vals(vals)
  check_param_r(r, length(vals))

  # calculate test statistic
  test.stat <- sum(vals[1:r]) / sum(vals)

  # return test statistic
  return(test.stat)
}

#####SUM-ROBUST-SUM (SRS) TEST STATISTIC#####
#' Sum-robust-sum (SRS) test statistic to identify dragon kings (DKs)
#'
#' \code{srs_stat} calculates the SRS test statistic to determine whether
#' there is significant support for the existence of \code{r} DKs in
#' \code{vals}. This test provides robustness to denominator masking.
#'
#' For more information, see:
#'
#' Wheatley S, Sornette D (2015). Multiple outlier detection in samples with
#' exponential & pareto tails: Redeeming the inward approach & detecting
#' dragon kings. Swiss Finance Institute Research Paper Series No. 15-28. doi:
#' 10.2139/ssrn.2645709
#'
#' Iglewicz B, Martinez J (1982). Outlier detection using robust measures of
#' scale. \emph{J Stat Comput Simul}, \strong{15}(4): 285-93. doi: 10.1080/00949658208810595
#'
#' @param vals  numeric vector with at least 3 elements
#' @param r integer indicating number of DKs in \code{vals}
#' @param m pre-specified maximum number of DKs in \code{vals}
#' @return SRS test statistic
#' @export
#' @examples
#' # generate a numeric vector with DKs
#' temp <- c(rexp(100),   # exponentially distributed RV
#'           15, 15, 15)  # DK elements
#'
#' # calculate test statistic for DKs
#' srs_stat(temp, r = 2, m = 3)
srs_stat <- function(vals, r, m) {
  # make sure all parameters look okay
  check_param_vals(vals)
  check_param_r(r, length(vals))
  check_param_m(m, r)

  # calculate test statistic
  test.stat <- sum(vals[1:r]) / sum(vals[(m + 1):length(vals)])

  # return test statistic
  return(test.stat)
}

#####MAX-SUM (MS) TEST STATISTIC#####
#' Max-sum (MS) test statistic to identify dragon kings (DKs)
#'
#' \code{ms_stat} calculates the MS test statistic to determine whether
#' there is significant support for the existence of \code{r} DKs in
#' \code{vals}. This statistic is less susceptible to swamping, but is also
#' less powerful in the case of clustered outliers, in comparison to the SS
#' and SRS test statistics.
#'
#' For more information, see:
#'
#' Wheatley S, Sornette D (2015). Multiple outlier detection in samples with
#' exponential & pareto tails: Redeeming the inward approach & detecting
#' dragon kings. Swiss Finance Institute Research Paper Series No. 15-28. doi:
#' 10.2139/ssrn.2645709
#'
#' Hawkins DM (1980). Identification of outliers, vol. 11. \emph{Chapman and Hall}.
#' ISBN: 9789401539944
#'
#' Kimber AC (1982). Tests for many outliers in an exponential sample. \emph{Appl
#' Statist}, \strong{31}(3): 263-71. doi: 10.2307/2348000
#'
#' @param vals  numeric vector with at least 3 elements
#' @param r integer indicating number of DKs in \code{vals}
#' @return MS test statistic
#' @export
#' @examples
#' # generate a numeric vector with DKs
#' temp <- c(rexp(100),   # exponentially distributed RV
#'           15, 15, 15)  # DK elements
#'
#' # calculate test statistic for DKs
#' ms_stat(temp, r = 3)
ms_stat <- function(vals, r) {
  # make sure all parameters look okay
  check_param_vals(vals)
  check_param_r(r, length(vals))

  # calculate test statistic
  test.stat <- vals[r] / sum(vals[r:length(vals)])

  # return test statistic
  return(test.stat)
}

#####MAX-ROBUST-SUM (MRS) TEST STATISTIC#####
#' Max-robust-sum (MRS) test statistic to identify dragon kings (DKs)
#'
#' \code{mrs_stat} calculates the MRS test statistic to determine whether
#' there is significant support for the existence of \code{r} DKs in
#' \code{vals}. This test avoids denominator masking.
#'
#' For more information, see:
#'
#' Wheatley S, Sornette D (2015). Multiple outlier detection in samples with
#' exponential & pareto tails: Redeeming the inward approach & detecting
#' dragon kings. Swiss Finance Institute Research Paper Series No. 15-28. doi:
#' 10.2139/ssrn.2645709
#'
#' @param vals  numeric vector with at least 3 elements
#' @param r integer indicating number of DKs in \code{vals}
#' @param m pre-specified maximum number of DKs in \code{vals}
#' @return MRS test statistic
#' @export
#' @examples
#' # generate a numeric vector with DKs
#' temp <- c(rexp(100),   # exponentially distributed RV
#'           15, 15, 15)  # DK elements
#'
#' # calculate test statistic for DKs
#' mrs_stat(temp, r = 2, m = 3)
mrs_stat <- function(vals, r, m) {
  ## make sure all parameters look okay
  check_param_vals(vals)
  check_param_r(r, length(vals))
  check_param_m(m, r)

  ## calculate test statistic
  test.stat <- vals[r] / sum(vals[(m + 1):length(vals)])

  ## return test statistic
  return(test.stat)
}

#####DIXON TEST STATISTIC#####
#' Dixon test statistic to identify dragon kings (DKs)
#'
#' \code{dixon_stat} calculates the DIxon test statistic to determine whether
#' there is significant support for the existence of \code{r} DKs in
#' \code{vals}. This test is less susceptible to swamping and masking, but is
#' also less powerful than the SS and SRS test statistics.
#'
#' For more information, see:
#'
#' Wheatley S, Sornette D (2015). Multiple outlier detection in samples with
#' exponential & pareto tails: Redeeming the inward approach & detecting
#' dragon kings. Swiss Finance Institute Research Paper Series No. 15-28. doi:
#' 10.2139/ssrn.2645709
#'
#' Dixon WJ (1950). Analysis of extreme values. \emph{Ann Math Stat}, \strong{21}(4): 488-506.
#' doi: 10.1214/aoms/1177729747
#'
#' Likes J (1967). Distribution of Dixon's statistics in the case of an
#' exponential population. \emph{Metrika}, \strong{11}(1): 46-54. doi: 10.1007/bf02613574
#'
#' @param vals  numeric vector with at least 3 elements
#' @param r integer indicating number of DKs in \code{vals}
#' @return Dixon test statistic
#' @export
#' @examples
#' # generate a numeric vector with DKs
#' temp <- c(rexp(100),   # exponentially distributed RV
#'           15, 15, 15)  # DK elements
#'
#' # calculate test statistic for DKs
#' dixon_stat(temp, r = 3)
dixon_stat <- function(vals, r) {
  # make sure all parameters look okay
  check_param_vals(vals)
  check_param_r(r, length(vals))

  # calculate test statistic
  test.stat <- vals[1] / vals[r + 1]

  # return test statistic
  return(test.stat)
}
