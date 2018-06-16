#####SUM-SUM (SS) TEST STATISTIC#####
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
dixon_stat <- function(vals, r) {
  # make sure all parameters look okay
  check_param_vals(vals)
  check_param_r(r, length(vals))

  # calculate test statistic
  test.stat <- vals[1] / vals[r + 1]

  # return test statistic
  return(test.stat)
}
