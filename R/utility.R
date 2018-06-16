# function to make sure vals parameter is okay
check_param_vals <- function(vals) {
  # make sure vals and r can be converted to numeric and integer, respectively
  vals <- as.numeric(vals)
  if (sum(is.na(vals)) > 0) {
    stop("vals must be of class numeric")
  }

  # vals must have at least 3 elements
  if (length(vals) < 3) {
    stop(paste("vals parameter must have at least 3 elements,",
               "but only has", length(vals)))
  }
}

# function to make sure r parameter is okay
check_param_r <- function(r, n) {
  # make sure r can be converted to integer
  r <- as.integer(r)
  if (is.na(r)) {
    stop("r must be of class integer")
  }

  # r must have exactly 1 element
  if (length(r) != 1) {
    stop(paste("r parameter must have exactly 1 element,",
               "but instead has", length(r)))
  }

  # 0 < r < n must be true
  if (r <= 0 || r >= n) {
    stop(paste("r must be between 0 and", n, "(non-inclusive),",
               "but instead is equal to", r))
  }
}

# function to make sure m parameter is okay
check_param_m <- function(m, r) {
  # make sure m can be converted to integer
  m <- as.integer(m)
  if (is.na(m)) {
    stop("m must be of class integer")
  }

  # m must have exactly 1 element
  if (length(m) != 1) {
    stop(paste("m parameter must have exactly 1 element,",
               "but instead has", length(m)))
  }

  # 0 < r <= m must be true (m >= 1 implicitly tested)
  if (r <= 0) {
    stop(paste("r must be greater than 0,",
               "but instead is equal to", r))
  } else if (r > m) {
    stop(paste("m must be greater than or equal to r, but m is",
               m, "and r is", r))
  }
}
