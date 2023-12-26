where_not_zero <- function(yVals, ignore = FALSE) {
  if (ignore)
    return(yVals)

  stopifnot(is.numeric(yVals))

  is_less_than_epsilon <- yVals < 1e-10
  is_greater_than_epsilon <- yVals > -1e-10
  yval_is_na <- is.na(yVals)

  ! (yval_is_na | (is_less_than_epsilon & is_greater_than_epsilon))
}
