where_not_zero <- function(yVals, ignore = FALSE) {
  if (ignore)
    return(yVals)

  stopifnot(is.numeric(yVals))

  is_less_than_epsilon <- yVals < 1e-10
  is_greater_than_epsilon <- yVals > -1e-10

  !(is_less_than_epsilon & is_greater_than_epsilon)
}
