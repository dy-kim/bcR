assureNumeric <- function(x) {
  result <- tryCatch(
    expr = as.numeric(x),
    error = function(e) {
      return(NULL)
    },
    warning = function(w) {
      if (w$message == "NAs introduced by coercion")
        return(NULL)
    }
  )
  return(result)
}

formatNonSci <- function(value) {
  return(format(value, scientific = FALSE))
}

asNumericFullSignificantFigure <- function(value) {
  expandSciNotation(value)
  return(as.numeric(value))
}

expandSciNotation <- function(value) {
  options(scipen = max(nchar(value)))
}
