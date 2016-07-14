#' Checks if GNU bc is availiable in the system
#' 
#' @export
chkGnuBc <- function() {
  versionString <-
    tryCatch(
      expr = system('bc --version', intern = TRUE, ignore.stderr = TRUE),
      error = function(e) {
        message("GNU bc is not availiable.")
        return(c())
      }
    )
  if (length(versionString) == 0L)
    return(FALSE)
  
  message(versionString[1])
  return(TRUE)
}

getBcCodeConvertBase <- function(val, ibase, obase) {
  if (ibase == 2 & obase == 2)
    obase <- 10
  
  else if (ibase == 2 & obase == 10)
    obase <- 1010
  
  else if (ibase == 2 & obase == 16)
    obase <- 10000
  
  else if (ibase == 16 & obase == 10)
    obase <- 'A'
  
  else if (ibase == 16 & obase == 16)
    obase <- '10'
  
  paste(
    paste0('ibase=', formatNonSci(ibase)),
    paste0('obase=', formatNonSci(obase)),
    formatNonSci(val),
    sep = ';'
  ) -> code
  
  return(code)
}

bcInterface <- function(code) {
  paste0("echo '", code, "' | bc") %>%
    system(intern = TRUE) -> result
  return(result)
}
