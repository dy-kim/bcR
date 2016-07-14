#' Convert base of a numeric (vectorized)
#'
#' @param x A numeric to convert
#' @param inputBase A character among \code{c('2', '10', '16')}
#' @param outputBase A character among \code{c('2', '10', '16')}
#'
#' @export
convertBase <- function(x, inputBase, outputBase) {
  # First define a scalar function and then vectorize it using base::Vetorize()
  .convertBaseScalar <-
    function(x, inputBase, outputBase) {
      stopifnot(chkGnuBc())
      stopifnot(!missing(x))
      
      inputBase  %<>% as.character() %>% match.arg(choices = getConvertibleBase())
      outputBase %<>% as.character() %>% match.arg(choices = getConvertibleBase())
      
      inputVal <- ifelse(
        test = (inputBase == '16'),
        yes = toupper(x),
        no = assureNumeric(x)
      )
      
      if (is.null(inputVal)) {
        message('Input value is not numeric. Return NULL.')
        return(NULL)
      }
      
      getBcCodeConvertBase(inputVal, inputBase, outputBase) %>%
        bcInterface() -> result
      
      result <-
        ifelse(
          test = (outputBase == '10'),
          yes = asNumericFullSignificantFigure(result),
          no = result
        )
      
      return(result)
    }
  
  .convertBaseVectorized <-
    Vectorize(FUN = .convertBaseScalar,
              vectorize.args = 'x')
  
  return(.convertBaseVectorized(x, inputBase, outputBase))
}


######################
#  Applied Functions
######################

#' Convert hexa value to decimal value
#'
#' @export
convertHex2Dec <- function(hexaValue) {
  result <-  convertBase(x = hexaValue,
                         inputBase  = '16',
                         outputBase = '10')
  
  return(result)
}

#' Convert decimal value to hexa value
#'
#' @export
convertDec2Hex <- function(decValue) {
  decVal <- assureNumeric(decValue)
  
  if (is.null(decVal)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  result <- convertBase(x = decVal,
                        inputBase  = '10',
                        outputBase = '16')
  
  return(result)
}

#' Convert decimal value to binary value
#'
#' @export
convertDec2Bin <- function(decValue) {
  decVal <- assureNumeric(decValue)
  
  if (is.null(decVal)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  result <- convertBase(x = decVal,
                        inputBase  = '10',
                        outputBase = '2')
  
  return(result)
}

#' Convert binary value to decimal value
#'
#' @export
convertBin2Dec <- function(binValue) {
  binVal <- assureNumeric(binValue)
  
  if (is.null(binVal)) {
    message('Input value is not numeric. Return NULL.')
    return(NULL)
  }
  
  result <- convertBase(x = bin.val,
                        inputBase  = '2',
                        outputBase = '10')
  
  return(result)
}


#' @export
getConvertibleBase <- function() {
  return(c('2', '10', '16'))
}
