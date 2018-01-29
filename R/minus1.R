#' Minus One
#'
#' It takes the variable and minuses one. For internal use in this package.
#'
#' @param data the data frame
#' @param x the variable (in quotes)
#'
#' @export
minus1 <- function(data, x){

  for(i in names(data)[grepl(x, names(data))]){
    data[ , i] = data[ , i] - 1
  }

  return(data)
}

