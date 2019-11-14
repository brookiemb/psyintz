#' Score Docs
#'
#' It scores the DOCS (the Dimensional Obsessive-Compulsive Scale).
#'
#' @param data the data frame
#' @param germs the germs subscale
#' @param harm the harm subscale
#' @param thoughts the thoughts subscale
#' @param symmetry the symmetry subscale
#' @param minus1 do we want to minus 1 from the raw scores; \code{default = TRUE}
#'
#' @import dplyr
#' @import furniture
#'
#' @export
score.docs <- function(data, germs, harm, thoughts, symmetry, minus1 = TRUE){

  if (!is.data.frame(data)){
    stop("data argument must be a data frame")
  }

  if (minus1){
    data = minus1(data, "docs")
  }

  data %>%
    mutate(docs.germs = furniture::rowsums()) %>%
    mutate(docs.harm = rowSums(data[, harm])) %>%
    mutate(docs.thoughts = rowSums(data[, thoughts])) %>%
    mutate(docs.symmetry = rowSums(data[, symmetry])) %>%
    mutate(docs = docs.germs + docs.harm + docs.thoughts + docs.symmetry)

}


#' Score Docs
#'
#' It scores the DOCS (the Dimensional Obsessive-Compulsive Scale).
#'
#' @param data the data frame
#' @param germs the germs subscale
#' @param harm the harm subscale
#' @param thoughts the thoughts subscale
#' @param symmetry the symmetry subscale
#' @param minus1 do we want to minus 1 from the raw scores; \code{default = TRUE}
#'
#' @import dplyr
#'
#' @export


# hi


