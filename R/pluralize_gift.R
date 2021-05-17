#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gift){

  gift <- as.vector(gift)
  gift <- as.character(gift)
  if (str_detect(gift, "oo")){
    return("geese")
  } else if(str_detect(gift, "partridge")){
    return("partridge")
  }else if (str_detect(gift, "y$")){
    return("ladies")
  }else{
    gift <- gift %>%
      str_replace("$","s")
  }

  return(gift)
}
