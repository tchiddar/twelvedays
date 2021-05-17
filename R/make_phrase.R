#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export



make_phrase <- function(num, num_word, item, verb, adjective, location){
  
  num_word <- english::english(num)
  verb <- str_replace_na(verb,"")
  location <- str_replace_na(location,"")
  adjective <- str_replace_na(adjective,"")
  
  
  phrase <- paste(num_word,adjective,item,verb,location)
  phrase <- str_replace_all(phrase, "  ", " ")
  phrase <- str_remove(phrase, "\\s$")
  return(phrase)
}

