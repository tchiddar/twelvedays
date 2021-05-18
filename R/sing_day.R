#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(data, line, phrase_col){
  data <- data %>% 
    filter(Day <= line)
  
  phrases <- data %>% pull({{phrase_col}})
  startingline <- paste("On the",data$Day.in.Words[line], "day of Christmas, my true love gave to me,")
  
  if (line > 1){
    phrases[1] <- paste("and", phrases[1])
  }

  all <- paste(phrases[line:1]) #%>% str_c(collapse = ",")
  allofit <- paste( all)
  cat(startingline, sep = "\n")
  cat(allofit, sep = "\n")
}


test_that("the line argument must be numeric", {
  expect_error(sing_day(data, "5", phrase_col = Full.Phrase))
})
