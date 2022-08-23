

#' Conversion from language code to language name
#'
#' Takes a language code (defined in the Marc standards)
#' and returns the language name.
#'
#' @import data.table
#'
#' @param x A language code (defined in the Marc standards) or a vector
#'          of language codes
#'
#' @return Returns the language name. NA if cannot be matched
#'         to language in standard.
#'
#' @examples
#'
#' get_language_from_code("yor")
#' # Yoruba
#'
#' # tolerant of case and leading/trailing whitespace
#' get_language_from_code(c("yor", " SPA ", "not-a-language", "nah", NA))
#' # c("Yoruba", "Spanish", NA, "Nahuatl", NA)
#'
#' @export
get_language_from_code <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")
  x <- stringr::str_trim(stringr::str_to_lower(x))
  thekey <- language <- language_code_crosswalk <-  NULL
  theinput <- data.table::data.table(thekey=x)
  data.table::setindex(theinput, thekey)
  data("language_code_crosswalk", envir = environment())
  result <- language_code_crosswalk[theinput, on="thekey"]
  return(result[, language])
}


#' Conversion from country code to country name
#'
#' Takes a country code (defined in the Marc standards)
#' and returns the country name.
#'
#' @details
#' Interestingly, although it's called 'country' in the Marc standard,
#' cities, states, and other non-countries also have codes
#'
#' @import data.table
#'
#' @param x A country code (defined in the Marc standards) or a vector
#'          of country codes
#'
#' @return Returns the country (place) name. NA if cannot be matched
#'         to country in standard.
#'
#' @examples
#'
#' get_country_from_code("ck")
#' # Colombia
#'
#' # tolerant of case and leading/trailing whitespace
#' get_country_from_code(c(" PE", "not-a-country", "nyu"))
#' # c("Peru", NA, "New York (State)")
#'
#' @export
get_country_from_code <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")
  x <- stringr::str_trim(stringr::str_to_lower(x))
  thekey <- country <- country_code_crosswalk <-  NULL
  theinput <- data.table::data.table(thekey=x)
  data.table::setindex(theinput, thekey)
  data("country_code_crosswalk", envir = environment())
  result <- country_code_crosswalk[theinput, on="thekey"]
  return(result[, country])
}







