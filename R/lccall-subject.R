

###################################################
###     Conversion from LC Calls to subject     ###
###################################################

#' Conversion from Library of Congress Call number to subject
#'
#' Takes a string representation of a Library of Congress
#' call number and returns either the broad subject description
#' (default) based on the first letter, or a second level
#' description based on the
#'
#' @param x A Library of Congress call number (string)
#' @param second.level A logical indicating whether the letters of
#'     call number past the first should be used to match to
#'     a subject
#' @param aggressive Attempts to make out a legitimate LC call number
#'
#'
#' @return Returns either the broad (top-level) subject description
#'     or the second level subject description. Returns "NA" if
#'     no subject could not be matched
#' @examples
#'
#' get_lc_call_subject("ND 237.S18 $b S87 1997")
#' # Fine Arts
#'
#' #' get_lc_call_subject("ND 237.S18 $b S87 1997", second.level=TRUE)
#' # Painting
#'
#' get_lc_call_subject("PQ2246.M3")
#' # Language and Literature
#'
#' get_lc_call_subject(c("ND 237", "\\\\$a ND 2", "PQ2246.M3"),
#'                     second.level=TRUE, aggressive=FALSE)
#' # c("Painting", NA, "French, Italian, Spanish, and Portuguese literature")
#'
#' get_lc_call_subject(c("ND 237", "\\\\$a ND 2", "PQ2246.M3"),
#'                     second.level=TRUE, aggressive=TRUE)
#' # c("Painting", "Painting", "French, Italian, Spanish, and Portuguese literature")
#'
#'
#' @export
get_lc_call_subject <- function(x, second.level=FALSE, aggressive=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")

  theinput <- data.table::data.table(usersupplied=x)

  SUBREGEX <- "\\s*([A-Za-z]{1,3}).*"
  if(aggressive){
    theinput$usersupplied <- stringr::str_replace(theinput[, usersupplied], SUBREGEX, "$1")
  }

  if(second.level){
    theinput$thekey <- stringr::str_extract(theinput[, usersupplied],
                                            "^[A-Z]{1,3}(?=[^A-Z])")
  } else{
    theinput$thekey <- stringr::str_extract(theinput[, usersupplied],
                                            "^[A-Z]{1}")
  }

  data.table::setindex(theinput, thekey)

  if(second.level){
    result <- second_level[theinput, on="thekey"]
  } else{
    result <- broad[theinput, on="thekey"]
  }

  return(result[, description])
}

