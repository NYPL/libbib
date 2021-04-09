


###################################################
###     Conversion from LC Calls to subject     ###
###################################################

#' Conversion from Library of Congress Call number to subject
#'
#' Takes a string representation of a Library of Congress
#' call number and returns either the broad subject description
#' (default) based on the first letter, or a second level
#' description based on the first two letters
#'
#' @import data.table
#'
#' @param x A Library of Congress call number (string)
#' @param second.level A logical indicating whether the letters of
#'     call number past the first should be used to match to
#'     a subject
#' @param already.parsed Skips the extraction of the subject letters
#'        and jumps to the subject matching
#'
#' @return Returns either the broad (top-level) subject description
#'     or the second level subject description. Returns "NA" if
#'     no subject could not be matched or call number is invalid
#' @examples
#'
#' get_lc_call_subject("ND 237.S18 $b S87 1997")
#' # Fine Arts
#'
#' get_lc_call_subject("ND 237.S18 $b S87 1997", second.level=TRUE)
#' # Painting
#'
#' get_lc_call_subject("PQ2246.M3")
#' # Language and Literature
#'
#' get_lc_call_subject("PQ2246.M3")
#' # "French, Italian, Spanish, and Portuguese literature"
#'
#' get_lc_call_subject("PQ2246.M3", already.parsed=TRUE)
#' # NA
#'
#' get_lc_call_subject("PQ", already.parsed=TRUE)
#' # "French, Italian, Spanish, and Portuguese literature"
#'
#' get_lc_call_subject(c("ND 237", "\\\\$a ND 2", "PQ2246.M3"),
#'                     second.level=TRUE)
#' # c("Painting", NA, "French, Italian, Spanish, and Portuguese literature")
#'
#'
#' @export
get_lc_call_subject <- function(x, second.level=FALSE, already.parsed=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")

  thekey <- usersupplied <- description <- NULL
  lc_two_letter_subject <- lc_first_letter_subject <- NULL

  theinput <- data.table::data.table(usersupplied=x)

  if(already.parsed){
    theinput[, thekey:=usersupplied]
  } else{
    if(second.level){
      theinput[, thekey:=get_lc_subject_letters(usersupplied)]
    } else{
      theinput[, thekey:=get_lc_broad_letter(usersupplied)]
    }
  }

  data.table::setindex(theinput, thekey)

  if(second.level){
    data("lc_two_letter_subject", envir = environment())
    result <- lc_two_letter_subject[theinput, on="thekey"]
  } else{
    data("lc_first_letter_subject", envir = environment())
    result <- lc_first_letter_subject[theinput, on="thekey"]
  }

  return(result[, description])
}


#' Check if LC Call Number is valid
#'
#' Takes a string representation of a Library of Congress
#' call number and returns either TRUE or FALSE based on
#' whether or not the input fits the canonical LC Call
#' Number pattern
#'
#' @import data.table
#'
#' @param x A Library of Congress call number (string)
#'
#' @return Returns either TRUE or FALSE based on whether the
#'         call number is valid
#' @examples
#'
#' is_valid_lc_call("Q172.5.E77")
#' # TRUE
#' is_valid_lc_call("AF172.5.E77")
#' # FALSE
#
#' is_valid_lc_call(c("Q 172.5", "AF172", "PR6023.A93"))
#' # TRUE FALSE TRUE
#'
#' @export
is_valid_lc_call <- function(x){
  ncs <- thekey <- lc_two_letter_subject <- NULL

  data("lc_two_letter_subject", envir = environment())
  rg <- data.table::copy(lc_two_letter_subject)
  rg[, ncs:=nchar(thekey)]
  setorder(rg, -ncs)

  VALIDLCCALL <- sprintf("^(%s)\\s*[1-9]", paste(rg[, thekey], collapse="|"))

  stringr::str_detect(x, VALIDLCCALL)
}

#' Get the first letter of LC Call Number
#'
#' Takes a string representation of a Library of Congress
#' call number and returns the first letter if and only if
#' the LC Call Number is valid
#'
#' @import data.table
#'
#' @param x A Library of Congress call number (string)
#'
#' @return Returns first letter or NA if invalid
#' @examples
#'
#' get_lc_broad_letter("Q172.5.E77")
#' # Q
#' get_lc_broad_letter("AF172.5.E77")
#' # NA
#
#' get_lc_broad_letter(c("Q 172.5", "AF172", "PR6023.A93"))
#' # Q NA P
#'
#' @export
get_lc_broad_letter <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")

  thekey <- usersupplied <- NULL

  theinput <- data.table::data.table(usersupplied=x)

  THESEAREVALID <- theinput[, is_valid_lc_call(usersupplied)]
  theinput[THESEAREVALID, thekey:=stringr::str_sub(usersupplied, 1, 1)]
  theinput[!THESEAREVALID, thekey:=NA]
  return(theinput[, thekey])
}

#' Get all subject letters of LC Call Number
#'
#' Takes a string representation of a Library of Congress
#' call number and returns all the subject letters if and only if
#' the LC Call Number is valid
#'
#' @import data.table
#' @import utils
#'
#' @param x A Library of Congress call number (string)
#'
#' @return Returns all the subject letters or NA if invalid
#' @examples
#'
#' get_lc_subject_letters("Q172.5.E77")
#' # Q
#' get_lc_subject_letters("AF172.5.E77")
#' # NA
#
#' get_lc_subject_letters(c("Q 172.5", "AF172", "PR6023.A93"))
#' # Q NA PR
#'
#' @export
get_lc_subject_letters <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")

  description <- usersupplied <- thekey <- lc_first_letter_subject <- NULL

  theinput <- data.table::data.table(usersupplied=x)

  THESEAREVALID <- theinput[, is_valid_lc_call(usersupplied)]
  theinput[THESEAREVALID, thekey:=stringr::str_extract(usersupplied, "^[A-Z]+")]
  theinput[!THESEAREVALID, thekey:=NA]
  return(theinput[, thekey])
}

