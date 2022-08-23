

make.valid.lccall.regex <- function(allow.bare=FALSE){
  thekey <- lc_subject_subclassification <- NULL
  data("lc_subject_subclassification", envir = environment())
  tmp <- copy(lc_subject_subclassification)
  tmp <- tmp[order(-nchar(thekey))]
  if(allow.bare)
    VALIDLCCALL <- sprintf("^(%s)$", paste(tmp[, thekey], collapse="|"))
  else
    VALIDLCCALL <- sprintf("^(%s)\\s*[0-9]", paste(tmp[, thekey], collapse="|"))
  VALIDLCCALL
}

REGEX.VALID.LCCALL <- make.valid.lccall.regex()
REGEX.VALID.LCCALL.BARE <- make.valid.lccall.regex(allow.bare=TRUE)

##################################################################
###     Conversion from LC Calls to subject classification     ###
##################################################################

#' Conversion from Library of Congress Call number to subject
#' classification
#'
#' Takes a string representation of a Library of Congress
#' call number and returns either the broad subject
#' classification description (default) based on the first
#' letter, or a second-level subclassification
#' description based on the all the letters
#'
#' @import data.table
#'
#' @param x A Library of Congress call number (string)
#' @param subclassification A logical indicating whether the letters of
#'        call number past the first should be used to match to
#'        a subject subclassification (default is \code{FALSE})
#' @param already.parsed Skips the extraction of the subject letters
#'        and jumps to the subject matching (default is \code{FALSE})
#' @param allow.bare A logical indicating whether an LC Call with only
#'                   the letters should be considered valid
#'                   (default is \code{TRUE})
#'
#' @return Returns either the broad (top-level) subject classification
#'         description or the second level subject subclassification
#'         description. Returns "NA" if no subject could not be matched
#'         or call number is invalid
#' @examples
#'
#' get_lc_call_subject_classification("ND 237.S18 $b S87 1997")
#' # Fine Arts
#'
#' get_lc_call_subject_classification("ND 237.S18 $b S87 1997", subclassification=TRUE)
#' # Painting
#'
#' get_lc_call_subject_classification("PQ2246.M3")
#' # Language and Literature
#'
#' get_lc_call_subject_classification("PQ2246.M3",
#'                                    subclassification=TRUE)
#' # "French, Italian, Spanish, and Portuguese literature"
#'
#' get_lc_call_subject_classification("PQ2246.M3", already.parsed=TRUE)
#' # NA
#'
#' get_lc_call_subject_classification("PQ", already.parsed=TRUE,
#'                                    subclassification=TRUE)
#' # "French, Italian, Spanish, and Portuguese literature"
#'
#' # vectorized
#' get_lc_call_subject_classification(c("ND 237", "\\\\$a ND 2", "PQ2246.M3"),
#'                                    subclassification=TRUE)
#' # c("Painting", NA, "French, Italian, Spanish, and Portuguese literature")
#'
#'
#' @export
get_lc_call_subject_classification <- function(x, subclassification=FALSE,
                                               already.parsed=FALSE,
                                               allow.bare=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")

  x <- stringr::str_to_upper(x)
  x <- stringr::str_trim(x)

  thekey <- usersupplied <- description <- NULL
  lc_subject_classification <- lc_subject_subclassification <- NULL

  theinput <- data.table::data.table(usersupplied=x)

  if(already.parsed){
    theinput[, thekey:=usersupplied]
  } else{
    if(subclassification){
      theinput[, thekey:=get_all_lc_call_subject_letters(usersupplied, allow.bare=allow.bare)]
    } else{
      theinput[, thekey:=get_lc_call_first_letter(usersupplied, allow.bare=allow.bare)]
    }
  }

  data.table::setindex(theinput, thekey)

  if(subclassification){
    data("lc_subject_subclassification", envir = environment())
    result <- lc_subject_subclassification[theinput, on="thekey"]
  } else{
    data("lc_subject_classification", envir = environment())
    result <- lc_subject_classification[theinput, on="thekey"]
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
#' @param allow.bare A logical indicating whether an LC Call with only
#'                   the letters should be considered valid
#'                   (default is \code{FALSE})
#'
#' @return Returns either TRUE or FALSE based on whether the
#'         call number is valid
#' @examples
#'
#' is_valid_lc_call("Q172.5.E77")
#' # TRUE
#' is_valid_lc_call("AF172.5.E77")
#' # FALSE
#'
#' # vectorized
#' is_valid_lc_call(c("Q 172.5", "AF172", "PR6023.A93"))
#' # TRUE FALSE TRUE
#'
#' @export
is_valid_lc_call <- function(x, allow.bare=FALSE){
  if(!methods::is(x, "character"))
    stop("Input must be a character string")

  x <- stringr::str_to_upper(x)
  x <- stringr::str_trim(x)

  if(allow.bare)
    ret <- (stringr::str_detect(x, REGEX.VALID.LCCALL.BARE) |
              stringr::str_detect(x, REGEX.VALID.LCCALL))
  else
    ret <- stringr::str_detect(x, REGEX.VALID.LCCALL)

  return(ret)
}
attr(is_valid_lc_call, "assertr_vectorized") <- TRUE


#' Get the first letter of LC Call Number
#'
#' Takes a string representation of a Library of Congress
#' call number and returns the first letter if and only if
#' the LC Call Number is valid
#'
#' @import data.table
#'
#' @param x A Library of Congress call number (string)
#' @param allow.bare A logical indicating whether an LC Call with only
#'                   the letters should be considered valid
#'                   (default is \code{FALSE})
#'
#' @return Returns first letter or NA if invalid
#' @examples
#'
#' get_lc_call_first_letter("Q172.5.E77")
#' # Q
#' get_lc_call_first_letter("AF172.5.E77")
#' # NA
#'
#' # vectorized
#' get_lc_call_first_letter(c("Q 172.5", "AF172", "PR6023.A93"))
#' # Q NA P
#'
#' @export
get_lc_call_first_letter <- function(x, allow.bare=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")

  x <- stringr::str_to_upper(x)
  x <- stringr::str_trim(x)

  thekey <- usersupplied <- NULL

  theinput <- data.table::data.table(usersupplied=x)

  THESEAREVALID <- theinput[, is_valid_lc_call(usersupplied, allow.bare=allow.bare)]
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
#' @param allow.bare A logical indicating whether an LC Call with only
#'                   the letters should be considered valid
#'                   (default is \code{FALSE})
#'
#' @return Returns all the subject letters or NA if invalid
#' @examples
#'
#' get_all_lc_call_subject_letters("Q172.5.E77")
#' # Q
#' get_all_lc_call_subject_letters("AF172.5.E77")
#' # NA
#'
#' # vectorized
#' get_all_lc_call_subject_letters(c("Q 172.5", "AF172", "PR6023.A93"))
#' # Q NA PR
#'
#' @export
get_all_lc_call_subject_letters <- function(x, allow.bare=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")

  x <- stringr::str_to_upper(x)
  x <- stringr::str_trim(x)

  description <- usersupplied <- thekey <- lc_first_letter_subject <- NULL

  theinput <- data.table::data.table(usersupplied=x)

  THESEAREVALID <- theinput[, is_valid_lc_call(usersupplied, allow.bare=allow.bare)]
  theinput[THESEAREVALID, thekey:=stringr::str_extract(usersupplied, "^[A-Z]+")]
  theinput[!THESEAREVALID, thekey:=NA]
  return(theinput[, thekey])
}



#########################################################################
###     Conversion from Dewey Decimals to subject classifications     ###
#########################################################################

#' Conversion from Dewey Decimal call numbers to first-level subject description
#'
#' Takes a string representation of a Dewey Decimal
#' call number (DDC) and returns it's subject description.
#' This uses the hundreds place of the DDC number
#' and returns the most general subject classification.
#'
#' @import data.table
#'
#' @param x A Dewey Decimal call number
#'
#' @return Returns the most general subject classification using the
#'         hundreds places from the DDC. Returns NA if the DDC looks
#'         invalid
#'
#' @examples
#'
#' get_dewey_decimal_subject_class("709.05")     # Arts
#'
#' get_dewey_decimal_subject_class("823.912")
#' # Literature (Belles-lettres) and rhetoric
#'
#' # vectorized
#' get_dewey_decimal_subject_class(c("709.05", "invalid", NA, "823.912"))
#' # c("Arts", NA, NA, "Literature (Belles-lettres) and rhetoric")
#'
#' @export
get_dewey_decimal_subject_class <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")

  x <- stringr::str_trim(x)

  where.bad <- !stringr::str_detect(x, "^\\d{3}")
  x[where.bad] <- NA_character_

  x <- stringr::str_pad(stringr::str_sub(x , 1, 1), width=3, pad="0", side="right")

  thekey <- description <- dewey_subject_crosswalk <- NULL

  theinput <- data.table::data.table(thekey=x)
  data.table::setindex(theinput, thekey)

  data("dewey_subject_crosswalk", envir = environment())

  result <- dewey_subject_crosswalk[theinput, on="thekey"]

  return(result[, description])
}


#' Conversion from Dewey Decimal call numbers to second-level subject description
#'
#' Takes a string representation of a Dewey Decimal
#' call number (DDC) and returns it's subject description.
#' This uses the first two digits of the DDC number
#' and returns the second most general subject classification.
#'
#' @import data.table
#'
#' @param x A Dewey Decimal call number
#'
#' @return Returns the most general subject classification using the
#'         first two digits from the DDC. Returns NA if the DDC looks
#'         invalid
#'
#' @examples
#'
#' get_dewey_decimal_subject_division("709.05")     # Arts
#'
#' get_dewey_decimal_subject_division("823.912")
#' # "English and Old English literatures"
#'
#' # vectorized
#' get_dewey_decimal_subject_division(c("709.05", "invalid", NA, "823.912"))
#' # c("Arts", NA, NA, "English and Old English literatures")
#'
#' @export
get_dewey_decimal_subject_division <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")

  x <- stringr::str_trim(x)

  where.bad <- !stringr::str_detect(x, "^\\d{3}")
  x[where.bad] <- NA_character_

  x <- stringr::str_pad(stringr::str_sub(x , 1, 2), width=3, pad="0", side="right")

  thekey <- description <- dewey_subject_crosswalk <- NULL

  theinput <- data.table::data.table(thekey=x)
  data.table::setindex(theinput, thekey)

  data("dewey_subject_crosswalk", envir = environment())

  result <- dewey_subject_crosswalk[theinput, on="thekey"]

  return(result[, description])
}


#' Conversion from Dewey Decimal call numbers to third-level subject description
#'
#' Takes a string representation of a Dewey Decimal
#' call number (DDC) and returns it's subject description.
#' This uses the first three digits of the DDC number
#' and returns the third most general subject classification.
#'
#' @import data.table
#'
#' @param x A Dewey Decimal call number
#'
#' @return Returns the most general subject sectionification using the
#'         first three digits from the DDC. Returns NA if the DDC looks
#'         invalid
#'
#' @examples
#'
#' get_dewey_decimal_subject_section("709.05")
#' # "History, geographic treatment, biography"
#'
#' get_dewey_decimal_subject_section("823.912")
#' # "English fiction"
#'
#' # vectorized
#' get_dewey_decimal_subject_section(c("709.05", "invalid", NA, "823.912"))
#' # c("History, geographic treatment, biography", NA, NA,
#' #   "English fiction")
#'
#' @export
get_dewey_decimal_subject_section <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(!methods::is(x, "character"))
    stop("Input must be a character string")

  x <- stringr::str_trim(x)

  where.bad <- !stringr::str_detect(x, "^\\d{3}")
  x[where.bad] <- NA_character_

  x <- stringr::str_sub(x , 1, 3)

  thekey <- description <- dewey_subject_crosswalk <- NULL

  theinput <- data.table::data.table(thekey=x)
  data.table::setindex(theinput, thekey)

  data("dewey_subject_crosswalk", envir = environment())

  result <- dewey_subject_crosswalk[theinput, on="thekey"]

  return(result[, description])
}


