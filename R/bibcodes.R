

REGEX.ISBN.10.9 <- "^(\\d{9,10}|\\d{9}[xX])$"
REGEX.ISBN.10.flex <- "^\\d{9}(x|X|\\d)$"
REGEX.ISBN.10 <- "^\\d{9}(X|\\d)$"

REGEX.ISBN.13.12 <- "^\\d{12,13}$"
REGEX.ISBN.13 <- "^\\d{13}$"

REGEX.ISSN.8.7 <- "^(\\d{7,8}|\\d{7}[xX])$"
REGEX.ISSN.flex <- "^\\d{7}(x|X|\\d)$"
REGEX.ISSN <- "^\\d{7}(X|\\d)$"


##############################################
###               ISBN 10                  ###
##############################################

#' Get ISBN 10 check digit
#'
#' Takes a string representation of an ISBN 10
#' and returns the check digit that satisfies the necessary condition.
#' It can take a 10 digit string (and ignore the already extant check digit)
#' or a 9 digit string (without the last digit)
#'
#' @param x A string of 9 or 10 digits
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#' @param errors.as.nas return NA if error instead of throwing error
#'
#' @return Returns the character check digit that satifies the
#'         mod 11 condition. Returns "X" if 10. Returns NA if input is NA
#' @examples
#'
#' get_isbn_10_check_digit("012491540X")
#' get_isbn_10_check_digit("0-124-91540-X", allow.hyphens=TRUE)
#'
#' # nine digit string
#' get_isbn_10_check_digit("900403781")
#'
#' get_isbn_10_check_digit("onetwothre", errors.as.nas=TRUE)  # NA
#'
#' # vectorized
#' get_isbn_10_check_digit(c("012491540X", "9004037810", "900403781"))
#'
#' @export
get_isbn_10_check_digit <- function(x, allow.hyphens=FALSE, errors.as.nas=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")
  if(allow.hyphens)
    x <- gsub("-", "", x)
  if(sum(!(nchar(x[!is.na(x)]) %in% c(9, 10)))>0){
    if(!errors.as.nas) stop("Input must be either 9 or 10 characters")
  }
  where.bad <- !grepl(REGEX.ISBN.10.9, x, perl=TRUE) & !is.na(x)
  if(sum(where.bad)>0){
    if(!errors.as.nas) stop("Illegal input")
    x[where.bad] <- NA
  }
  first9 <- lapply(strsplit(substr(x, 1, 9), ""), as.numeric)
  rem <- unlist(lapply(lapply(first9, function(x) x*(10:2)), sum))
  should.be <- (11 - (rem %% 11)) %% 11
  ifelse(should.be==10, "X", as.character(should.be))
}


#' Check the check digit of an ISBN 10
#'
#' Takes a string representation of an ISBN 10 and verifies that check digit
#' checks out
#'
#' @param x A string of 10 digits or 9 digits with terminal "X"
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#' @param errors.as.false return false if error instead of throwing error
#'
#' @return Returns TRUE if check passes, FALSE if not, and NA if NA
#' @examples
#'
#' check_isbn_10_check_digit("012491540X")      # TRUE
#' check_isbn_10_check_digit("0-124-91540-X")   # TRUE
#'
#' # vectorized
#' check_isbn_10_check_digit(c("012491540X", "9004037812"))  # TRUE FALSE
#'
#' @export
check_isbn_10_check_digit <- function(x, allow.hyphens=TRUE, errors.as.false=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character"){
    if(errors.as.false)
      return(rep(FALSE, length(x)))
    stop("Input must be a character string")
  }
  x <- toupper(x)
  if(allow.hyphens)
    x <- gsub("-", "", x)
  where.bad <- (!grepl(REGEX.ISBN.10, x, perl=TRUE) & !is.na(x))
  if(sum(where.bad)>0){
    if(!errors.as.false) stop("Illegal input")
  }
  check.digit <- substr(x, 10, 10)
  should.be <- get_isbn_10_check_digit(x, errors.as.nas = errors.as.false)
  ret <- ifelse(should.be==toupper(check.digit), TRUE, FALSE)
  ret[where.bad] <- FALSE
  return(ret)
}


#' Return TRUE if valid ISBN 10
#'
#' Takes a string representation of an ISBN 10 verifies that it is valid.
#' An ISBN 10 is valid if it is a 10 digit string or a 9 digit string
#' with a terminal "X" AND the check digit matches
#'
#' @param x A string of 10 digits or 9 digits with terminal "X"
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#' @param lower.x.allowed A logical indicating whether ISBN 10s with
#'                        a check digit with a lower-case "x" should
#'                        be treated as valid
#'
#' @return Returns TRUE if checks pass, FALSE if not, and NA if NA
#' @examples
#'
#' is_valid_isbn_10("012491540X")    # TRUE
#' is_valid_isbn_10("0-124-91540-X") # TRUE
#'
#' # vectorized
#' is_valid_isbn_10(c("012491540X", "9004037812"))      # TRUE FALSE
#' is_valid_isbn_10(c("012491540X", "hubo un tiempo"))  # TRUE FALSE
#'
#' @export
is_valid_isbn_10 <- function(x, allow.hyphens=TRUE, lower.x.allowed=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character"){
    stop("Input must be a character string")
  }
  if(allow.hyphens)
    x <- gsub("-", "", x)
  CHECKREGEX <- REGEX.ISBN.10
  if(lower.x.allowed)
    CHECKREGEX <- REGEX.ISBN.10.flex
  where.bad <- !grepl(CHECKREGEX, x, perl=TRUE) & !is.na(x)
  x[where.bad] <- NA
  ret <- ifelse(check_isbn_10_check_digit(x, errors.as.false=TRUE), TRUE, FALSE)
  ret[is.na(x)] <- NA
  ret[where.bad] <- FALSE
  return(ret)
}
attr(is_valid_isbn_10, "assertr_vectorized") <- TRUE



#' Attempt to enforce validity and canonical form to ISBN 10
#'
#' Takes a string representation of an ISBN 10. Strips all non-digit
#' or "X" characters and checks if it is valid (whether the
#' check digit works out, etc). User can specify whether "aggresive"
#' measures should be taken to salvage the malformed ISBN 10 string.
#'
#' @param x A string
#' @param aggresive A logical indicating whether aggresive measures
#'                      should be taken to try to get the "ISBN 10"
#'                      into a valid form. See "Details" for more info
#' @param convert.to.isbn.13 A logical indication whether the ISBN 10
#'                           should be converted into an ISBN 13
#' @param pretty A logical indicating whether the ISBN should be
#'               prettily hyphenated
#'
#' @details If \code{aggresive} is TRUE, aggresive measures are taken to
#' try to salvage the malformed ISBN 10 string. If the ISBN 10, for example,
#' is 9 digits, and either adding an "X" to the end, or leading "0"s fix it,
#' this function will return the salvaged ISBN 10. If the ISBN 10 has
#' garbage digits/characters in the front and has an "X" check digit,
#' it will return the salvaged ISBN 10.
#' #### MORE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#'
#' @return Returns TRUE if checks pass, FALSE if not, and NA if NA
#' @examples
#'
#' normalize_isbn_10("012491540x")                    # "012491540X"
#' normalize_isbn_10("012491540x xe32ea")             # "012491540X"
#' normalize_isbn_10("012491540x", pretty=TRUE)       # "0-124-91540-X"
#' normalize_isbn_10("012491540x", convert.to.isbn.13=TRUE)
#' # "9780124915404"
#' normalize_isbn_10("012491540x", convert.to.isbn.13=TRUE, pretty=TRUE)
#' # "978-0-12-491540-4"
#' normalize_isbn_10("513213012491540x")              # "012491540X"
#'
#' @export
normalize_isbn_10 <- function(x, aggresive=TRUE, convert.to.isbn.13=FALSE, pretty=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    x <- as.character(x)
  x <- toupper(x)
  x <- gsub("[^\\d|X]", "", x, perl=TRUE)
  x <- gsub("X(.+$)", "\\1", x, perl=TRUE)
  is.all.valid <- all(is_valid_isbn_10(x))
  if(aggresive && !is.all.valid){
    will_padding_zeros_fix_it <- function(x){
      ifelse(nchar(x)==9 & is_valid_isbn_10(stringr::str_pad(x, 10, "left", "0")), TRUE, FALSE)
    }
    will_adding_an_X_fix_it <- function(x){
      ifelse(nchar(x)==9 & get_isbn_10_check_digit(x)=="X", TRUE, FALSE)
    }
    will_the_first_10_do <- function(x){
      ifelse(nchar(x)>10 & is_valid_isbn_10(substr(x, 1, 10)), TRUE, FALSE)
    }
    will_the_hiddens_do <- function(x){
      ifelse(nchar(x)>10 & is_valid_isbn_10(gsub("^.*?(\\d{9}X).*$", "\\1",
                                                 x, perl=TRUE)), TRUE, FALSE)
    }
    thenines <- x[nchar(x)==9 & !is.na(x)]
    if(length(thenines)>0){
      x[nchar(x)==9 & !is.na(x)] <- ifelse(will_padding_zeros_fix_it(thenines),
                                           stringr::str_pad(thenines, 10, "left", "0"),
                                           thenines)
    }
    thenines <- x[nchar(x)==9 & !is.na(x)]
    if(length(thenines)>0){
      x[nchar(x)==9 & !is.na(x)] <- ifelse(will_adding_an_X_fix_it(thenines),
                                           sprintf("%sX", thenines),
                                           thenines)
    }
    thebig <- x[nchar(x)>10 & !is.na(x)]
    if(length(thebig)){
      x[nchar(x)>10 & !is.na(x)] <- ifelse(will_the_first_10_do(thebig),
                                           substr(thebig, 1, 10),
                                           thebig)
    }
    thehiddens <- x[grepl("\\d{9}X", x) & !is.na(x)]
    if(length(thehiddens)){
      x[grepl("\\d{9}X", x) & !is.na(x)] <- ifelse(will_the_hiddens_do(thehiddens),
                                                   gsub("^.*?(\\d{9}X).*$", "\\1",
                                                        x, perl=TRUE),
                                                   thehiddens)
    }
  }
  # maybe shouldn't return NA if couldn't be salvaged?
  ret <- ifelse(is_valid_isbn_10(x), x, NA)
  if(convert.to.isbn.13)
    return(convert_to_isbn_13(ret, pretty=pretty))
  if(pretty){
    nonnas <- !is.na(ret)
    these <- ret[nonnas]
    ret[nonnas] <- sprintf("%s-%s-%s-%s", substr(these, 1, 1),
                           substr(these, 2, 4), substr(these, 5, 9),
                           substr(these, 10, 10))
  }
  return(ret)
}


# also add functionality to find an "X" and look 9 digits behind

# ------------------------------------------ #


##############################################
###               ISBN 13                  ###
##############################################

#' Get ISBN 13 check digit
#'
#' Takes a string representation of an ISBN 13
#' and returns the check digit that satisfies the necessary condition.
#' It can take a 13 digit string (and ignore the already extant check digit)
#' or a 12 digit string (without the last digit)
#'
#' @param x A string of 12 or 13 digits
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#' @param errors.as.nas return NA if error instead of throwing error
#'
#' @return Returns the character check digit that satifies the
#'         mod 10 condition. Returns NA if input is NA
#' @examples
#'
#' get_isbn_13_check_digit("9780306406157")
#'
#' # 12 digit string
#' get_isbn_13_check_digit("978030640615")
#'
#' get_isbn_13_check_digit("onetwothreefo", errors.as.nas=TRUE)  # NA
#'
#' # vectorized
#' get_isbn_13_check_digit(c("9780306406157", "9783161484100"))
#'
#' @export
get_isbn_13_check_digit <- function(x, allow.hyphens=FALSE, errors.as.nas=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")
  if(allow.hyphens)
    x <- gsub("-", "", x)
  if(sum(!(nchar(x[!is.na(x)]) %in% c(12, 13)))>0){
    if(!errors.as.nas) stop("Input must be either 12 or 13 characters")
  }
  where.bad <- !grepl(REGEX.ISBN.13.12, x, perl=TRUE) & !is.na(x)
  if(sum(where.bad)>0){
    if(!errors.as.nas) stop("Illegal input")
    x[where.bad] <- NA
  }
  first12 <- lapply(strsplit(substr(x, 1, 12), ""), as.numeric)
  rem <- unlist(lapply(lapply(first12, function(x) x*(rep(c(1,3), 6))), sum))
  should.be <- (10 - (rem %% 10)) %% 10
  return(as.character(should.be))
}



#' Check the check digit of an ISBN 13
#'
#' Takes a string representation of an ISBN 13 and verifies that check digit
#' checks out
#'
#' @param x A string of 13 digits
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#' @param errors.as.false return false if error instead of throwing error
#'
#' @return Returns TRUE if check passes, FALSE if not, and NA if NA
#' @examples
#'
#' check_isbn_13_check_digit("9780306406157")          # TRUE
#' check_isbn_13_check_digit("978-0-306-40615-7")      # TRUE
#'
#' # vectorized
#' check_isbn_13_check_digit(c("978-0-306-40615-7", "9783161484103"))  # TRUE FALSE
#'
#' @export
check_isbn_13_check_digit <- function(x, allow.hyphens=TRUE, errors.as.false=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character"){
    if(errors.as.false)
      return(rep(FALSE, length(x)))
    stop("Input must be a character string")
  }
  if(allow.hyphens)
    x <- gsub("-", "", x)
  where.bad <- (!grepl(REGEX.ISBN.13, x, perl=TRUE) & !is.na(x))
  if(sum(where.bad)>0){
    if(!errors.as.false) stop("Illegal input")
  }
  check.digit <- substr(x, 13, 13)
  should.be <- get_isbn_13_check_digit(x, errors.as.nas = errors.as.false)
  ret <- ifelse(should.be==check.digit, TRUE, FALSE)
  ret[where.bad] <- FALSE
  return(ret)
}

#' Return TRUE if valid ISBN 13
#'
#' Takes a string representation of an ISBN 13 verifies that it is valid.
#' An ISBN 13 is valid if it is a 13 digit string and the check digit matches
#'
#' @param x A string of 13
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#'
#' @return Returns TRUE if checks pass, FALSE if not, and NA if NA
#' @examples
#'
#' is_valid_isbn_13("9780306406157")          # TRUE
#' is_valid_isbn_13("978-0-306-40615-7")      # TRUE
#'
#' # vectorized
#' is_valid_isbn_10(c("012491540X", "9004037812"))  # TRUE FALSE
#' is_valid_isbn_13(c("978-0-306-40615-7", "9783161484103"))  # TRUE FALSE
#' is_valid_isbn_13(c("978-0-306-40615-7", "hubo un tiempo"))  # TRUE FALSE
#'
#' @export
is_valid_isbn_13 <- function(x, allow.hyphens=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character"){
    stop("Input must be a character string")
  }
  if(allow.hyphens)
    x <- gsub("-", "", x)
  where.bad <- !grepl(REGEX.ISBN.13, x, perl=TRUE) & !is.na(x)
  x[where.bad] <- NA
  ret <- ifelse(check_isbn_13_check_digit(x, errors.as.false=TRUE), TRUE, FALSE)
  ret[is.na(x)] <- NA
  ret[where.bad] <- FALSE
  return(ret)
}
attr(is_valid_isbn_13, "assertr_vectorized") <- TRUE


#' Convert ISBN 10 to ISBN 13
#'
#' Takes a string representation of an ISBN 10 and converts it to an ISBN 13.
#'
#' @param x A string of 10 digits or 9 digits with terminal "X"
#' @param skip.validity.check Skip the checking for whether the ISBN 10 is valid
#' @param errors.as.nas return NA if error instead of throwing error
#' @param pretty A logical indicating whether the ISBN 13 should be
#'               prettily hyphenated
#'
#' @return Returns ISBN 13 as a string
#' @examples
#'
#' convert_to_isbn_13("012491540X")                # 9780124915404
#' convert_to_isbn_13("012491540X", pretty=TRUE)   # 978-0-12-491540-4
#'
#' # vectorized
#' convert_to_isbn_13(c("012491540X", "9004037810"), pretty=TRUE)
#' # "978-0-12-491540-4" "978-9-00-403781-6"
#'
#' @export
convert_to_isbn_13 <- function(x, skip.validity.check=FALSE,
                               errors.as.nas=FALSE, pretty=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character"){
    stop("Input must be a character string")
  }
  x <- toupper(x)
  x <- gsub("[^\\d|X]", "", x, perl=TRUE)
  x <- gsub("X(.+$)", "\\1", x, perl=TRUE)
  if(!skip.validity.check){
    where.bad <- !is_valid_isbn_10(x) & !is.na(x)
    if(any(where.bad) & !errors.as.nas) stop("Invalid ISBN 10 detected")
    x[where.bad] <- NA
  }
  first9 <- substr(x, 1, 9)
  first12 <- ifelse(!is.na(first9), sprintf("978%s", first9), NA)
  newcheckdigit <- get_isbn_13_check_digit(as.character(first12))
  newisbn13 <- ifelse(!is.na(first12), sprintf("%s%s", first12, newcheckdigit), NA)
  if(pretty){
    nonnas <- !is.na(newisbn13)
    these <- newisbn13[nonnas]
    newisbn13[nonnas] <- sprintf("%s-%s-%s-%s-%s", substr(these, 1, 3),
                                 substr(these, 4, 4), substr(these, 5, 6),
                                 substr(these, 7, 12), substr(these, 13, 13))
  }
  return(newisbn13)
}

# make sure that in normalize isbn 10 (if convert is TRUE)
# that the validity check is skipped, and that
# pretty is propagated



###### NO NORMALIZE ISBN 13 YET!!!









# ------------------------------------------ #



##############################################
###                 ISSN                   ###
##############################################


#' Get ISSN check digit
#'
#' Takes a string representation of an ISSN
#' and returns the check digit that satisfies the necessary condition.
#' It can take a 8 digit string (and ignore the already extant check digit)
#' or a 7 digit string (without the last digit)
#'
#' @param x A string of 7 or 8 digits
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#' @param errors.as.nas return NA if error instead of throwing error
#'
#' @return Returns the character check digit that satifies the
#'         mod 11 condition. Returns "X" if 10. Returns NA if input is NA
#' @examples
#'
#' get_issn_check_digit("03785955")
#'
#' get_issn_check_digit("2434-561X", allow.hyphens=TRUE)
#'
#' # nine digit string
#' get_issn_check_digit("0378595")
#'
#' # vectorized
#' get_issn_check_digit(c("0378595", "2434561X", NA))
#'
#' @export
get_issn_check_digit <- function(x, allow.hyphens=FALSE, errors.as.nas=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")
  if(allow.hyphens)
    x <- gsub("-", "", x)
  if(sum(!(nchar(x[!is.na(x)]) %in% c(7, 8)))>0){
    if(!errors.as.nas) stop("Input must be either 7 or 8 characters")
  }
  where.bad <- !grepl(REGEX.ISSN.8.7, x, perl=TRUE) & !is.na(x)
  if(sum(where.bad)>0){
    if(!errors.as.nas) stop("Illegal input")
    x[where.bad] <- NA
  }
  first7 <- lapply(strsplit(substr(x, 1, 7), ""), as.numeric)
  rem <- unlist(lapply(lapply(first7, function(x) x*(8:2)), sum))
  should.be <- (11 - (rem %% 11)) %% 11
  ifelse(should.be==10, "X", as.character(should.be))
}


#' Check the check digit of an ISSN
#'
#' Takes a string representation of an ISSN and verifies that check digit
#' checks out
#'
#' @param x A string of 8 digits or 7 digits with terminal "X"
#' @param allow.hyphens A logical indicating whether the hyphen
#'     separator should be allowed
#' @param errors.as.false return false if error instead of throwing error
#'
#' @return Returns TRUE if check passes, FALSE if not, and NA if NA
#' @examples
#'
#' check_issn_check_digit("2434561X")   # TRUE
#' check_issn_check_digit("2434-561X")  # TRUE
#'
#' # vectorized
#' check_issn_check_digit(c("03785955", "2434561X", NA))  # TRUE TRUE NA
#' check_issn_check_digit(c("0378-5955", "2434-561X", NA))
#' # TRUE TRUE NA
#'
#' @export
check_issn_check_digit <- function(x, allow.hyphens=TRUE, errors.as.false=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character"){
    if(errors.as.false)
      return(rep(FALSE, length(x)))
    stop("Input must be a character string")
  }
  x <- toupper(x)
  if(allow.hyphens)
    x <- gsub("-", "", x)
  where.bad <- (!grepl(REGEX.ISSN, x, perl=TRUE) & !is.na(x))
  if(sum(where.bad)>0){
    if(!errors.as.false) stop("Illegal input")
  }
  check.digit <- substr(x, 8, 8)
  should.be <- get_issn_check_digit(x, allow.hyphens=allow.hyphens, errors.as.nas=errors.as.false)
  ret <- ifelse(should.be==toupper(check.digit), TRUE, FALSE)
  ret[where.bad] <- FALSE
  return(ret)
}

# is valid ISSN (assertr vectorize it)
# normalize ISSN

