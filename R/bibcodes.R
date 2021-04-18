

# regular expression patterns
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
#'                      separator should be allowed
#'                      (default is \code{FALSE})
#' @param errors.as.nas return NA if error instead of throwing error
#'                      (default is \code{FALSE})
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
    x <- gsub("-", "", x, fixed=TRUE)
  if(any(!(nchar(x[!is.na(x)]) %in% c(9, 10))>0))
    if(!errors.as.nas) stop("Input must be either 9 or 10 characters")
  where.bad <- !grepl(REGEX.ISBN.10.9, x, perl=TRUE) & !is.na(x)
  if(any(where.bad)){
    if(!errors.as.nas) stop("Illegal input")
    x[where.bad] <- NA
  }
  if(any(!where.bad)){
    first9 <- stringr::str_split(substr(x[!where.bad], 1, 9), "", simplify=TRUE)
    class(first9) <- "numeric"
    first9 <- as.numeric(first9 %*% matrix(10:2))
    should.be <- (11 - (first9 %% 11)) %% 11
    ret <- ifelse(should.be==10, "X", as.character(should.be))
    x[!where.bad] <- ret
  }
  x
}


#' Check the check digit of an ISBN 10
#'
#' Takes a string representation of an ISBN 10 and verifies that check digit
#' checks out
#'
#' @param x A string of 10 digits or 9 digits with terminal "X"
#' @param allow.hyphens A logical indicating whether the hyphen
#'                      separator should be allowed
#'                      (default is \code{FALSE})
#' @param errors.as.false return false if error instead of throwing error
#'                        (default is \code{TRUE})
#'
#' @return Returns TRUE if check passes, FALSE if not, and NA if NA
#'
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
    x <- gsub("-", "", x, fixed=TRUE)
  where.bad <- !grepl(REGEX.ISBN.10, x, perl=TRUE) & !is.na(x)
  if(any(where.bad)){
    if(!errors.as.false) stop("Illegal input")
  }
  check.digit <- stringr::str_sub(x, -1)
  should.be <- get_isbn_10_check_digit(x, errors.as.nas = errors.as.false)
  ret <- ifelse(should.be==check.digit, TRUE, FALSE)
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
#'                      separator should be allowed
#'                      (default is \code{TRUE})
#' @param lower.x.allowed A logical indicating whether ISBN 10s with
#'                        a check digit with a lower-case "x" should
#'                        be treated as valid
#'                        (default is \code{TRUE})
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
    x <- gsub("-", "", x, fixed=TRUE)
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
#' and non-"X" characters and checks if it is valid (whether the
#' check digit works out, etc). User can specify whether "aggressive"
#' measures should be taken to salvage the malformed ISBN 10 string.
#'
#' @param x A string
#' @param aggressive A logical indicating whether aggressive measures
#'                   should be taken to try to get the "ISBN 10"
#'                   into a valid form. See "Details" for more info
#'                   (default is \code{TRUE})
#' @param convert.to.isbn.13 A logical indicating whether the ISBN 10
#'                           should be converted into an ISBN 13
#'                           (default is \code{FALSE})
#'
#' @details If \code{aggressive} is TRUE, aggressive measures are taken to
#' try to salvage the malformed ISBN 10 string. If the ISBN 10, for example,
#' is 9 digits, and either adding an "X" to the end, or leading "0"s fix it,
#' this function will return the salvaged ISBN 10. If the ISBN 10 has
#' garbage digits/characters in the front and has an "X" check digit,
#' it will return the salvaged ISBN 10.
#'
#' @return Returns valid ISBN 10 if possible, NA if not
#' @examples
#'
#' normalize_isbn_10("012491540x")                    # "012491540X"
#' normalize_isbn_10("012491540x xe32ea")             # "012491540X"
#' normalize_isbn_10("012491540x", convert.to.isbn.13=TRUE)
#' # "9780124915404"
#' normalize_isbn_10("513213012491540x")              # "012491540X"
#'
#' @seealso \code{\link{normalize_isbn}} \code{\link{normalize_isbn_13}}
#'
#' @export
normalize_isbn_10 <- function(x, aggressive=TRUE, convert.to.isbn.13=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    x <- as.character(x)
  x <- toupper(x)
  x <- gsub("[^\\d|X]", "", x, perl=TRUE)
  y <- x
  x <- gsub("X(.+$)", "\\1", x, perl=TRUE)
  is.all.valid <- all(is_valid_isbn_10(x), na.rm=TRUE)
  if(aggressive && !is.all.valid){
    will_padding_zeros_fix_it <- function(x){
      nchar(x)==9 & is_valid_isbn_10(stringr::str_pad(x, 10, "left", "0"), lower.x.allowed=FALSE)
    }
    will_adding_an_X_fix_it <- function(x){
      nchar(x)==9 & get_isbn_10_check_digit(x, errors.as.nas=TRUE)=="X"
    }
    will_the_first_10_do <- function(x){
      nchar(x)>10 & is_valid_isbn_10(substr(x, 1, 10), lower.x.allowed=FALSE)
    }
    will_the_hiddens_do <- function(x){
      nchar(x)>10 & is_valid_isbn_10(gsub("^.*?(\\d{9}X).*$", "\\1", x, perl=TRUE),
                                     lower.x.allowed=FALSE)
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
    loghidden <- grepl("\\d{9}X", y, perl=TRUE) & !is.na(x)
    if(any(loghidden)){
      loghidden[loghidden] <- will_the_hiddens_do(y[loghidden])
      thehiddens <- y[loghidden]
      x[loghidden] <- gsub("^.*?(\\d{9}X).*$", "\\1", thehiddens, perl=TRUE)
    }

  }
  # maybe shouldn't return NA if couldn't be salvaged?
  ret <- ifelse(is_valid_isbn_10(x), x, NA)
  if(convert.to.isbn.13)
    return(convert_to_isbn_13(ret, skip.validity.check=TRUE))
  return(ret)
}

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
#'                      separator should be allowed
#'                      (default is \code{FALSE})
#' @param errors.as.nas return NA if error instead of throwing error
#'                      (default is \code{FALSE})
#'
#' @return Returns the character check digit that satifies the
#'         mod 10 condition. Returns NA if input is NA
#'
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
    x <- gsub("-", "", x, fixed=TRUE)
  if(any(!(nchar(x[!is.na(x)]) %in% c(12, 13))>0))
    if(!errors.as.nas) stop("Input must be either 12 or 13 characters")
  where.bad <- !grepl(REGEX.ISBN.13.12, x, perl=TRUE) & !is.na(x)
  if(any(where.bad)){
    if(!errors.as.nas) stop("Illegal input")
    x[where.bad] <- NA
  }
  if(any(!where.bad)){
    first12 <- stringr::str_split(substr(x[!where.bad], 1, 12), "", simplify=TRUE)
    class(first12) <- "numeric"
    first12 <- as.numeric(first12 %*% matrix(rep(c(1,3), 6)))
    should.be <- (10 - (first12 %% 10)) %% 10
    x[!where.bad] <- as.character(should.be)
  }
  return(x)
}



#' Check the check digit of an ISBN 13
#'
#' Takes a string representation of an ISBN 13 and verifies that check digit
#' checks out
#'
#' @param x A string of 13 digits
#' @param allow.hyphens A logical indicating whether the hyphen
#'                      separator should be allowed
#'                      (default is \code{TRUE})
#' @param errors.as.false return false if error instead of throwing error
#'                        (default is \code{TRUE})
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
    x <- gsub("-", "", x, fixed=TRUE)
  where.bad <- !grepl(REGEX.ISBN.13, x, perl=TRUE) & !is.na(x)
  if(any(where.bad)){
    if(!errors.as.false) stop("Illegal input")
  }
  check.digit <- stringr::str_sub(x, -1)
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
#'                      separator should be allowed
#'                      (default is \code{TRUE})
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
    x <- gsub("-", "", x, fixed=TRUE)
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
#'                            (default is \code{FALSE})
#' @param errors.as.nas return NA if error instead of throwing error
#'                      (default is \code{FALSE})
#'
#' @return Returns ISBN 13 as a string
#'
#' @examples
#'
#' convert_to_isbn_13("012491540X")                # 9780124915404
#'
#' # vectorized
#' convert_to_isbn_13(c("012491540X", "9004037810"))
#' # "9780124915404" "9789004037816"
#'
#' @export
convert_to_isbn_13 <- function(x, skip.validity.check=FALSE,
                               errors.as.nas=FALSE){
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
  return(newisbn13)
}



#' Attempt to enforce validity and canonical form to ISBN 13
#'
#' Takes a string representation of an ISBN 13. Strips all non-digit
#' characters and checks if it is valid (whether the
#' check digit works out, etc). User can specify whether "aggressive"
#' measures should be taken to salvage the malformed ISBN 13 string.
#'
#' @param x A string
#' @param aggressive A logical indicating whether aggressive measures
#'                   should be taken to try to get the "ISBN 13"
#'                   into a valid form. See "Details" for more info
#'                   (default is \code{TRUE})
#'
#' @details If \code{aggressive} is TRUE, aggressive measures are taken to
#' try to salvage the malformed ISBN 13 string. If the ISBN 13, for example,
#' is more than 13 characters, this function will attempt to make a valid
#' ISBN 13 from the first 13 digits.
#'
#' @return Returns valid ISBN 13 if possible, NA if not
#' @examples
#'
#' normalize_isbn_13("978966819^*!X7918")        # "9789668197918"
#'
#' # vectorized
#' normalize_isbn_13(c("978-9-66-819791-8", "__9__781572411579"))
#' # "9789668197918" "9781572411579"
#'
#' @seealso \code{\link{normalize_isbn}} \code{\link{normalize_isbn_10}}
#'
#' @export
normalize_isbn_13 <- function(x, aggressive=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    x <- as.character(x)
  x <- gsub("\\D", "", x, perl=TRUE)
  is.all.valid <- all(is_valid_isbn_13(x), na.rm=TRUE)
  if(aggressive && !is.all.valid){
    will_the_first_13_do <- function(x){
      nchar(x)>13 & is_valid_isbn_13(substr(x, 1, 13))
    }
    wherebig <- nchar(x)>13 & !is.na(x)
    thebig <- x[wherebig]
    if(length(thebig)){
      x[wherebig] <- ifelse(will_the_first_13_do(thebig),
                                           substr(thebig, 1, 13),
                                           thebig)
    }
  }
  # maybe shouldn't return NA if couldn't be salvaged?
  ret <- ifelse(is_valid_isbn_13(x), x, NA)
  return(ret)
}


# ------------------------------------------ #


#' Attempt to enforce validity and canonical form to an ISBN
#'
#' Takes a string representation of an ISBN (10 or 13). This function uses
#' tries to normalize the string as a ISBN 13, then an ISBN 10. If one of
#' those methods are able to salvage the ISBN, the canonicalized ISBN is
#' returned. User can specify whether "aggressive"
#' measures should be taken to salvage the malformed ISBN string.
#'
#' @param x A string
#' @param aggressive A logical indicating whether aggressive measures
#'                      should be taken to try to get the "ISBN 10"
#'                      into a valid form. See "Details" for more info
#'                      (default is \code{TRUE})
#' @param convert.to.isbn.13 A logical indicating whether the ISBN 10
#'                           should be converted into an ISBN 13
#'                           (default is \code{FALSE})
#'
#' @details If \code{aggressive} is TRUE, aggressive measures are taken to
#' try to salvage the malformed ISBN string. Since this function attempts
#' to salvage both an ISBN 10 and 13, to learn about examples of the
#' aggressive methods, see \code{\link{normalize_isbn_10}} and
#' \code{\link{normalize_isbn_13}}
#'
#' @return Returns valid ISBN if possible, NA if not
#' @examples
#'
#' normalize_isbn("012491540x")                           # "012491540X"
#' normalize_isbn("012491540x", convert.to.isbn.13=TRUE)
#' "9780124915404"
#'
#' # vectorized
#' normalize_isbn(c("513213012491540x245",
#'                  "978966819^*!X7918",
#'                  NA,
#'                  "97815724115799781572411579"))
#' # "012491540X", "9789668197918", NA, "9781572411579"
#'
#' @seealso \code{\link{normalize_isbn_10}} \code{\link{normalize_isbn_13}}
#'
#' @export
normalize_isbn <- function(x, aggressive=TRUE, convert.to.isbn.13=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    x <- as.character(x)

  x <- gsub("[^\\d|X|x]", "", x, perl=TRUE)

  tried <- normalize_isbn_13(x, aggressive=aggressive)
  where.na <- is.na(tried)
  tried[where.na] <- normalize_isbn_10(x[where.na], aggressive=aggressive,
                               convert.to.isbn.13=convert.to.isbn.13)

  return(tried)
}


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
#'                      separator should be allowed
#'                      (default is \code{FALSE})
#' @param errors.as.nas return NA if error instead of throwing error
#'                      (default is \code{FALSE})
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
    x <- gsub("-", "", x, fixed=TRUE)
  if(any(!(nchar(x[!is.na(x)]) %in% c(7, 8))>0)){
    if(!errors.as.nas) stop("Input must be either 7 or 8 characters")
  }
  where.bad <- !grepl(REGEX.ISSN.8.7, x, perl=TRUE) & !is.na(x)
  if(any(where.bad)){
    if(!errors.as.nas) stop("Illegal input")
    x[where.bad] <- NA
  }

  if(any(!where.bad)){
    first7 <- stringr::str_split(substr(x[!where.bad], 1, 7), "", simplify=TRUE)
    class(first7) <- "numeric"
    first7 <- as.numeric(first7 %*% matrix(8:2))
    should.be <- (11 - (first7 %% 11)) %% 11
    ret <- ifelse(should.be==10, "X", as.character(should.be))
    x[!where.bad] <- ret
  }
  x
}


#' Check the check digit of an ISSN
#'
#' Takes a string representation of an ISSN and verifies that check digit
#' checks out
#'
#' @param x A string of 8 digits or 7 digits with terminal "X"
#' @param allow.hyphens A logical indicating whether the hyphen
#'                      separator should be allowed
#'                      (default is \code{TRUE})
#' @param errors.as.false return false if error instead of throwing error
#'                        (default is \code{FALSE})
#'
#' @return Returns TRUE if check passes, FALSE if not, and NA if NA
#'
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
    x <- gsub("-", "", x, fixed=TRUE)
  where.bad <- !grepl(REGEX.ISSN, x, perl=TRUE) & !is.na(x)
  if(any(where.bad>0)){
    if(!errors.as.false) stop("Illegal input")
  }
  check.digit <- stringr::str_sub(x, -1)
  should.be <- get_issn_check_digit(x, allow.hyphens=allow.hyphens, errors.as.nas=errors.as.false)
  ret <- ifelse(should.be==check.digit, TRUE, FALSE)
  ret[where.bad] <- FALSE
  return(ret)
}



#' Return TRUE if valid ISSN
#'
#' Takes a string representation of an ISSN verifies that it is valid.
#' An ISSN is valid if it is a 8 digit string or a 7 digit string
#' with a terminal "X" AND the check digit matches
#'
#' @param x A string of 8 digits or 7 digits with terminal "X"
#' @param allow.hyphens A logical indicating whether the hyphen
#'                      separator should be allowed
#'                      (default is \code{TRUE})
#' @param lower.x.allowed A logical indicating whether ISSNs with
#'                        a check digit with a lower-case "x" should
#'                        be treated as valid
#'                        (default is \code{TRUE})
#'
#' @return Returns TRUE if checks pass, FALSE if not, and NA if NA
#' @examples
#'
#' is_valid_issn("2434561X")           # TRUE
#' is_valid_issn("2434-561X")          # TRUE
#'
#' # vectorized
#'
#' is_valid_issn(c("2434-561X", "2434-5611", "0378-5955", NA))
#' # TRUE FALSE TRUE NA
#'
#' @export
is_valid_issn <- function(x, allow.hyphens=TRUE, lower.x.allowed=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character"){
    stop("Input must be a character string")
  }
  if(allow.hyphens)
    x <- gsub("-", "", x, fixed=TRUE)
  if(lower.x.allowed)
    x <- toupper(x)
  where.bad <- !grepl(REGEX.ISSN, x, perl=TRUE) & !is.na(x)
  x[where.bad] <- NA
  ret <- ifelse(check_issn_check_digit(x, errors.as.false=TRUE), TRUE, FALSE)
  ret[is.na(x)] <- NA
  ret[where.bad] <- FALSE
  return(ret)
}
attr(is_valid_issn, "assertr_vectorized") <- TRUE



#' Attempt to enforce validity and canonical form to ISSN
#'
#' Takes a string representation of an ISSN. Strips all non-digit
#' and non-"X" characters and checks if it is valid (whether the
#' check digit works out, etc). User can specify whether "aggressive"
#' measures should be taken to salvage the malformed ISSN string.
#'
#' @param x A string
#' @param aggressive A logical indicating whether aggressive measures
#'                      should be taken to try to get the "ISSN"
#'                      into a valid form. See "Details" for more info
#'                      (default is \code{TRUE})
#' @param pretty A logical indicating whether the ISSN should be
#'               prettily hyphenated
#'               (default is \code{FALSE})
#'
#' @details If \code{aggressive} is TRUE, aggressive measures are taken to
#' try to salvage the malformed ISSN string. If the ISSN, for example,
#' is 7 digits, and either adding an "X" to the end, or leading "0"s fix it,
#' this function will return the salvaged ISSN. If the ISSN has
#' garbage digits/characters in the front and has an "X" check digit,
#' it will return the salvaged ISSN.
#'
#' @return Returns valid ISSN if possible, NA if not
#' @examples
#'
#' # adds leading zero
#' normalize_issn("3785955")                          # "03785955"
#'
#' # adds X to 7 digit ISSN if valid
#' normalize_issn("2434561")                          # "2434561X"
#'
#' normalize_issn("2434561", pretty=TRUE)             # "2434-561X"
#'
#' # finds correct sequence
#' normalize_issn("21335212434561X")                  # "2434561X"
#'
#' # vectorized
#' normalize_issn(c("__2434__561X", "2434561", "21335212434561X"))
#' # "2434561X" "2434561X" "2434561X"
#'
#' @export
normalize_issn <- function(x, aggressive=TRUE, pretty=FALSE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    x <- as.character(x)
  x <- toupper(x)
  x <- gsub("[^\\d|X]", "", x, perl=TRUE)
  y <- x
  x <- gsub("X(.+$)", "\\1", x, perl=TRUE)
  is.all.valid <- all(is_valid_issn(x), na.rm=TRUE)
  if(aggressive && !is.all.valid){
    will_padding_zeros_fix_it <- function(x){
      ifelse(nchar(x)==7 & is_valid_issn(stringr::str_pad(x, 8, "left", "0")), TRUE, FALSE)
    }
    will_adding_an_X_fix_it <- function(x){
      ifelse(nchar(x)==7 & get_issn_check_digit(x, errors.as.nas=TRUE)=="X", TRUE, FALSE)
    }
    will_the_first_8_do <- function(x){
      ifelse(nchar(x)>8 & is_valid_issn(substr(x, 1, 8)), TRUE, FALSE)
    }
    will_the_hiddens_do <- function(x){
      ifelse(nchar(x)>8 & is_valid_issn(gsub("^.*?(\\d{7}X).*$", "\\1", x, perl=TRUE)),
             TRUE, FALSE)
    }
    thesevens <- x[nchar(x)==7 & !is.na(x)]
    if(length(thesevens)>0){
      x[nchar(x)==7 & !is.na(x)] <- ifelse(will_padding_zeros_fix_it(thesevens),
                                           stringr::str_pad(thesevens, 8, "left", "0"),
                                           thesevens)
    }
    thesevens <- x[nchar(x)==7 & !is.na(x)]
    if(length(thesevens)>0){
      x[nchar(x)==7 & !is.na(x)] <- ifelse(will_adding_an_X_fix_it(thesevens),
                                           sprintf("%sX", thesevens),
                                           thesevens)
    }
    thebig <- x[nchar(x)>8 & !is.na(x)]
    if(length(thebig)){
      x[nchar(x)>8 & !is.na(x)] <- ifelse(will_the_first_8_do(thebig),
                                          substr(thebig, 1, 8),
                                          thebig)
    }
    loghidden <- grepl("\\d{7}X", y, perl=TRUE) & !is.na(x)
    if(any(loghidden)){
      loghidden[loghidden] <- will_the_hiddens_do(y[loghidden])
      thehiddens <- y[loghidden]
      x[loghidden] <- gsub("^.*?(\\d{7}X).*$", "\\1", thehiddens, perl=TRUE)
    }

  }
  # maybe shouldn't return NA if couldn't be salvaged?
  ret <- ifelse(is_valid_issn(x), x, NA)
  if(pretty)
    ret <- sprintf("%s-%s", substr(x, 1, 4), substr(x, 5, 8))
  return(ret)
}

# ------------------------------------------ #



##############################################
###                 LCCN                   ###
##############################################

#' Attempt to enforce validity of LCCN and convert to normalized form
#'
#' Takes a string representation of an LCCN. Returns a normalized one
#'
#' @param userlccns A string (or vector of strings) of LCCNs
#' @param allow.hyphens a logical indicating whether hyphens separating
#'                      the year and serial should be handled. Adds
#'                      complexity and time to the function.
#'                      (default is \code{TRUE})
#'
#' @details Normalization procedure is documented here:
#' https://www.loc.gov/marc/lccn-namespace.html
#'
#' This does not include revisions or use "#" as a padding character
#' The normalized LCCN is not always the same number of characters
#'
#' @return Returns valid LCCN if possible, NA if not
#'
#' @examples
#'
#' normalize_lccn("n 78890351 ")                  # "n78890351"
#'
#' normalize_lccn("###78890351#")                 # "78890351"
#'
#' normalize_lccn(" 79139101 /AC/r932")           #  "79139101"
#'
#' normalize_lccn("85-2 ")                        #  "85000002"
#' normalize_lccn("85-2 ", allow.hyphens=FALSE)   #  NA
#'
#' # vectorized
#' normalize_lccn(c("85-2 ", " 79139101 /AC/r932", "n 78890351 "))
#' # c("85000002", "79139101", "n78890351"))
#'
#' @export
normalize_lccn <- function(userlccns, allow.hyphens=TRUE){
  if(all(is.na(userlccns))) return(as.character(userlccns))
  if(class(userlccns)!="character")
    stop("Input must be a character string")

  userlccns <- stringr::str_replace_all(userlccns, "\\s", "")
  userlccns <- stringr::str_replace_all(userlccns, "#", "")
  userlccns <- stringr::str_replace(userlccns, "/.+$", "")

  where.bad <- !stringr::str_detect(userlccns, "\\d")
  userlccns[where.bad] <- NA

  where.bad <- stringr::str_detect(userlccns, "[a-z]{4,}")
  userlccns[where.bad] <- NA

  if(allow.hyphens){
    where.bad <- stringr::str_detect(userlccns, "[^a-z\\d-]")
    userlccns[where.bad] <- NA

    pieces <- stringr::str_split(userlccns, "-")

    process_pieces <- function(pieces){
      ifelse(length(pieces)>2, NA_character_,
             ifelse(length(pieces)==1, pieces,
                    ifelse(nchar(pieces[2])>6, NA_character_,
                      sprintf("%s%s", pieces[1],
                              stringr::str_pad(pieces[2], width=6, pad="0")))))
    }
    userlccns <- unlist(lapply(pieces, process_pieces))
  } else{
    where.bad <- stringr::str_detect(userlccns, "[^a-z\\d]")
    userlccns[where.bad] <- NA
  }

  where.bad <- nchar(userlccns)<8 | nchar(userlccns)>12
  userlccns[where.bad] <- NA

  return(userlccns)
}


