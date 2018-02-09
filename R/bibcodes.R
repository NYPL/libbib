

#' Get ISBN 10 check digit
#'
#' Takes a string representation of an ISBN 10
#' and returns the check digit that satisfies the necessary condition.
#' It can take a 10 digit string (and ignore the already extant check digit)
#' or a 9 digit string (without the last digit)
#'
#' @param x A string of nine or 10 digits
#'
#' @return Returns the character check digit that satifies the
#'         mod 11 condition. Returns "X" if 10. Returns NA if input is NA
#' @examples
#'
#' get_isbn_10_check_digit("012491540X")
#'
#' # nine digit string
#' get_isbn_10_check_digit("900403781")
#'
#' # vectorized
#' get_isbn_10_check_digit(c("012491540X", "9004037810", "900403781"))
#'
#' @export
get_isbn_10_check_digit <- function(x){
  if(class(x)!="character")
    stop("Input must be a character string")
  if(sum(!(nchar(x[!is.na(x)]) %in% c(9, 10)))>0)
     stop("Input must be either 9 or 10 characters")
  first9 <- lapply(strsplit(substr(x, 1, 9), ""), as.numeric)
  rem <- unlist(lapply(lapply(first9, function(x) x*(10:2)), sum))
  should.be <- (11 - (rem %% 11)) %% 11
  ifelse(should.be==10, "X", should.be)
}
attr(get_isbn_10_check_digit, "assertr_vectorized") <- TRUE



#' Check the check digit of an ISBN 10
#'
#' Takes a string representation of an ISBN 10 and verifies that check digit
#' checks out
#'
#' @param x A string of or 10 digits or nine digits with terminal "X"
#'
#' @param error.is.false return false if error instead of throwing error
#'
#' @return Returns TRUE if check passes, FALSE if not, and NA if NA
#' @examples
#'
#' check_isbn_10_check_digit("012491540X")  # TRUE
#'
#' # vectorized
#' check_isbn_10_check_digit(c("012491540X", "9004037812"))  # TRUE FALSE
#'
#' @export
check_isbn_10_check_digit <- function(x, error.is.false=FALSE){
  if(class(x)!="character"){
    if(error.is.false)
      return(rep(FALSE, length(x)))
    stop("Input must be a character string")
  }
  if(error.is.false){
    x[nchar(x)!=10] <- "0124915401" # invalid isbn 10
  }
  if(sum(!(nchar(x[!is.na(x)])==10))>0){
    stop("Input must be 10 characters")
  }
  check.digit <- substr(x, 10, 10)
  should.be <- get_isbn_10_check_digit(x)
  ifelse(should.be==toupper(check.digit), TRUE, FALSE)
}
attr(check_isbn_10_check_digit, "assertr_vectorized") <- TRUE


#' Return TRUE if valid ISBN 10
#'
#' Takes a string representation of an ISBN 10 verifies that it is valid.
#' An ISBN 10 is valid if it is a 10 digit string or a 9 digit string
#' with a terminal "X" AND the check digit matches
#'
#' @param x A string of or 10 digits or nine digits with terminal "X"
#' @param lower.x.allowed A logical indicating whether ISBN 10s with
#'                        a check digit with a lower-case "x" should
#'                        be treated as valid
#'
#' @return Returns TRUE if checks pass, FALSE if not, and NA if NA
#' @examples
#'
#' is_valid_isbn_10("012491540X")  # TRUE
#'
#' # vectorized
#' is_valid_isbn_10(c("012491540X", "9004037812"))  # TRUE FALSE
#' is_valid_isbn_10(c("012491540X", "hubo un tiempo"))  # TRUE FALSE
#'
#' @export
is_valid_isbn_10 <- function(x, lower.x.allowed=TRUE){
  if(class(x)!="character"){
    stop("Input must be a character string")
  }
  CHECKREGEX <- "^\\d{9}[\\d|X]$"
  if(lower.x.allowed)
    CHECKREGEX <- "^\\d{9}[\\d|X|x]$"
  ret <- ifelse(grepl(CHECKREGEX, x, perl=TRUE) &
                  check_isbn_10_check_digit(x, error.is.false=TRUE),
                TRUE, FALSE)
  ret[is.na(x)] <- NA
  return(ret)
}
attr(is_valid_isbn_10, "assertr_vectorized") <- TRUE




