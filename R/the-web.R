
#' Get Library of Congress catalog permalinks from LCCNs
#'
#' Takes a string representation of an LCCNs. Returns permalinks
#' to the Library of Congress catalog entries using those LCCNs.
#'
#' @param x A string (or vector of strings) of LCCNs
#' @param normalize a logical indicating whether the LCCN should be
#'                  normalized prior to creating the permalink
#'                  (default = TRUE)
#'
#' @details
#' If normalize=TRUE and the LCCN is invalid, the permalink is NA.
#' If normalize=FALSE, the permalink may be invalid. No validity
#' check on the URL is performed
#'
#' @return Library of Congress permalinks using LCCNs.
#'
#' @examples
#' loc_permalink_from_lccn("n78-890351")        # "https://lccn.loc.gov/n78890351"
#' loc_permalink_from_lccn("85-2 ")             # "https://lccn.loc.gov/85000002"
#' loc_permalink_from_lccn("75-425165//r75")    # "https://lccn.loc.gov/75425165"
#'
#' # vectorized
#' loc_permalink_from_lccn(c("###78890351#", NA, "n78-890351"))
#'
#' @export
loc_permalink_from_lccn <- function(x, normalize=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")
  if(normalize)
    x <- normalize_lccn(x)

  ifelse(is.na(x), NA_character_,
         sprintf("https://lccn.loc.gov/%s", x))
}


#' Get WorldCat catalog permalinks from ISSNs
#'
#' Takes a string representation of ISSNs. Returns permalinks
#' to the WorldCat catalog entries using those ISSNs.
#'
#' @param x A string (or vector of strings) of ISSNs
#' @param normalize a logical indicating whether the ISSNs should be
#'                  normalized prior to creating the permalink
#'                  (default = TRUE)
#'
#' @details
#' If normalize=TRUE and the ISSN is invalid, the permalink is NA.
#' If normalize=FALSE, the permalink may be invalid. No validity
#' check on the URL is performed
#'
#' @return Worldcat permalinks using ISSNs.
#'
#' @examples
#' worldcat_permalink_from_issn("0968-1221")   # http://www.worldcat.org/issn/0968-1221
#'
#' worldcat_permalink_from_issn("2434-561X")   # http://www.worldcat.org/issn/2434561X
#'
#' # vectorized
#' worldcat_permalink_from_issn(c("0968-1221", NA, "2434-561X"))
#'
#' @export
worldcat_permalink_from_issn <- function(x, normalize=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")
  if(normalize)
    x <- normalize_issn(x)

  ifelse(is.na(x), NA_character_,
         sprintf("http://www.worldcat.org/issn/%s", x))
}


#' Get WorldCat catalog permalinks from ISBNs
#'
#' Takes a string representation of ISBNs. Returns permalinks
#' to the WorldCat catalog entries using those ISBNs.
#'
#' @param x A string (or vector of strings) of ISBNs
#' @param normalize a logical indicating whether the ISBNs should be
#'                  normalized prior to creating the permalink
#'                  (default = TRUE)
#'
#' @details
#' If normalize=TRUE and the ISBN is invalid, the permalink is NA.
#' If normalize=FALSE, the permalink may be invalid. No validity
#' check on the URL is performed
#'
#' @return Worldcat permalinks using ISBNs.
#'
#' @examples
#'
#' worldcat_permalink_from_isbn("1788393724")
#' # http://www.worldcat.org/isbn/1788393724
#'
#' worldcat_permalink_from_isbn("0-124-91540-X")
#' # http://www.worldcat.org/isbn/012491540X
#'
#' worldcat_permalink_from_isbn("0-124-91540-X", normalize=FALSE)
#' # http://www.worldcat.org/isbn/0-124-91540-X
#'
#' # vectorized
#' worldcat_permalink_from_isbn(c("1788393724", NA, "0-124-91540-X"))
#'
#' @export
worldcat_permalink_from_isbn <- function(x, normalize=TRUE){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")
  if(normalize)
    x <- normalize_isbn(x)

  ifelse(is.na(x), NA_character_,
         sprintf("http://www.worldcat.org/isbn/%s", x))
}


#' Get WorldCat catalog permalinks from OCLC numbers
#'
#' Takes a string representation of OCLC numbers. Returns permalinks
#' to the WorldCat catalog entries using those OCLC numbers
#'
#' @param x A string (or vector of strings) of OCLC numbers
#'
#' @details
#' If the OCLC number is invalid, the permalink is NA.
#' No validity check on the URL is performed
#'
#' @return Worldcat permalinks using the OCLC numbers
#'
#' @examples
#'
#' worldcat_permalink_from_oclc_number("1005106045")
#' # http://www.worldcat.org/oclc/1005106045
#'
#' # vectorized
#' worldcat_permalink_from_oclc_number(c("1049727704", NA,
#'                                       "1005106045"))
#'
#' @export
worldcat_permalink_from_oclc_number <- function(x){
  if(all(is.na(x))) return(as.character(x))
  if(class(x)!="character")
    stop("Input must be a character string")

  ifelse(is.na(x), NA_character_,
         sprintf("http://www.worldcat.org/oclc/%s", x))
}

