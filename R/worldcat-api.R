

# un-exported internal function to get HTTP response
worldcat_api_get_http_response <- function(aurl, print.api.responses=FALSE){
  req <- curl::curl_fetch_memory(aurl)
  status <- req$status_code
  # make more cases
  if(status!=200)
    stop("API returned non-OK status code")
  if(print.api.responses){
    cat("\n\nHTTP response:\n")
    print(req$status_code)
    cat("End of HTTP response\n\n")
  }
  resp <- list(content=rawToChar(req$content),
               http_status_code=req$status_code)
  resp
}



# un-exported internal helper function
worldcat_api_classify <- function(thetype, thenumber, print.api.responses=FALSE){
  if(length(thenumber)>1) stop("only accepts one standard number at a time")
  if(is.na(thenumber)) return(NA)
  if(class(thenumber)!="character")
    stop("ISBN, ISSN, or OCLC number must be a string")

  template      <- "http://classify.oclc.org/classify2/Classify?%s=%s&summary=true"
  api_response  <- worldcat_api_get_http_response(sprintf(template, thetype, thenumber),
                                                  print.api.responses=print.api.responses)
  content       <- api_response$content

  if(print.api.responses){
    cat("\n\nClassify API response:\n")
    cat(content)
    cat("\nEnd of Classify API response\n\n")
  }

  http_status_code  <- api_response$http_status_code
  exemel            <- xml2::read_xml(content, options=NULL)
  xml2::xml_ns_strip(exemel)

  resp_code <- as.integer(xml2::xml_attr(xml2::xml_find_first(exemel,
                                                              "//classify/response"),
                                         "code"))

  # work title, author, holdings, etc..
  work            <- xml2::xml_find_first(exemel, "//classify/work")
  work_title      <- xml2::xml_attr(work, "title")
  work_author     <- xml2::xml_attr(work, "author")
  work_holdings   <- xml2::xml_attr(work, "holdings")
  work_eholdings  <- xml2::xml_attr(work, "eholdings")

  # dcc
  dcc           <- xml2::xml_find_first(exemel,
                                        "//classify/recommendations/ddc/mostPopular")
  dcc_holdings  <- xml2::xml_attr(dcc, "holdings")
  dcc_sfa       <- xml2::xml_attr(dcc, "sfa")
  dcc_frame     <- data.table(standard_number=thenumber,
                              call_type="DCC",
                              holdings=dcc_holdings,
                              recommendation=dcc_sfa)

  # lcc
  lcc           <- xml2::xml_find_first(exemel,
                                        "//classify/recommendations/lcc/mostPopular")
  lcc_holdings  <- xml2::xml_attr(lcc, "holdings")
  lcc_sfa       <- xml2::xml_attr(lcc, "sfa")
  lcc_frame     <- data.table(standard_number=thenumber,
                              call_type="LCC",
                              holdings=lcc_holdings,
                              recommendation=lcc_sfa)

  retframe <- rbindlist(list(dcc_frame, lcc_frame))
  setnames(retframe, "standard_number", thetype)
  retframe[, `:=`(classify_response_code=resp_code,
                  http_status_code=http_status_code,
                  title=work_title, author=work_author,
                  total_holdings=as.integer(work_holdings),
                  total_eholdings=as.integer(work_eholdings))]
  setcolorder(retframe, c(thetype, "title", "author",
                          "total_holdings", "total_eholdings", "call_type",
                          "recommendation", "holdings", "http_status_code",
                          "classify_response_code"))
  return(retframe[])
}



#' Search WorldCat classify API by ISBN, ISSN, or OCLC number
#'
#' Access the results of a WorldCat classify API search by ISBN, ISSN,
#' or OCLC number to get the most frequent call numbers (DCC and LCC)
#' associated with a work. Returns a \code{data.table} with those call
#' numbers and various other metadata. See "Details" for more information.
#'
#' @details
#' The returned \code{data.table} contains fields for various pieces of
#' metadata returned by the API request. These fields include the
#' ISBN/ISSN/OCLC number used, title of work, author, total number of
#' holdings, total number of electronic holdings, call number type,
#' call number recommendation (by most popular), number of holdings
#' using that call number, the HTTP status code, and the Classify
#' API response code.
#'
#' For each ISBN/ISSN/OCLC number used, two rows will be returned; one for
#' the DCC and one for the LCC. Common information (work metadata)
#' will the the same in both rows. If one of the call numbers is missing,
#' the recommendation and holdings fields will be NA.
#'
#' The API can be persnickety, and there are many things that can go
#' wrong. For example, the API can respond with multiple works for a
#' single standard number (ISBN 9780900565748, for example). If this happens,
#' no attempt is made to follow one of the results, and the returned
#' \code{data.table} will return no useful information.
#'
#' If the \code{http_status_code} is 200 and the \code{classify_response_code}
#' is 0, you've received good results.
#' If the \code{classify_response_code} is 4, the standard number may have
#' returned multiple works.
#'
#' The \code{http_status_code} should never not be 200.
#'
#' If something went wrong (for example, the status/response codes are not
#' 200 and 0, respectively), you may want to re-run the function call with
#' \code{print.api.responses} set to \code{TRUE}. This will print the
#' HTTP status code and the raw XML text response from the API.
#'
#' As with all API access functions in this package, it's up to the
#' user to limit their API usage so as to not get blocked. These
#' functions are deliberately not vectorized for this reason; they
#' only accept one standard number at a time.
#'
#' @param x A string representation of an OCLC number
#' @param print.api.responses A logical indicating whether the HTTP and
#'                            classify API responses should be printed
#'                            (for debugging) (default is \code{FALSE})
#'
#' @return A \code{data.table} with most popular DCC and LCC call numbers
#'         and various other metadata. See "Details" for more information.
#'
#' @examples
#'
#' \dontrun{
#'   worldcat_api_classify_by_oclc("93976650")
#'    #         oclc   title           author total_holdings total_eholdings call_type
#'    #       <char>  <char>           <char>          <int>           <int>    <char>
#'    # 1: 939766505 Lobster King, Richard J.            244             534       DCC
#'    # 2: 939766505 Lobster King, Richard J.            244             534       LCC
#'    #    recommendation holdings http_status_code classify_response_code
#'    #            <char>   <char>            <int>                  <int>
#'    # 1:        641.395      767              200                      0
#'    # 2:      QL444.M33      318              200                      0
#' }
#'
#' @name worldcat_api_classify_by



#' @rdname worldcat_api_classify_by
#' @export
worldcat_api_classify_by_oclc <- function(x, print.api.responses=FALSE){
  worldcat_api_classify("oclc", x, print.api.responses=print.api.responses)
}



#' @rdname worldcat_api_classify_by
#' @export
worldcat_api_classify_by_isbn <- function(x, print.api.responses=FALSE){
  worldcat_api_classify("isbn", x, print.api.responses=print.api.responses)
}


#' @rdname worldcat_api_classify_by
#' @export
worldcat_api_classify_by_issn <- function(x, print.api.responses=FALSE){
  worldcat_api_classify("issn", x, print.api.responses=print.api.responses)
}


