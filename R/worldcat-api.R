

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



####################################
###         CLASSIFY API         ###
####################################

# un-exported internal helper function
worldcat_api_classify <- function(thetype, thenumber, print.api.responses=FALSE){
  if(length(thenumber)>1) stop("only accepts one standard number at a time")
  if(is.na(thenumber)) return(NULL)
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
#' is 0, you've received good results.If the \code{classify_response_code} is
#' 4, the standard number may have returned multiple works.
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
#' Final note: all of these API functions seem to work better with
#' OCLC numbers than any other standard number. If multiple standard
#' numbers are available, using the OCLC number is always preferred.
#'
#' @param x A string representation of the standard number that the function
#'          chosen accepts.
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
#'
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



# --------------------------------------------------------------- #

####################################
###         BIB READ API         ###
####################################

read_a_marcxml_record <- function(exemel, more=FALSE){

  if(is.na(exemel))
    return(NULL)
  if(is.null(exemel))
    return(NULL)

  leader <- oh08 <- publisher <- NULL

  xml2::xml_ns_strip(exemel)

  TITLEXPATH    <- "datafield[@tag='245']/subfield[@code='a']"
  AUTHORXPATH   <- "datafield[@tag='100']/subfield[@code='a']"
  OCLCXPATH     <- "controlfield[@tag='001']"
  ISBNXPATH     <- "datafield[@tag='020']/subfield[@code='a']"
  ISSNXPATH     <- "datafield[@tag='022']/subfield[@code='a']"
  LEADERXPATH   <- "leader"
  OH08XPATH     <- "controlfield[@tag='008']"

  # more
  PUBLISHXPATH  <- "datafield[@tag='260']/subfield[@code='b']"
  # TOPICALXPATH  <- "datafield[@tag='650']/subfield[@code='b']/[contains(. 'fast')]"

  the_title <- xml2::xml_text(xml2::xml_find_first(exemel, TITLEXPATH))
  the_author <- xml2::xml_text(xml2::xml_find_first(exemel, AUTHORXPATH))
  the_oclc <- xml2::xml_text(xml2::xml_find_first(exemel, OCLCXPATH))
  the_isbn <- xml2::xml_text(xml2::xml_find_first(exemel, ISBNXPATH))
  the_issn <- xml2::xml_text(xml2::xml_find_first(exemel, ISSNXPATH))
  the_leader <- xml2::xml_text(xml2::xml_find_first(exemel, LEADERXPATH))
  the_oh08 <- xml2::xml_text(xml2::xml_find_first(exemel, OH08XPATH))
  the_isbn <- normalize_isbn(the_isbn, convert.to.isbn.13=TRUE)
  the_issn <- normalize_issn(the_issn)

  final <- data.table(oclc=the_oclc, isbn=the_isbn, issn=the_issn,
                      title=the_title, author=the_author, leader=the_leader,
                      oh08=the_oh08)

  if(more){
    final[, publisher:=xml2::xml_text(xml2::xml_find_first(exemel, PUBLISHXPATH))]
    li <- marc_leader_get_info(final[1, leader])
    oi <- marc_008_get_info(final[1, oh08])
    final <- cbind(final, li, oi)
    # all_fast_topical_terms <- xml2::xml_text(xml2::xml_find_all(exemel, TOPICALXPATH))
    # print(all_fast_topical_terms)
    # uses original pub date
    setcolorder(final, c("oclc", "isbn", "issn", "title", "author", "pub_date",
                         "lang_code", "bib_level", "record_type",
                         "pub_place_code", "publisher", "leader", "oh08"))
  }

  final[]
}


worldcat_api_bib_read_info_by_something <- function(x,
                                                    type_std_num="oclc",
                                                    wskey=getOption("libbib.wskey", ""),
                                                    more=FALSE,
                                                    debug=FALSE){
  # error checking
  if(length(x)>1) stop("only accepts one standard number at a time")
  if(is.na(x)) return(NULL)
  if(class(x)!="character")
    stop("x must be a string or NULL")
  if(!(type_std_num %chin% c("oclc", "isbn", "issn")))
    stop('type of standard number must be "oclc", "isbn", or "issn"')


  template <- "http://www.worldcat.org/webservices/catalog/content/%s%s?wskey=%s"
  fullurl <- sprintf(template,
                     fcase(type_std_num=="oclc", "",
                           type_std_num=="isbn", "isbn/",
                           type_std_num=="issn", "issn/"),
                     x, wskey)

  if(debug)
    cat(sprintf("\n\nfull api call url:\n%s\n\n", fullurl))

  resp <- worldcat_api_get_http_response(fullurl, print.api.responses=debug)
  content <- resp$content

  if(debug){
    cat("\n\nBib Read API response:\n")
    cat(content)
    cat("\nEnd of Bib Read API response\n\n")
  }

  exemel <- xml2::read_xml(content, options=NULL)
  xml2::xml_ns_strip(exemel)

  final <- read_a_marcxml_record(exemel, more=more)

  return(final[])
}


#' Get bibliographic info from a standard number
#'
#' Access the results of a WorldCat bib read API search by ISBN, ISSN,
#' or OCLC number. The MARCXML returned by the API is parsed and the
#' function returns a \code{data.table} containing the oclc number,
#' ISBN, ISSN, title, author, MARC leader, and the 008 control field,
#' respectively.
#'
#' @details
#' Though this function gets all standard numbers (OCLC, ISBN, ISSN)
#' from the MARCXML, the standard number that was supplied to the function
#' will be the one in the returned \code{data.table}. For example, if
#' you use \code{worldcat_api_bib_read_info_by_isbn}, the returned
#' \code{data.table} will have that ISBN in the ISBN column, not the
#' ISBN in the MARC record.
#'
#' If something went wrong, all columns (except the one corresponding to
#' the supplied standard number) will be NA.
#'
#' This function is helpful to call before attempting to use
#' the Location and Classify API functions as it will ensure that
#' the supplied standard number actually resolves to a OCLC work.
#'
#' As with all API access functions in this package, it's up to the
#' user to limit their API usage so as to not get blocked. These
#' functions are deliberately not vectorized for this reason; they
#' only accept one standard number at a time.
#'
#' This (and other) WorldCat API communication functions require a
#' WorldCat API key. The easiest way to use these functions is to
#' set a global options with your key:
#' \code{options("libbib.wskey"="YOUR KEY HERE")}
#'
#' Final note: all of these API functions seem to work better with
#' OCLC numbers than any other standard number. If multiple standard
#' numbers are available, using the OCLC number is always preferred.
#'
#' @param x A string representation of the standard number that the function
#'          chosen accepts.
#' @param wskey A WorldCat API key (default is \code{getOption("libbib.wskey")})
#' @param more A logical indicating whether more infomation from the MARCXML
#'             should be returned (publisher, bib level etc....) In the
#'             interest of memory consumption, the default is \code{FALSE}
#' @param debug A logical indicating whether the HTTP and bib read API
#'              responses should be printed (for debugging)
#'              (default is \code{FALSE})
#'
#' @return A \code{data.table} containing the OCLC number, ISBN, ISSN,
#'         title, author, MARC leader, and the 008 control field,
#'         respectively,
#'
#' @examples
#'
#' \dontrun{
#' worldcat_api_bib_read_info_by_isbn("9780984201006")
#' #         oclc          isbn   issn                        title
#' #       <char>        <char> <char>                       <char>
#' # 1: 462894360 9780984201006   <NA> The Great Debate about Art /
#' #          author                   leader
#' #          <char>                   <char>
#' # 1: Harris, Roy, 00000cam a2200000 a 4500
#' #                                        oh08
#' #                                      <char>
#' # 1: 091031s2010    ilua     b    000 0 eng c
#'
#' worldcat_api_bib_read_info_by_issn("13602365")
#' #        oclc   isbn     issn                        title author
#' #      <char> <char>   <char>                       <char> <char>
#' # 1: 37787277   <NA> 14664410 The journal of architecture.   <NA>
#' #                      leader                                     oh08
#' #                      <char>                                   <char>
#' # 1: 00000cas a2200000 a 4500 971015c19969999enkbx pso     0   a0eng c
#'
#' }
#'
#' @name worldcat_api_bib_read_info_by

#' @rdname worldcat_api_bib_read_info_by
#' @export
worldcat_api_bib_read_info_by_oclc <- function(x,
                                               wskey=getOption("libbib.wskey", ""),
                                               more=FALSE,
                                               debug=FALSE){
  # error checking
  if(length(x)>1) stop("only accepts one standard number at a time")
  if(is.na(x)) return(NULL)
  if(class(x)!="character")
    stop("x must be a string or NULL")

  oclc <- NULL
  ret <- worldcat_api_bib_read_info_by_something(x, type_std_num="oclc",
                                                 wskey=wskey, more=more,
                                                 debug=debug)
  ret[, oclc:=x]
  return(ret[])
}

#' @rdname worldcat_api_bib_read_info_by
#' @export
worldcat_api_bib_read_info_by_isbn <- function(x,
                                               wskey=getOption("libbib.wskey", ""),
                                               more=FALSE,
                                               debug=FALSE){
  # error checking
  if(length(x)>1) stop("only accepts one standard number at a time")
  if(is.na(x)) return(NULL)
  if(class(x)!="character")
    stop("x must be a string or NULL")

  isbn <- NULL
  ret <- worldcat_api_bib_read_info_by_something(x, type_std_num="isbn",
                                                 wskey=wskey, more=more,
                                                 debug=debug)
  ret[, isbn:=x]
  return(ret[])
}

#' @rdname worldcat_api_bib_read_info_by
#' @export
worldcat_api_bib_read_info_by_issn <- function(x,
                                               wskey=getOption("libbib.wskey", ""),
                                               more=FALSE,
                                               debug=FALSE){
  # error checking
  if(length(x)>1) stop("only accepts one standard number at a time")
  if(is.na(x)) return(NULL)
  if(class(x)!="character")
    stop("x must be a string or NULL")

  issn <- NULL
  ret <- worldcat_api_bib_read_info_by_something(x, type_std_num="issn",
                                                 wskey=wskey,
                                                 more=more, debug=debug)
  ret[, issn:=x]
  return(ret[])
}




# --------------------------------------------------------------- #

####################################
###         LOCATION API         ###
####################################

# un-exported (but tested) helper function
construct_wcapiloc_url <- function(stdnum,
                                   type_std_num="oclc",
                                   location="10032",
                                   max_libraries=100,
                                   servicelevel="full",
                                   frbrGrouping="on",
                                   libtype=NULL,
                                   start_at=1,
                                   wskey=getOption("libbib.wskey", "")){

  # error checking
  if(class(stdnum)!="character")
    stop("standard number must be a string")
  if(class(max_libraries)!="numeric")
    stop("max_libraries must be a number between 1 and 100")
  if(max_libraries < 1 || max_libraries > 100)
    stop("max_libraries must be a number between 1 and 100")
  if(!is.null(location) && class(location)!="character")
    stop("location must be a string or NULL")
  if(!(type_std_num %chin% c("oclc", "isbn", "issn")))
    stop('type of standard number must be "oclc", "isbn", or "issn"')
  if(!(servicelevel %chin% c("full", "default")))
    stop('service level must be "full" or "default"')
  if(!(frbrGrouping %chin% c("on", "off")))
    stop('frfbGrouping must be "on" or "off"')
  if(!is.null(libtype) && !(libtype %chin% c("academic", "public", "government", "other")))
    stop('libtype must be either NULL, "academic", "public", "government", or "other"')
  if(class(start_at)!="numeric")
    stop("start_at must be a number")

  building <- "http://www.worldcat.org/webservices/catalog/content/libraries/"

  # standard number
  building <- sprintf("%s%s%s?", building, fcase(type_std_num=="isbn", "isbn/",
                                                 type_std_num=="issn", "issn/",
                                                 default=""), stdnum)

  # location
  building <- sprintf("%s%s", building,
                      ifelse(!is.null(location),
                             sprintf("location=%s&", location), ""))

  # max libraries
  building <- sprintf("%s%s", building,
                      sprintf("maximumLibraries=%s&", max_libraries))

  # service level
  building <- sprintf("%s%s", building,
                      sprintf("servicelevel=%s&", servicelevel))

  # frbr grouping
  building <- sprintf("%s%s", building,
                      sprintf("frbrGrouping=%s&", frbrGrouping))

  # lib type
  building <- sprintf("%s%s", building,
                      ifelse(!is.null(libtype),
                             sprintf("libtype=%s&",
                                     fcase(libtype=="academic",   "1",
                                           libtype=="public",     "2",
                                           libtype=="government", "3",
                                           libtype=="other",      "4")), ""))

  # start at
  building <- sprintf("%s%s&", building, sprintf("startLibrary=%s", start_at))

  # wskey
  building <- sprintf("%swskey=%s", building, wskey)

  return(building)
}


worldcat_api_locations_helper <- function(x,
                                          type_std_num="oclc",
                                          location="10032",
                                          max_libraries=100,
                                          servicelevel="full",
                                          frbrGrouping="on",
                                          libtype=NULL,
                                          start_at=1,
                                          wskey=getOption("libbib.wskey", ""),
                                          debug=FALSE){
  fullurl <- construct_wcapiloc_url(x, type_std_num=type_std_num,
                                    location=location,
                                    max_libraries=max_libraries,
                                    servicelevel=servicelevel,
                                    frbrGrouping=frbrGrouping,
                                    libtype=libtype, start_at=start_at,
                                    wskey=wskey)
  if(debug)
    cat(sprintf("\n\nfull api call url:\n%s\n\n", fullurl))

  resp <- worldcat_api_get_http_response(fullurl, print.api.responses=debug)
  content <- resp$content

  if(debug){
    cat("\n\nLocation API response:\n")
    cat(content)
    cat("\nEnd of Location API response\n\n")
  }

  exemel <- xml2::read_xml(content, options=NULL)
  xml2::xml_ns_strip(exemel)

  all_holdings <- xml2::xml_find_all(exemel, "//holdings/holding")
  all_ident <- xml2::xml_text(xml2::xml_find_first(all_holdings, "institutionIdentifier/value"))
  all_locs <- xml2::xml_text(xml2::xml_find_first(all_holdings, "physicalLocation"))
  all_hold <- xml2::xml_text(xml2::xml_find_first(all_holdings, "holdingSimple/copiesSummary/copiesCount"))

  if(length(all_holdings)==0)
    return(NULL)

  final <- data.table(standard_num=x,
                      institution_identifier=all_ident,
                      institution_name=all_locs,
                      copies=all_hold)
  setnames(final, "standard_num", type_std_num)
  return(final[])
}


worldcat_api_locations_by_something <- function(x,
                                               type_std_num="oclc",
                                               include.bib.info=TRUE,
                                               location="10032",
                                               max_libraries=Inf,
                                               servicelevel="full",
                                               frbrGrouping="on",
                                               libtype=NULL,
                                               wskey=getOption("libbib.wskey", ""),
                                               print.progress=TRUE,
                                               debug=FALSE){
  # error checking
  if(length(x)>1) stop("only accepts one standard number at a time")
  if(is.na(x)) return(NULL)
  if(class(x)!="character")
    stop("x must be a string or NULL")

  # debug implies print progress
  if(debug) print.progress=TRUE

  all_the_way_p <- FALSE
  if(is.infinite(max_libraries)){
    all_the_way_p <- TRUE
    max_libraries <- 100
  }

  runninglist <- list()
  counter <- 1
  starting_at <- 1

  ret <- worldcat_api_locations_helper(x, type_std_num=type_std_num,
                                      location=location,
                                      max_libraries=max_libraries,
                                      servicelevel=servicelevel,
                                      frbrGrouping=frbrGrouping, libtype=libtype,
                                      start_at=1, wskey=wskey, debug=debug)
  if(is.null(ret))
    return(NULL)

  bibinfo <- NULL
  if(include.bib.info){
    bibinfo <- worldcat_api_bib_read_info_by_something(x,
                                                       type_std_num=type_std_num,
                                                       wskey=wskey,
                                                       debug=debug)
  }

  if(!all_the_way_p || ret[,.N]<100){
    if(include.bib.info){
      dt_del_cols(bibinfo, type_std_num)
      ret <- cbind(ret, bibinfo)
    }
    return(ret[])
  }

  runninglist[[counter]] <- ret
  last_pull_n_count <- 100

  while(last_pull_n_count==100){
    starting_at <- 1+(100*counter)
    cat(sprintf("request %s pulled %s rows... repeating, starting at library %s\n",
                counter, last_pull_n_count, starting_at))

    ret <- worldcat_api_locations_helper(x, type_std_num=type_std_num,
                                        max_libraries=max_libraries,
                                        location=location,
                                        servicelevel=servicelevel,
                                        frbrGrouping=frbrGrouping,
                                        libtype=libtype,
                                        start_at=starting_at,
                                        wskey=wskey, debug=debug)
    if(is.null(ret)){
      last_pull_n_count <- 0
    } else{
      counter <- counter + 1
      runninglist[[counter]] <- ret
      last_pull_n_count <- ret[,.N]
    }
  }

  final <- rbindlist(runninglist)

  cat(sprintf("request %s pulled %s rows, returning with all data tables with %s rows in total\n\n",
              counter, last_pull_n_count, final[,.N]))

  if(include.bib.info){
    dt_del_cols(bibinfo, type_std_num)
    final <- cbind(final, bibinfo)
  }

  return(final[])
}


#' Get holding libraries by standard number
#'
#' Access the results of a WorldCat location API search by ISBN, ISSN,
#' or OCLC number. Returns a \code{data.table} with rows corresponding to
#' each holding institution. The columns contain the standard number
#' provided, the institution identifier, the institution name, number
#' of copies held by that insitution, and, by default, the bibliographic
#' information provided by \code{worldcat_api_bib_read_info_by_...}.
#' This information is helpful to ensure thaat the standard number
#' provided successfully resolved to a single OCLC work.
#'
#' Numerous parameters are provided that change the API url
#' parameters. See parameter section for details on each.
#'
#' @details
#' If something went wrong, most columns (especially the bibliographic
#' info columns) will be NA. You should always check the output.
#'
#' As with all API access functions in this package, it's up to the
#' user to limit their API usage so as to not get blocked. These
#' functions are deliberately not vectorized for this reason; they
#' only accept one standard number at a time.
#'
#' This (and other) WorldCat API communication functions require a
#' WorldCat API key. The easiest way to use these functions is to
#' set a global options with your key:
#' \code{options("libbib.wskey"="YOUR KEY HERE")}
#'
#' Final note: all of these API functions seem to work better with
#' OCLC numbers than any other standard number. If multiple standard
#' numbers are available, using the OCLC number is always preferred.
#' In this function, for example, searching for ISSN: 14664410
#' (Journal of Architecture) will (at time of writing) return only one
#' insitution, whereas searching by it's OCLC number (958283020) will
#' yield many more (660, at time of writing, with default parameters).
#'
#' @param x The standard number to search using. Must be a string.
#' @param include.bib.info A logical indicating whether to include
#'                         bibliographic metadata associated with the
#'                         work (provided by
#'                         \code{worldcat_api_bib_read_info_by_...}).
#'                         This is very useful for error checking so
#'                         default is \code{TRUE}.
#' @param location The holding institutions are sorted roughly by geographic
#'                 proximity to this zip-code, country code, etc...
#'                 If \code{max_libraries} is \code{Inf} (the default), the
#'                 starting location doesn't matter since all holding
#'                 institutions are returned. Defaults to the zip code
#'                 of Washington Heights, NYC.
#' @param max_libraries The maximum number of libraries to return.
#'                      Must be a number between 0 and 100 or \code{Inf}.
#'                      If \code{Inf} (default), the function will
#'                      automatically make all follow-up requests to retrieve
#'                      all holding institutions. Beware that each page of
#'                      100 institutions counts as one API request. If the
#'                      bib searched for is popular, set this to non-\code{Inf}.
#' @param servicelevel Either "full" (the default) or "default". If "full",
#'                     the number of holding libraries returned is the same
#'                     as if a user logged in to an institution when making
#'                     a WorldCat search. If "default", the results are
#'                     a subset of WorldCat libraries, namely those that
#'                     participate in worldcat.org. In this way, the number
#'                     of holding libraries is tantamount to if a non-logged-in
#'                     user searched WorldCat. The number of results with "full"
#'                     is always at least as high as with "default", so the
#'                     default is "full". If this package is being used in
#'                     an application where a user is not logged in to an
#'                     institution, set this to "default". It is up to you
#'                     to respect the WorldCat API's conditions.
#' @param frbrGrouping With this parameter set to "on" (default),
#'                     an attempt is made by the WorldCat API to group
#'                     together similar editions and present only the top
#'                     held record as the representative record for that group.
#'                     If not, only institutions holding the exact standard
#'                     number specified will be returned.
#' @param libtype One of \code{NULL} (default), "academic", "public",
#'                "government", or "other". \code{NULL} will return all
#'                library subsets. The others will only search for holdings
#'                from insitutions of that library type.
#' @param wskey A WorldCat API key (default is \code{getOption("libbib.wskey")})
#' @param print.progress A logical indicating whether a message should be
#'                       displayed for each API request. If \code{max_libraries}
#'                       is \code{TRUE} a message will be displayed for every
#'                       group of 100 institutions the function fetches.
#'                       (default is \code{TRUE})
#' @param debug A logical indicating whether the HTTP and API
#'              responses should be printed (for debugging)
#'              (default is \code{FALSE})
#'
#' @return A \code{data.table} with each row corresponding to a holding library.
#'
#' @examples
#'
#' \dontrun{
#' # worldcat_api_locations_by_oclc("877749545", max_libraries=10,
#' #                                include.bib.info=FALSE)
#' #         oclc institution_identifier
#' #       <char>                 <char>
#' # 1: 877749545                    NLE
#' # 2: 877749545                    NLW
#' # 3: 877749545                    EUM
#' # 4: 877749545                    LTU
#' # 5: 877749545                    ELU
#' # 6: 877749545                  UKUAL
#' #                                 institution_name copies
#' #                                           <char> <char>
#' # 1:                  National Library of Scotland      1
#' # 2:                     National Library of Wales      1
#' # 3:              University of Manchester Library      1
#' # 4: University of Leicester, David Wilson Library      1
#' # 5:     University of London Senate House Library      1
#' # 6:                 University of the Arts London      1
#'
#' }
#'
#' @name worldcat_api_locations_by

#' @rdname worldcat_api_locations_by
#' @export
worldcat_api_locations_by_oclc <- function(x,
                                          location="10032",
                                          include.bib.info=TRUE,
                                          max_libraries=Inf,
                                          servicelevel="full",
                                          frbrGrouping="on",
                                          libtype=NULL,
                                          wskey=getOption("libbib.wskey", ""),
                                          print.progress=TRUE,
                                          debug=FALSE){
  ret <- worldcat_api_locations_by_something(x, type_std_num="oclc",
                                            include.bib.info=include.bib.info,
                                            location=location,
                                            max_libraries=max_libraries,
                                            servicelevel=servicelevel,
                                            frbrGrouping=frbrGrouping,
                                            libtype=libtype,
                                            wskey=wskey,
                                            print.progress=print.progress,
                                            debug=debug)
  if(is.null(ret))
    return(NULL)

  return(ret[])
}


#' @rdname worldcat_api_locations_by
#' @export
worldcat_api_locations_by_isbn <- function(x,
                                          location="10032",
                                          include.bib.info=TRUE,
                                          max_libraries=Inf,
                                          servicelevel="full",
                                          frbrGrouping="on",
                                          libtype=NULL,
                                          wskey=getOption("libbib.wskey", ""),
                                          print.progress=TRUE,
                                          debug=FALSE){
  ret <- worldcat_api_locations_by_something(x, type_std_num="isbn",
                                            include.bib.info=include.bib.info,
                                            location=location,
                                            max_libraries=max_libraries,
                                            servicelevel=servicelevel,
                                            frbrGrouping=frbrGrouping,
                                            libtype=libtype,
                                            wskey=wskey,
                                            print.progress=print.progress,
                                            debug=debug)
  if(is.null(ret))
    return(NULL)

  return(ret[])
}

#' @rdname worldcat_api_locations_by
#' @export
worldcat_api_locations_by_issn <- function(x,
                                          location="10032",
                                          include.bib.info=TRUE,
                                          max_libraries=Inf,
                                          servicelevel="full",
                                          frbrGrouping="on",
                                          libtype=NULL,
                                          wskey=getOption("libbib.wskey", ""),
                                          print.progress=TRUE,
                                          debug=FALSE){
  ret <- worldcat_api_locations_by_something(x, type_std_num="issn",
                                            include.bib.info=include.bib.info,
                                            location=location,
                                            max_libraries=max_libraries,
                                            servicelevel=servicelevel,
                                            frbrGrouping=frbrGrouping,
                                            libtype=libtype,
                                            wskey=wskey,
                                            print.progress=print.progress,
                                            debug=debug)
  if(is.null(ret))
    return(NULL)

  return(ret[])
}



# --------------------------------------------------------------- #

####################################
###          SEARCH API          ###
####################################

# un-exported helper function
construct_wcapi_search_url <- function(sru, max_records=100,
                                       frbrGrouping="on", start_at=1,
                                       wskey=getOption("libbib.wskey", "")){
  # error checking
  if(class(sru)!="character")
    stop("SRU search must be a string")
  if(class(max_records)!="numeric")
    stop("max_records must be a number between 1 and 100")
  if(max_records < 1 || max_records > 100)
    stop("max_records must be a number between 1 and 100")
  if(!(frbrGrouping %chin% c("on", "off")))
    stop('frfbGrouping must be "on" or "off"')
  if(class(start_at)!="numeric")
    stop("start_at must be a number")

  building <- "http://www.worldcat.org/webservices/catalog/search/worldcat/sru"

  # wskey
  building <- sprintf("%s?wskey=%s", building, wskey)

  # sru query
  building <- sprintf("%s&query=%s", building, URLencode(sru))

  # max records
  building <- sprintf("%s&maximumRecords=%s", building, max_records)

  # frbr grouping
  building <- sprintf("%s&frbrGrouping=%s", building, frbrGrouping)

  # start at
  building <- sprintf("%s&startRecord=%s", building, start_at)

  # lib type appears not to matter

  # Turns out that service level matters a great deal
  building <- sprintf("%s&servicelevel=full", building)

  building
}


worldcat_api_search_helper <- function(sru, max_records=100,
                                       frbrGrouping="on", start_at=1,
                                       wskey=getOption("libbib.wskey", ""),
                                       more=TRUE,
                                       debug=FALSE){

  query <- result_number <- NULL

  fullurl <- construct_wcapi_search_url(sru,
                                        max_records=max_records,
                                        frbrGrouping=frbrGrouping,
                                        start_at=start_at,
                                        wskey=wskey)
  if(debug)
    cat(sprintf("\n\nfull api call url:\n%s\n\n", fullurl))

  resp <- worldcat_api_get_http_response(fullurl, print.api.responses=debug)
  content <- resp$content

  if(debug){
    cat("\nSearch API response:\n")
    cat(content)
    cat("\nEnd of Search API response\n\n")
  }

  exemel <- xml2::read_xml(content, options=NULL)
  xml2::xml_ns_strip(exemel)

  num_results <- xml2::xml_text(xml2::xml_find_first(exemel, "//searchRetrieveResponse/numberOfRecords"))
  all_records <- xml2::xml_find_all(exemel, "//searchRetrieveResponse/records/record/recordData/record")
  num_records <- length(all_records)
  if(num_records==0)
    return(NULL)

  tmp <- lapply(all_records, function(x){read_a_marcxml_record(x, more=more)})
  ret <- rbindlist(tmp)

  ret[, num_results:=num_results]
  ret[, result_number:=seq(start_at, start_at+num_records-1)]
  ret[, query:=sru]
  setcolorder(ret, c("num_results", "result_number"))
  ret[]
}




#' Use the WorldCat Search API
#'
#' Searches WorldCat using a CQL query. Returns a \code{data.table}
#' containing the bibliographic metadata of the results, along
#' with the total number of results.
#'
#' @param sru The search query (in CQL syntax). See \code{examples} section
#'            for some examples.
#' @param max_records The maximum number of search results to return.
#'                    Must be a number between 0 and 100 or \code{Inf}.
#'                    If \code{Inf} (default), the function will
#'                    automatically make all follow-up requests to retrieve
#'                    all search results. For safety, the default is 10.
#' @param frbrGrouping With this parameter set to "on" (default),
#'                     an attempt is made by the WorldCat API to group
#'                     together similar editions and present only the top
#'                     held record as the representative record for that group.
#' @param start_at The search result to start at (default is 1)
#' @param wskey A WorldCat API key (default is \code{getOption("libbib.wskey")})
#' @param more A logical indicating whether more infomation from the MARCXML
#'             search results should be returned (publisher, bib level, etc....).
#'             (Default is \code{TRUE})
#' @param print.progress A logical indicating whether a message should be
#'                       displayed for each API request. If \code{max_records}
#'                       is \code{TRUE} a message will be displayed for every
#'                       group of 100 search results the function fetches.
#'                       (default is \code{TRUE})
#' @param debug A logical indicating whether the HTTP and API
#'              responses should be printed (for debugging)
#'              (default is \code{FALSE})
#'
#' @details
#'
#' As with all API access functions in this package, it's up to the
#' user to limit their API usage so as to not get blocked. These
#' functions are deliberately not vectorized for this reason; they
#' only accept one standard number at a time.
#'
#' This (and other) WorldCat API communication functions require a
#' WorldCat API key. The easiest way to use these functions is to
#' set a global options with your key:
#' \code{options("libbib.wskey"="YOUR KEY HERE")}
#'
#' @return A \code{data.table} containing the bibliographic metadata of the
#'         results, along with the total number of results.
#'
#' @examples
#'
#' \dontrun{
#'
#' # A title search of "The Brothers Karamazov"
#' worldcat_api_search('srw.ti="Brothers Karamazov"')
#'
#' # Search for title "Madame Bovary" by author "Gustave Flaubert"
#' # in language Greek (all results)
#' sru <- 'srw.au="Gustave Flaubert" and srw.ti="Madame Bovary" and srw.la=gre'
#' worldcat_api_search(sru, max_records=Inf)
#'
#' # Hip Hop (subject) materials on Cassette, CD, or wax from years 1987 to 1990
#' sru <- '((srw.mt=cas or srw.mt=cda or srw.mt=lps) and srw.su="Rap") and srw.yr="1987-1990"'
#' worldcat_api_search(sru)
#'
#' # all materials with keyword "Common Lisp" at The New York Public Library
#' sru <- 'srw.kw="common lisp" and srw.li=NYP'
#' worldcat_api_search(sru, max_records=Inf)
#'
#' # 19th century materials on ethics (Dewey code 170s / LC Call prefix BJ)
#' sru <- '(srw.dd="17*" or srw.lc="bj*") and srw.yr="18*"'
#' worldcat_api_search(sru, max_records=Inf)
#'
#' # Music (Dewey 780s) materials that are only held by The New York Public
#' # Library (a "cg" code of 11 means there is only one holding)
#' # [searching with debugging]
#' sru <- 'srw.dd="78*" and srw.li=NYP and srw.cg=11'
#' worldcat_api_search(sru, debug=TRUE)
#'
#' }
#' @export
worldcat_api_search <- function(sru, max_records=10,
                                 frbrGrouping="on", start_at=1,
                                 wskey=getOption("libbib.wskey", ""),
                                 more=TRUE, print.progress=TRUE,
                                 debug=FALSE){
  # debug implies print progress
  if(debug) print.progress=TRUE

  all_the_way_p <- FALSE
  if(is.infinite(max_records)){
    all_the_way_p <- TRUE
    max_records <- 100
  }

  runninglist <- list()
  counter <- 1
  starting_at <- 1

  ret <- worldcat_api_search_helper(sru, max_records=max_records,
                                    frbrGrouping=frbrGrouping,
                                    start_at=start_at,
                                    wskey=wskey,
                                    more=more, debug=debug)

  if(is.null(ret)){
    print("no results found")
    return(NULL)
  }

  if(!all_the_way_p || ret[,.N]<100){
    return(ret[])
  }

  runninglist[[counter]] <- ret
  last_pull_n_count <- 100

  while(last_pull_n_count==100){
    starting_at <- 1+(100*counter)
    cat(sprintf("request %s pulled %s rows... repeating, starting at result %s\n",
                counter, last_pull_n_count, starting_at))

    ret <- worldcat_api_search_helper(sru, max_records=max_records,
                                      frbrGrouping=frbrGrouping,
                                      start_at=starting_at,
                                      wskey=wskey,
                                      more=more, debug=debug)
    counter <- counter + 1
    runninglist[[counter]] <- ret
    last_pull_n_count <- ret[,.N]
  }

  final <- rbindlist(runninglist)

  cat(sprintf("request %s pulled %s rows, returning with all data tables with %s rows in total\n\n",
              counter, last_pull_n_count, final[,.N]))

  return(final[])
}
