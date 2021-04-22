
#' Get info from MARC leader
#'
#' Takes one or more MARC leaders (string/strings) and returns a
#' \code{data.table} containing the record type and bib level
#'
#' @param x A string (or vector of strings) of MARC leaders
#'
#' @return A \code{data.table}
#'
#' @examples
#'
#' marc_leader_get_info("00000cam a22000008i 4500")
#' #          record_type      bib_level
#' #               <char>         <char>
#' # 1: Language Material Monograph/Item
#'
#' # vectorized
#' marc_leader_get_info(c("00000cam a2200000Ma 4500", NA,
#'                        "00000cem a2200000Ma 4500"))
#' #              record_type      bib_level
#' #                    <char>         <char>
#' #  1:     Language Material Monograph/Item
#' #  2:                  <NA>           <NA>
#' #  3: Cartographic material Monograph/Item
#'
#' @export
marc_leader_get_info <- function(x){
  if(class(x)!="character")
    stop("x must be a string or NA")

  record_type       <- stringr::str_sub(x, 7, 7)
  bib_level         <- stringr::str_sub(x, 8, 8)

  record_type <- fcase(record_type=="a", "Language Material",
                       record_type=="c", "Notated music",
                       record_type=="d", "Manuscript notated music",
                       record_type=="e", "Cartographic material",
                       record_type=="f", "Manuscript cartographic material",
                       record_type=="g", "Projected medium",
                       record_type=="i", "Nonmusical sound recording",
                       record_type=="j", "Musical sound recording",
                       record_type=="k", "Two-dimensional nonprojectable graphic",
                       record_type=="m", "Computer file",
                       record_type=="o", "Kit",
                       record_type=="p", "Mixed materials",
                       record_type=="r", "Three-dimensional artifact or naturally occurring object",
                       record_type=="t", "Manuscript language material ",
                       default=NA)

  bib_level <- fcase(bib_level=="a", "Monographic component part",
                     bib_level=="b", "Serial component part",
                     bib_level=="c", "Collection",
                     bib_level=="d", "Subunit",
                     bib_level=="i", "Integrating resource",
                     bib_level=="m", "Monograph/Item",
                     bib_level=="s", "Serial",
                     default=NA)

  ret <- data.table(record_type=record_type, bib_level=bib_level)
  ret[]
}



#' Get info from MARC control field 008
#'
#' Takes one or more MARC 008 fields (string/strings) and returns a
#' \code{data.table} containing the publication date, publication
#' place code, and language code.
#'
#' @param x A string (or vector of strings) of LCCNs
#' @param original.pub.date If \code{TRUE} (default) and if applicable,
#'                          return the original publication date, not
#'                          the re-issue publication date
#' @param include.questionable.dates A logical indicating whether "questionable"
#'                                   dates should be replaced with \code{NA}
#'
#' @return A \code{data.table}
#'
#' @examples
#'
#' # The Brothers Karamazov (1970 reissue but original publication date)
#' marc_008_get_info("950622r19701880ru            000 0 rus d")
#' #    pub_date pub_place_code lang_code
#' #       <int>         <char>    <char>
#' # 1:     1880             ru       rus
#'
#' # reissue publication date
#' marc_008_get_info("950622r19701880ru            000 0 rus d",
#'                   original.pub.date=FALSE)
#' #     pub_date pub_place_code lang_code
#' #        <int>         <char>    <char>
#' #  1:     1970             ru       rus
#'
#' # vectorized
#' marc_008_get_info(c("101106s1992    gr            000 1 gre d", NA,
#'                     "180528s2017    ag            000 j spa d"))
#' #      pub_date pub_place_code lang_code
#' #         <int>         <char>    <char>
#' #   1:     1992             gr       gre
#' #   2:       NA           <NA>      <NA>
#' #   3:     2017             ag       spa
#'
#' @export
marc_008_get_info <- function(x, original.pub.date=TRUE,
                              include.questionable.dates=FALSE){
  if(class(x)!="character")
    stop("x must be a string or NA")

  pub_date <- NULL

  status          <- stringr::str_sub(x, 7, 7)
  date1           <- stringr::str_sub(x, 8, 11)
  date2           <- stringr::str_sub(x, 12, 15)
  pub_place_code  <- stringr::str_trim(stringr::str_sub(x, 16, 18))
  lang_code       <- stringr::str_sub(x, 36, 38)
  ret <- data.table(status=status, pub_date=date1, date2=date2,
                    pub_place_code=pub_place_code, lang_code=lang_code)
  if(original.pub.date){
    ret[status=="r" & !is.na(date2), pub_date:=date2]
  }
  if(!include.questionable.dates)
    ret[!(status %chin% c("r", "s")), pub_date:=NA]
  ret[!stringr::str_detect(pub_date, "^\\d{3,4}$"), pub_date:=NA]
  ret[,pub_date:=as.integer(pub_date)]
  ret[,status:=NULL]
  ret[,date2:=NULL]
  ret[]
}



