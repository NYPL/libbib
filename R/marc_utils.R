#!/usr/local/bin//Rscript --vanilla

library(data.table)
library(stringr)

options(warn=1)

load("./sysdata.rda")



tests_008 <- c("850617s1911    ilu     ab    000 0 eng  ",
               "841106s1969    ua            000 1 arax ",
               "930111s199u    gt a     b    000 0 spa  ",
               "930111s199u    gt a     b    000 0 zxx  ",
               "930111s199u    gt a     b    000 0 cam  ",
               "930111s199u    gt a     b    000 0   ")

tests_leader <- c("00767cam a2200253   4500",
                  "00948cam a2200265 a 4500")




#########################################################
#########################################################
#########################################################

## LEADER

get_leader_record_status <- function(x, translate=TRUE){
  codes <- stringr::str_sub(x, 6, 6)
  if(translate){
    codes <- ifelse(codes=="a",
                    "Increase in encoding level",
             ifelse(codes=="c",
                    "Corrected or revised",
             ifelse(codes=="d",
                    "Deleted",
             ifelse(codes=="n",
                    "New",
             ifelse(codes=="p",
                    "Increase in encoding level from prepublication",
                    NA)))))
  }
  return(codes)
}
get_leader_record_status(tests_leader)
get_leader_record_status(tests_leader, translate=FALSE)

get_leader_record_type <- function(x, translate=TRUE){
  codes <- stringr::str_sub(x, 7, 7)
  if(translate){
    codes <- ifelse(codes=="a",
                    "Language material",
             ifelse(codes=="c",
                    "Notated music",
             ifelse(codes=="d",
                    "Manuscript notated music",
             ifelse(codes=="e",
                    "Cartographic material",
             ifelse(codes=="f",
                    "Manuscript cartographic material",
             ifelse(codes=="g",
                    "Projected medium",
             ifelse(codes=="i",
                    "Nonmusical sound recording",
             ifelse(codes=="j",
                    "Musical sound recording",
             ifelse(codes=="k",
                    "Two-dimensional nonprojectable graphic",
             ifelse(codes=="m",
                    "Computer file",
             ifelse(codes=="o",
                    "Kit",
             ifelse(codes=="p",
                    "Mixed materials",
             ifelse(codes=="r",
                    "Three-dimensional artifact or naturally occurring object",
             ifelse(codes=="t",
                    "Manuscript language material",
                    NA))))))))))))))
  }
  return(codes)
}
get_leader_record_type(tests_leader)
get_leader_record_type(tests_leader, translate=FALSE)

get_leader_bib_level <- function(x, translate=TRUE){
  codes <- stringr::str_sub(x, 8, 8)
  if(translate){
    codes <- ifelse(codes=="a",
                    "Monographic component part",
             ifelse(codes=="b",
                    "Serial component part",
             ifelse(codes=="c",
                    "Collection",
             ifelse(codes=="d",
                    "Subunit",
             ifelse(codes=="i",
                    "Integrating resource",
             ifelse(codes=="m",
                    "Monograph/Item",
             ifelse(codes=="s",
                    "Serial",
                    NA)))))))
  }
  return(codes)
}
get_leader_bib_level(tests_leader)
get_leader_bib_level(tests_leader, translate=FALSE)


get_leader_info <- function(x, translate=TRUE){
  record_status <- get_leader_record_status(x, translate=translate)
  record_type   <- get_leader_record_type(x, translate=translate)
  bib_level     <- get_leader_bib_level(x, translate=translate)
  return(data.table::data.table(record_status=record_status,
                                record_type=record_type,
                                bib_level=bib_level))
}
get_leader_info(tests_leader)
get_leader_info(tests_leader, translate=FALSE)


#########################################################
#########################################################
#########################################################

## 008

is_valid_format_008 <- function(x){
  return(nchar(x)==40)
}
is_valid_format_008(tests_008)

# 35-37 -> 36,38
get_008_language <- function(x, translate=TRUE){
  langcodes <- data.table(thekey=stringr::str_sub(x, 36, 38))
  if(!translate) return(langcodes[, thekey])
  setindex(langcodes, "thekey")
  results <- langxwalk[langcodes]
  return(results[,language])
}
get_008_language(tests_008)
get_008_language(tests_008, translate=FALSE)


# 15-17 -> 16,18
get_008_pub_place <- function(x, translate=TRUE){
  countrycodes <- data.table(thekey=stringr::str_trim(stringr::str_sub(x, 16, 18)))
  if(!translate) return(countrycodes[, thekey])
  setindex(countrycodes, "thekey")
  results <- countryxwalk[countrycodes]
  return(results[, country])
}
get_008_pub_place(tests_008)
get_008_pub_place(tests_008, translate=FALSE)


##### WHY DO SOME HAVE "U" AT THE END??!!?!
# 7-10 -> 8,11
get_008_date1 <- function(x){
  return(stringr::str_sub(x, 8, 11))
}
get_008_date1(tests_008)


get_008_info  <- function(x, translate=TRUE){
  language    <- get_008_language(x, translate=translate)
  pub_place   <- get_008_pub_place(x, translate=translate)
  date1       <- get_008_date1(x)
  return(data.table::data.table(language=language,
                                pub_place=pub_place,
                                date1=date1))
}
get_008_info(tests_008)
get_008_info(tests_008, translate=FALSE)




