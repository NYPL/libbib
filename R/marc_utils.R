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

# 0-4 - record length
get_leader_record_length <- function(x){
  codes <- stringr::str_sub(x, 0, 5)
  return(codes)
}

# 5 - record status
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

# 6 - type of record
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

# 7 - bibliographic level
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

# 8 - type of control
get_leader_type_of_control <- function(x){
  codes <- stringr::str_sub(x, 9, 9)
  return(codes)
}

# 17 - encoding level
get_leader_encoding_level <- function(x, translate=TRUE){
  codes <- stringr::str_sub(x, 18, 18)
  if(translate){
    codes <- ifelse(codes=="#",
                    "Full level",
                  ifelse(codes=="1",
                    "Full level, material not examined",
                  ifelse(codes=="2",
                    "Less-than-full level, material not examined",
                  ifelse(codes=="3",
                    "Abbreviated level",
                  ifelse(codes=="4",
                    "Core level",
                  ifelse(codes=="5",
                    "Partial (preliminary) level",
                  ifelse(codes=="7",
                    "Minimal level",
                  ifelse(codes=="8",
                    "Prepublication level",
                  ifelse(codes=="u",
                    "Unknown",
                  ifelse(codes=="z",
                    "Not applicable",
                    NA))))))))))
  }
  return(codes)
}
get_leader_encoding_level(tests_leader)
get_leader_encoding_level(tests_leader, translate=FALSE)


get_leader_info <- function(x, translate=TRUE){
  leader_length <- get_leader_record_length(x)
  record_status <- get_leader_record_status(x, translate=translate)
  record_type   <- get_leader_record_type(x, translate=translate)
  bib_level     <- get_leader_bib_level(x, translate=translate)
  encodinglevel <- get_leader_encoding_level(x, translate=translate)
  return(data.table::data.table(record_length=leader_length,
                                record_status=record_status,
                                record_type=record_type,
                                bib_level=bib_level,
                                type_of_control=type_of_cntrl,
                                encodinglevel=encodinglevel))
}
get_leader_info(tests_leader)
get_leader_info(tests_leader, translate=FALSE)


#########################################################
#########################################################
#########################################################
nyplleaders <- readLines(con="~/Desktop/get-leaders/some-headers.txt")
nyplleaders <- data.table(leader=nyplleaders)
leaderinfo <- get_leader_info(nyplleaders[, leader])
nyplfullleader <- cbind(nyplleaders, leaderinfo)
nyplfullleader[, .N, bib_level][order(-N)]
nyplfullleader[, .N, record_type][order(-N)]
nyplfullleader[, .N, record_status][order(-N)]
nyplfullleader[, .N, encodinglevel][order(-N)]
#########################################################
#########################################################




#########################################################
#########################################################
#########################################################

## 008

is_valid_format_008 <- function(x){
  return(nchar(x)==40)
}
is_valid_format_008(tests_008)

# 0-5 - date entered on file
get_008_date_entered <- function(x, translate=translate){
  return(stringr::str_sub(x, 1, 6))
}

# 6 - type of date
get_008_type_of_date <- function(x, translate=TRUE){
  codes <- stringr::str_sub(x, 7, 7)
  if(translate){
    codes <- ifelse(codes=="b",
                    "No dates given; B.C. date involved",
             ifelse(codes=="c",
                    "Continuing resource currently published",
             ifelse(codes=="d",
                    "Continuing resource currently published",
             ifelse(codes=="e",
                    "Detailed date",
             ifelse(codes=="i",
                    "Inclusive dates of collection",
             ifelse(codes=="k",
                    "Range of years of bulk of collection",
             ifelse(codes=="m",
                    "Multiple dates",
             ifelse(codes=="n",
                    "Dates unknown",
             ifelse(codes=="p",
                    "Date of distribution/release/issue and production/recording session when different",
             ifelse(codes=="q",
                    "Questionable date",
             ifelse(codes=="r",
                    "Reprint/reissue date and original date",
             ifelse(codes=="s",
                    "Single known date/probable date",
             ifelse(codes=="t",
                    "Publication date and copyright date",
             ifelse(codes=="u",
                    "Continuing resource status unknown",
             ifelse(codes=="|",
                    "No attempt to code",
                    NA)))))))))))))))
  }
  return(codes)
}
get_008_type_of_date(tests_008)
get_008_type_of_date(tests_008, translate = FALSE)

##### WHY DO SOME HAVE "U" AT THE END??!!?!
# 7-10 - date1
get_008_date1 <- function(x){
  return(stringr::str_sub(x, 8, 11))
}
get_008_date1(tests_008)

# 11-14 - date2
get_008_date2 <- function(x){
  return(stringr::str_sub(x, 12, 15))
}
get_008_date2(tests_008)

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


get_18_to_21 <- function(x){
  return(stringr::str_sub(x, 19, 22))
}





get_008_info  <- function(x, translate=TRUE){
  date_entered <- get_008_date_entered(x)
  type_of_date <- get_008_type_of_date(x, translate=translate)
  date1        <- get_008_date1(x)
  date2        <- get_008_date2(x)
  pub_place    <- get_008_pub_place(x, translate=translate)
  language     <- get_008_language(x, translate=translate)
  eighteen     <- get_18_to_21(x)   ###
  return(data.table::data.table(date_entered=date_entered,
                                type_of_date=type_of_date,
                                date1=date1,
                                date2=date2,
                                pub_place=pub_place,
                                language=language,
                                eighteen=eighteen))
}
get_008_info(tests_008)
get_008_info(tests_008, translate=FALSE)


#########################################################
#########################################################
#########################################################
nypl008s <- readLines(con="~/Desktop/get-leaders/some-008s.txt")
nypl008s <- data.table(oh08=nypl008s)
oh08sinfo <- get_008_info(nypl008s[, oh08])

nyplfull008s <- cbind(nypl008s, oh08sinfo)

nyplfull008s[, .N, type_of_date][order(-N)]

#########################################################
#########################################################


#########################################################
#########################################################
#########################################################

## 007

get_007_category <- function(x, translate=TRUE){
  codes <- stringr::str_sub(x, 0, 1)
  if(translate){
    codes <- ifelse(codes=="a",
                    "Map",
             ifelse(codes=="c",
                    "Electronic resource",
             ifelse(codes=="d",
                    "Globe",
             ifelse(codes=="f",
                    "Tactile material",
             ifelse(codes=="g",
                    "Projected graphic",
             ifelse(codes=="h",
                    "Microform",
             ifelse(codes=="k",
                    "Nonprojected graphic",
             ifelse(codes=="m",
                    "Motion picture",
             ifelse(codes=="o",
                    "Kit",
             ifelse(codes=="q",
                    "Notated music",
             ifelse(codes=="r",
                    "Remote-sensing image",
             ifelse(codes=="s",
                    "Sound recording",
             ifelse(codes=="t",
                    "Text",
             ifelse(codes=="v",
                    "Videorecording",
             ifelse(codes=="z",
                    "Unspecified",
                    NA)))))))))))))))
  }
  return(codes)
}

#########################################################
#########################################################
#########################################################




#########################################################
#########################################################
#########################################################

nyplboth <- fread("~/Desktop/get-leaders/both.txt", colClasses="character",
                     sep="\t", header=FALSE, fill=TRUE)
setnames(nyplboth, c("leader", "oh07", "oh08"))

leaderinfo <- get_leader_info(nyplboth[, leader])
newnames <- names(leaderinfo) %>% sprintf("leader_%s", .)
setnames(leaderinfo, newnames)

oh08info <- get_008_info(nyplboth[, oh08])
newnames <- names(oh08info) %>% sprintf("oh08_%s", .)
setnames(oh08info, newnames)

nyplboth[, oh07cat:=get_007_category(oh07)]
nyplboth <- nyplboth %>% cbind(leaderinfo) %>% cbind(oh08info)

nyplboth %>% fwrite("nypl-both.txt", sep="\t")
#########################################################
#########################################################




#########################################################
#########################################################
#########################################################




nyplfullleader[, .N, record_type][order(-N)]
utleaderinfo <- get_leader_info(nyplleaders[, leader], translate=FALSE)
utnyplfullleader <- cbind(nyplleaders, utleaderinfo)
utnyplfullleader[, .N, record_type][order(-N)]


nypl008s <- fread("~/Desktop/recap-dashboard/computed-data/oh08s/nypl-oh08s.txt",
                  header=FALSE)
setnames(nypl008s, c("bibid", "oh08"))
oh08info <- get_008_info(nypl008s[, oh08])

nyplfull008 <- cbind(nypl008s, oh08info)

nyplfull008[is.na(language), .N]

nyplfull008[, .N, language][order(-N)]
nyplfull008[, .N, pub_place][order(-N)]
nyplfull008[!is.na(as.numeric(date1)), as.numeric(date1)] %>% density %>% plot(xlim=c(1500, 2100))
#########################################################
#########################################################
#########################################################

