

#' Set special libbib attribute on object
#'
#' Takes an object, attribute name, and a value and sets a special
#' libbib attribute by reference
#'
#' @import data.table
#'
#' @param x An object to set the attribute on
#' @param type The name of the attribute to set. \code{lb.} will be appended
#'             to this attribute name. For example, if this argument is
#'             \code{source}, and attribute called \code{lb.source} will
#'             be set on the object with the value specified
#' @param value The value of the attribute
#'
#' @return Nothing, since the object is modified by reference.
#'
#' @examples
#' set_lb_attribute(mtcars, "source", "R built-in dataset")
#'
#' versicolor <- iris[iris$Species=="versicolor", ]
#' set_lb_attribute(versicolor, "note", "modified built-in dataset")
#' attributes(versicolor)$lb.note
#' # [1] "modified built-in dataset"
#'
#' @export
set_lb_attribute <- function(x, type, value){
  setattr(x, sprintf("lb.%s", type), value)
}


#' Set special libbib date attribute on object
#'
#' Takes an object and a date and sets a special attribute, "lb.date"
#' by reference
#'
#' @import data.table
#'
#' @param x An object to set the attribute on
#' @param value Either a value of class \code{Date} or a string in ISO 8601
#'              date format (yyyy-mm-dd) which will be converted into a Date
#'
#' @return Nothing, since the object is modified by reference.
#'
#' @examples
#' set_lb_date(mtcars, "2021-05-08")
#' attributes(mtcars)$lb.date
#' # [1] "2021-05-08
#'
#' set_lb_date(mtcars, Sys.Date())
#'
#' @export
set_lb_date <- function(x, value){
  if("Date" %chin% class(value))
    set_lb_attribute(x, "date", value)
  else
    set_lb_attribute(x, "date", as.Date(value))
}



#' Copy special libbib attributes from one object to another
#'
#' Takes two objects and copies all special libbib attributes
#' (attributes beginning with \code{lb.}) from the first object
#' to the second, by reference.
#'
#' @import data.table
#'
#' @param a The first object (the one with the attributes to copy)
#' @param b The second object (the one to copy those attributes to)
#'
#' @return Nothing, since the object is modified by reference.
#'
#' @examples
#'
#' tmp1 <- "a"
#' set_lb_date(tmp1, "2021-05-08")
#' set_lb_attribute(tmp1, "note", "just an example")
#'
#' tmp2 <- "b"
#' cp_lb_attributes(tmp1, tmp2)
#' attributes(tmp2)$lb.date
#' # [1] "2021-05-08"
#' attributes(tmp2)$lb.note
#' # [1] "just an example"
#'
#' @export
cp_lb_attributes <- function(a, b){
  tmp <- names(attributes(a))
  tmp <- stringr::str_subset(tmp, "^lb\\.")
  invisible(sapply(tmp, function(x) setattr(b, x, attr(a, x))))
}



# un-exported helper function
split_extension <- function(fname){
  if(length(fname)>1) stop("only takes one filename")
  dirpart <- sprintf("%s/", dirname(fname))
  if(dirpart=="./") dirpart <- ""
  basepart <- basename(fname)
  pieces <- stringr::str_split(basepart, "\\.", n=2)
  before_ext <- pieces[[1]][1]
  after_ext <- pieces[[1]][2]
  if(!is.na(after_ext)) after_ext <- sprintf(".%s", after_ext)
  if(is.na(before_ext) & is.na(after_ext)) stop("couldn't split extension")
  c(sprintf("%s%s", dirpart, before_ext), after_ext)
}



# un-exported helper function
fread_plus_helper <- function(fname){
  pieces <- split_extension(fname)
  file.match <- Sys.glob(sprintf("%s*%s", pieces[1], pieces[2]))
  if(length(file.match)==0) stop("no matching filename")
  if(length(file.match)>1){
    tmp <- sprintf("\n  %s", paste(file.match, collapse=", "))
    stop("more than one matching file: ", tmp)
  }
  # will use the native pipe operator when everone uses > R v4.1
  plus.part <- stringr::str_replace(file.match, sprintf("^%s\\D*", pieces[1]), "")
  plus.part <- stringr::str_replace(plus.part,  sprintf("\\D*%s$", pieces[2]), "")

  list(matching_filename=file.match,
       given_filename=fname,
       plus_part=plus.part,
       before_ext=pieces[1],
       after_ext=pieces[2])
}




#' Read a file and set a special libbib date attribute
#'
#' Takes a file name, reads it with \code{data.table::fread}, and
#' sets an attribute called \code{lb.date} with a date extracted
#' from the file name.
#'
#' The file name can be one with a valid ISO 8601 date (yyyy-mm-dd)
#' already in it, or it can be a file name with the date elided.
#'
#' For example, if there is a file you'd like to read on your disk
#' called "iris-2021-05-08.csv", you can call this function with
#' either "iris.csv" or "iris-2021-05-08.csv" as the file name.
#'
#' When you call this function with a file name without an ISO 8601
#' date (e.g. "iris.csv.gz"),  the file name extension ".csv.gz" is
#' removed and the function looks for a file name beginning with
#' "iris", a date, and the file extension. The file extension is
#' considered to be anything after the first period in the base name.
#' For example, if the file name given is "./my.data/iris.csv.gz", the
#' extension is ".csv.gz". This means no period can be present in
#' the base file name (after any directories) with the exception of
#' the file extension.
#'
#' If you call this function with "iris.csv" and there is no file name
#' with an ISO 8601 date appended to that file name on your disk, and
#' \code{allow.fallback.date} is \code{TRUE}, then the \code{lb.date}
#' attribute is set to the current date.
#'
#' @import data.table
#'
#' @param fname The file name to read
#' @param allow.fallback.date A logical indicating whether, if no
#'                            matching file name with a date is found,
#'                            to use today's date as the date attribute.
#'                            Default is \code{TRUE}.
#' @param ... Arbitrary arguments to use with \code{fread}
#'
#' @return A \code{data.table} with an attribute called \code{lb.date} set
#'
#' @examples
#' \dontrun{
#'   # there's a file called "iris-2021-05-08.csv" on disk
#'   dat <- fread_plus_date("iris.csv")
#'   attribute(dat)$lb.date
#'   # [1] "2021-05-08
#'
#'   # can also read the full file name
#'   dat <- fread_plus_date("iris-2021-05-08.csv")
#'   attribute(dat)$lb.date
#'   # [1] "2021-05-08
#' }
#'
#' @export
fread_plus_date <- function(fname, allow.fallback.date=TRUE, ...){
  dat <- NULL
  helper.ret  <- fread_plus_helper(fname)
  already.date <- stringr::str_extract(helper.ret$before_ext,
                                       "\\d{4}-\\d{2}-\\d{2}$")
  if(!is.na(already.date)){
    # already has the date in the filename
    fname.to.use <- fname
    thedate <- as.Date(already.date)
  } else{
    # doesn't already have the date in the filename
    fname.to.use <- helper.ret$matching_filename
    plus_part <- helper.ret$plus_part
    if(plus_part==""){
      if(!allow.fallback.date)
        stop("no date in filename found")
      message("no date in filename found... using today's date")
      thedate <- Sys.Date()
    } else {
      thedate <- as.Date(plus_part)
    }
  }
  dat <- fread(fname.to.use, ...)
  set_lb_attribute(dat, "date", thedate)
  dat
}




#' Write a file with a date appended to the file name.
#'
#' Takes a \code{data.table}, a file name, and writes it with
#' \code{data.table::fwrite}.
#'
#' The supplied file name will be modified to include an ISO 8601 date
#' (yyyy-mm-dd) between the file name and the file extension. Under the
#' default settings, the date used will be from the \code{lb.date}
#' attribute of the supplied \code{data.table}. If there is no \code{lb.date}
#' attribute, the current date will be used, instead.
#'
#' For example, if there is a \code{data.table} with an \code{lb.date}
#' attribute of "2021-05-08", and you supply this function with the file
#' name "iris.csv", the file name actually written to disk will be
#' "iris-2021-05-08.csv". Under the default settings, if there is no
#' \code{lb.date} attribute, but today's date is "2038-01-19", the file
#' name written to disk will be "iris-2038-01-19.csv".
#'
#' The ISO 8601 date is sandwiched between the file name and the file
#' extension. The file extension is considered to be anything after the
#' first period in the base name.
#' For example, if the file name given is "./my.data/iris.csv.gz", the
#' extension is ".csv.gz". This means no period can be present in
#' the base file name (after any directories) with the exception of
#' the file extension.
#'
#' @import data.table
#'
#' @param DT a \code{data.table} to write to disk
#' @param fname The file name to write the \code{data.table} to. The
#'              date will be appended between the file name and its
#'              file extension
#' @param from.attribute A logical indicating whether the date should
#'                       be taken from the \code{lb.date} attribute of
#'                       the \code{data.table}, or whether it should be
#'                       today's date. Default (\code{TRUE}) takes it
#'                       from the \code{lb.date} attribute.
#' @param allow.fallback.date A logical indicating, if there is no
#'                            \code{lb.date} attribute in the supplied
#'                            \code{data.table}, whether it is permissible
#'                            to use today's date, instead.
#'                            Default is \code{TRUE}.
#' @param ... Arbitrary arguments to pass to \code{fwrite}
#'
#' @examples
#' \dontrun{
#'
#' set_lb_date(iris, "2021-05-08")
#' fwrite_plus_date(iris, "iris.csv.gz")
#' # "iris-2021-05-08.csv.gz" is now written to disk
#'
#' }
#'
#' @export
fwrite_plus_date <- function(DT, fname, from.attribute=TRUE,
                             allow.fallback.date=TRUE, ...){
  pieces <- split_extension(fname)
  if(from.attribute){
    tmp <- attr(DT, "lb.date")
    if(is.null(tmp) || is.na(tmp)){
      if(allow.fallback.date){
        message("DT has no usable date attribute... using today's date")
        thedate <- Sys.Date()
      } else {
        stop("DT has no usable date attribute")
      }
    } else{
      thedate <- tmp
    }
  } else {
    thedate <- Sys.Date()
  }
  newfname <- sprintf("%s-%s%s", pieces[1], thedate, pieces[2])
  fwrite(DT, newfname, ...)
}

