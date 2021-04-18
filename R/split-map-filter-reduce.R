

# --------------------------------------------------------------- #

#' Return first element of vector
#'
#' Takes a vector and returns the first element
#' Equivalent to Lisp's \code{car} function
#'
#' Originally for use as a reduction function in \code{split_map_filter_reduce}
#'
#' @param x A vector
#'
#' @return Returns first element of vector
#'
#' @seealso \code{\link{split_map_filter_reduce}}
#'
#' @examples
#' car(c(8, 6, 7, 5, 3, 0, 9))      # 8
#' mt <- as.data.table(mtcars)
#' dt_del_cols(mt, "cyl", "disp", "hp")
#'
#' @export
car <- function(x){
  return(x[[1]])
}

# --------------------------------------------------------------- #

#' Remove duplicate elements and NAs from a vector
#'
#' Takes a vector and returns the same vector without duplicate
#' elements and without NA values
#'
#' Can be used as a filtering function in \code{split_map_filter_reduce}
#'
#' @param x A vector
#'
#' @return Returns vector with duplicates and NAs removed
#'
#' @seealso \code{\link{split_map_filter_reduce}}
#'
#' @examples
#'
#' remove_duplicates_and_nas(c(8, 6, 7, 5, 3, 0, 9, 6, NA, 3))
#' # 8 6 7 5 3 0 9
#'
#' remove_duplicates_and_nas(c(NA, NA))
#' # NA
#'
#' @export
remove_duplicates_and_nas <- function(x){
  x <- x[!is.na(x) & !duplicated(x)]
  if(!length(x))
    return(NA)
  return(x)
}

# --------------------------------------------------------------- #

#' Return a function that will combine/contatenate a vector
#'
#' This function takes and optional separator, and returns
#' a function that takes a vector and \code{paste}s the
#' elements of that vector together
#'
#' Can be used as a reduction function in \code{split_map_filter_reduce}
#'
#' @param sep A character to use in between the elements
#'            (default is a semicolon character)
#'
#' @return Returns a closure/function
#'
#' @seealso \code{\link{split_map_filter_reduce}}
#' @seealso \code{\link{paste}}
#'
#' @examples
#'
#' lambda <- recombine_with_sep_closure()
#' lambda(c(8, 6, 7))                               # "8;6;7"
#'
#' # directly
#' recombine_with_sep_closure()(c(8,6,7))           #  "8;6;7"
#
#' lambda <- recombine_with_sep_closure(" ")
#' lambda(c("this", "that", NA,"the-other"))        # "this that NA the-other"
#'
#' @export
recombine_with_sep_closure <- function(sep=";"){
  function(x){
    if(length(x)==1 && is.na(x))
      return(NA)
    paste(x, collapse=sep, sep=sep)
  }
}

# --------------------------------------------------------------- #

#' Split, Map, Filter, and Reduce a string vector
#'
#' This function takes a vector of strings, splits those strings
#' on a particular character; string; or regex patters, applies a
#' user-specified function to each sub-element of the now split element,
#' filters those sub-elements using a user-specified function, and, finally,
#' recombines each element's sub-elements using a user specified reduction
#' function.
#'
#' Since this operation cannot be vectorized, if the user specifies
#' a non-zero \code{cl} argument, the workload will be parallelized
#' and \code{cl} many child processes will be spawned to do the work.
#' The package \code{pbapply} will be used to do this.
#'
#' See \code{examples} for more information and ideas on why this
#' might be useful for, as an example, batch normalizing ISBNs that,
#' for each bibliographic record, is separated by a semicolon
#'
#' @import pbapply
#'
#' @param x A vector of strings
#' @param sep A character to use containing a character, string, or
#'            regular expression pattern to split each element by.
#'            If \code{fixed=TRUE}, the separator will be used exactly;
#'            If not, a Perl-compatible regular expression can be used
#'            (default is ";")
#' @param fixed Should it be split by a fixed string/character or
#'              a regular expression (default is \code{TRUE})
#' @param mapfun A vectorized function that will be applied to the
#'               sub-elements (after splitting) of each element in x
#'               (default is \code{identity} which would leave the
#'               sub-elements unchanged)
#' @param filterfun A vectorized function that, when given a vector
#'                  returns the same vector with un-wanted elements
#'                  removed
#'                  (default is \code{identity} which would not remove
#'                  any sub-elements)
#' @param reduxfun A vectorized function that, when given a vector,
#'                 will combine all of it's elements into one value
#'                 (default is \code{car}, which would return the first
#'                 element only)
#' @param cl An integer to indicate the number of child processes
#'           should be used to parallelize the work-load. If 0,
#'           the workload will not be parallelized. Can also
#'           take a cluster object created by 'makeCluster'
#'           (default is 0)
#'
#' @return Returns a vector
#'
#' @seealso \code{\link{car}}
#' @seealso \code{\link{remove_duplicates_and_nas}}
#' @seealso \code{\link{recombine_with_sep_closure}}
#'
#' @examples
#'
#' someisbns <- c("9782711875177;garbage-isbn;2711875172;2844268900",
#'                "1861897952; 978-1-86189-795-4")
#'
#' # will return only the first ISBN for each record
#' split_map_filter_reduce(someisbns)
#' # "9782711875177" "1861897952"
#'
#' # will return only the first ISBN for each record, after normalizing
#' # each ISBN
#' split_map_filter_reduce(someisbns, mapfun=function(x){normalize_isbn(x, convert.to.isbn.13=TRUE)})
#' # "9782711875177" "9781861897954"
#'
#' # will return all ISBNs, for each record, separated by a semicolon
#' # after applying normalize_isbn to each ISBN
#' # note the duplicates introduced after normalization occurs
#' split_map_filter_reduce(someisbns, mapfun=function(x){normalize_isbn(x, convert.to.isbn.13=TRUE)},
#'                         reduxfun=recombine_with_sep_closure())
#' # "9782711875177;NA;9782711875177;9782844268907" "9781861897954;9781861897954"
#'
#' # After splitting each items ISBN list by semicolon, this runs
#' # normalize_isbn in each of them. Duplicates are produced when
#' # an ISBN 10 converts to an ISBN 13 that is already in the ISBN
#' # list for the item. NAs are produced when an ISBN fails to normalize.
#' # Then, all duplicates and NAs are removed. Finally, the remaining
#' # ISBNs, for each record, are pasted together using a space as a separator
#' split_map_filter_reduce(someisbns, mapfun=function(x){normalize_isbn(x, convert.to.isbn.13=TRUE)},
#'                         filterfun=remove_duplicates_and_nas,
#'                         reduxfun=recombine_with_sep_closure(" "))
#' # "9782711875177 9782844268907" "9781861897954"
#'
#' @export
split_map_filter_reduce <- function(x, sep=";", fixed=TRUE,
                                    mapfun=identity, filterfun=identity,
                                    reduxfun=car, cl=0){
  # function to process just one element of x
  do_just_one <- function(astring, sep=";", fixed=FALSE,
                          mapfun=identity, filterfun=identity,
                          reduxfun=car){
    thesplit <- unlist(strsplit(astring, sep, fixed=fixed, perl=!fixed))
    thesplit <- mapfun(thesplit)
    thesplit <- filterfun(thesplit)
    thesplit <- reduxfun(thesplit)
    thesplit
  }
  if(cl==0){
    ret <- sapply(x, function(y) do_just_one(y, sep=sep, fixed=fixed,
                                             mapfun=mapfun, filterfun=filterfun,
                                             reduxfun=reduxfun))
  } else {
    ret <- pbapply::pbsapply(x, function(y) do_just_one(y, sep=sep, fixed=fixed,
                                                        mapfun=mapfun,
                                                        filterfun=filterfun,
                                                        reduxfun=reduxfun),
                             USE.NAMES=FALSE, cl=cl)
  }
  ret <- unlist(ret)
  names(ret) <- NULL
  ret
}



