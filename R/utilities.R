
## various utility functions


# --------------------------------------------------------------- #

#' Delete columns in a data.table
#'
#' Takes a data.table and a quoted sequence of column names
#' and removes the specified column names from the data.tablee
#'
#' @import data.table
#'
#' @param DT A data.table
#' @param ... arbitrary number of column names in quotes
#'
#' @return Returns data.table with those columns removed
#'
#' @examples
#' mt <- as.data.table(mtcars)
#' dt_del_cols(mt, "cyl", "disp", "hp")
#'
#' @export
dt_del_cols <- function(DT, ...){
  if(!("data.table" %in% class(DT)))
    stop("DT must be a data.table object")
  cols <- c(...)
  DT[, (cols):=NULL]
}


# --------------------------------------------------------------- #

#' Keep columns in a data.table
#'
#' Takes a data.table and a quoted sequence of column names
#' and removes all columns but the ones specified
#'
#' @import data.table
#'
#' @param DT A data.table
#' @param ... arbitrary number of column names in quotes
#'
#' @return Returns data.table with only those columns
#'
#' @examples
#' mt <- as.data.table(mtcars)
#' dt_keep_cols(mt, "mpg", "am", "gear", "carb")
#'
#' @export
dt_keep_cols <- function(DT, ...){
  if(!("data.table" %in% class(DT)))
    stop("DT must be a data.table object")
  cols <- c(...)
  these <- setdiff(names(DT), cols)
  dt_del_cols(DT, these)
}


# --------------------------------------------------------------- #

### INTERNAL FUNCTION
## examples:
# iris_dt <- as.data.table(iris)
# dt_pivot(iris_dt, "Species", mean(Petal.Length), value.name="mean_petal_length")
# dt_pivot(iris_dt, "Species", sum(Petal.Length), value.name="mean_petal_length",
#          percent.cutoff=20)
#
# # breaks
# dt_pivot(iris_dt, "Species", .N, percent.name="tmp", value.name="tmp")
# dt_pivot(iris_dt, "Sp", .N)
# dt_pivot(iris_dt, "Species", sum(nothing))
dt_pivot <- function(DT, theby, theexp, percent.cutoff=0, value.name="value",
                     percent.name="percent_count"){
  if(value.name == percent.name)
    stop("value name and percent name cannot be the same")

  val <- percent <- . <- NULL

  inexp <- substitute(theexp)
  strexp <- as.character(deparse(inexp))
  template <- sprintf("DT[, .(val=%s), theby][order(-val)]", strexp)
  tmp <- eval(parse(text=template))
  thetotal <- tmp[, sum(val)]
  tmp[, percent:=round(val/thetotal*100, 2)]
  tmp[percent<percent.cutoff, (theby):="OTHER"]
  tmp <- tmp[, .(val=sum(val), percent=sum(percent)), theby]
  tmp2 <- tmp[1,]
  tmp2[1,1] <- "TOTAL"
  tmp2[1,2] <- thetotal
  tmp2[1,3] <- 100
  tmp3 <- rbind(tmp, tmp2)
  setnames(tmp3, "val", value.name)
  setnames(tmp3, "percent", percent.name)
  tmp3
}

# --------------------------------------------------------------- #

#' Group by, count, and percent count in a data.table
#'
#' This function takes a (quoted) column to group by, counts the
#' number of occurrences, sorts descending, and adds the percent
#' of occurrences for each level of the grouped-by column.
#'
#' For long-tailed count distributions, a cutoff on the percent can be
#' placed; percent of counts lower than this percent will be grouped
#' into a category called "OTHER".
#' The percent is a number out of 100
#'
#' The final row is a total count
#'
#' The quoted group-by variable must be a character or factor
#'
#' @import data.table
#'
#' @param DT The data.table object to operate on
#' @param group_by_this A quoted column to group by
#' @param percent.cutoff A percent (out of 100) such that all
#'        the count percents lower than this number will be
#'        grouped into "OTHER" in the returned data.table
#'        (default is 0)
#'
#' @return Returns a data.table with three columns:
#'         the grouped-by column, a count column, and a
#'         percent column (out of 100) to two decimal places
#'
#' @examples
#'
#' iris_dt <- as.data.table(iris)
#' dt_counts_and_percents(iris_dt, "Species")
#
#' mt <- as.data.table(mtcars)
#' mt[, cyl:=factor(cyl)]
#' dt_counts_and_percents(mt, "cyl")
#' dt_counts_and_percents(mt, "cyl", percent.cutoff=25)
#'
#' @export
dt_counts_and_percents <- function(DT, group_by_this, percent.cutoff=0){
  if(!("data.table" %in% class(DT)))
    stop("DT must be a data.table object")
  dt_pivot(DT, group_by_this, .N, percent.cutoff=percent.cutoff,
           value.name="count", percent.name="percent")
}


# --------------------------------------------------------------- #

#' Takes a data.frame and returns cleaned column names
#'
#' This function takes a data.frame, extracts the column names,
#' and returns a vector of those column names but cleaned and
#' stripped of potentially troublesome names
#'
#' All space/whitespace characters are replaced with underscores,
#' as are all characters not from A-Z, a-z, an underscore, or a digit
#'
#' @param dat A data.frame
#'
#' @return Returns a vector of cleaned names
#'
#' @examples
#' ejemplo <- iris
#' names(ejemplo) <- c("Sepal Length", "Sepal@Width", "Petal	Length",
#'                     "Petal\\nWidth", "Spêcies")
# get_clean_names(ejemplo)
#' # c("Sepal_Length" "Sepal_Width"  "PetalLength"  "Petal_nWidth" "Sp_cies")
#'
#' @export
get_clean_names <- function(dat){
  thenames <- names(dat)
  nospace <- stringr::str_replace_all(thenames, "\\s+", "_")
  nobad <- stringr::str_replace_all(nospace, "[^_A-Za-z1-9]", "_")
  return(nobad)
}


# --------------------------------------------------------------- #

#' Takes a data.table and returns
#'
#' This function takes a data.table, and returns the same data.table
#' with column names that are cleaned and stripped of potentially
#' troublesome names
#'
#' All space/whitespace characters are replaced with underscores,
#' as are all characters not from A-Z, a-z, an underscore, or a digit
#'
#' @import data.table
#'
#' @param DT a data.table
#'
#' @return Returns the data.table but with cleaned names
#'
#' @seealso \code{\link{get_clean_names}}
#'
#' @examples
#'
#' ejemplo <- as.data.table(iris)
#' setnames(ejemplo, c("Sepal Length", "Sepal@Width", "Petal	Length",
#'                     "Petal\\nWidth", "Spêcies"))
#' dt_set_clean_names(ejemplo)
#'
#' @export
dt_set_clean_names <- function(DT){
  if(!("data.table" %in% class(DT)))
    stop("DT must be a data.table object")
  setnames(DT, get_clean_names(DT))
}


# --------------------------------------------------------------- #

#' Return the percentage of non-NA instances in a data.table column
#'
#' This function takes a data.table and a quoted column name and
#' returns the percentage of the data in the column that is not
#' NA.
#' The percent is out of 100 and contains up to two decimal places
#'
#' @import data.table
#'
#' @param DT A data.table object
#' @param acolumn a quoted column name
#'
#' @return Returns percentage of non-NA instances in column
#'
#' @seealso \code{\link{is.na}}
#'
#' @examples
#'
#' mt <- as.data.table(mtcars)
#' mt[mpg<16, mpg:=NA]
#' dt_percent_not_na(mt, "mpg")         # 68.75
#'
#' @export
dt_percent_not_na <- function(DT, acolumn){
  if(!("data.table" %in% class(DT)))
    stop("DT must be a data.table object")
  DT[, round(100*sum(!is.na(get(acolumn)))/.N, 2)]
}


# --------------------------------------------------------------- #

#' Add string to all column names in a data.table
#'
#' Takes a data.table and a string. The supplied string will
#' be added to end of the each column's name. If \code{prefix}
#' is \code{TRUE}, the string is added to the beginning, instead.
#'
#' @import data.table
#'
#' @param DT A data.table
#' @param astring A string to add to each column name
#' @param prefix A logical indicating whether the string should be added
#'               to the beginning of each column name, instead of the end.
#'               (default is \code{FALSE})
#' @param exclude A quoted vector or column names to exclude from renaming.
#'        Cannot co-exist with \code{include}
#' @param include A quoted vector or column names. Changes names of only
#'        these columns. Cannot co-exist with \code{exclude}
#'
#' @return Returns data.table with string appended or prefixed
#'
#' @examples
#' DT <- as.data.table(iris)
#'
#' dt_add_to_col_names(DT, "_post")
#' names(DT)
#' # [1] "Sepal.Length_post" "Sepal.Width_post"  "Petal.Length_post"
#' # [4] "Petal.Width_post"  "Species_post"
#'
#'  DT <- as.data.table(iris)
#' dt_add_to_col_names(DT, "pre_", prefix=TRUE)
#' names(DT)
#' # [1] "pre_Sepal.Length" "pre_Sepal.Width"  "pre_Petal.Length" "pre_Petal.Width"
#' # [5] "pre_Species"
#'
#' DT <- as.data.table(iris)
#' dt_add_to_col_names(DT, "_post", exclude="Species")
#' names(DT)
#' # [1] "Sepal.Length_post" "Sepal.Width_post"  "Petal.Length_post"
#' # [4] "Petal.Width_post"  "Species"
#'
#' @export
dt_add_to_col_names <- function(DT, astring, prefix=FALSE,
                                exclude=NULL, include=NULL){
  if(!("data.table" %chin% class(DT)))
    stop("DT must be a data.table")
  if(!is.null(exclude) && !is.null(include))
    stop("cannot have both 'include' and 'exclude' parameters at the same time")

  here <- old_names <- new_names <- NULL

  tmp <- data.table(old_names=names(DT), here=TRUE)

  if(!is.null(exclude) || !is.null(include)){
    check <- tmp[c(exclude, include), on="old_names"]
    if(check[is.na(here), .N]>0){
      missingnames <- check[is.na(here), old_names]
      missingnames <- paste(sprintf('"%s"', missingnames), collapse=', ')
      warning(sprintf("Columns (%s) are missing from supplied data.table",
                      missingnames))
    }
  }

  if(prefix)
    tmp[,new_names:=sprintf("%s%s", astring, old_names)]
  else
    tmp[,new_names:=sprintf("%s%s", old_names, astring)]

  if(!is.null(exclude))
    tmp[old_names %chin% exclude, new_names:=old_names]
  if(!is.null(include))
    tmp[!(old_names %chin% include), new_names:=old_names]

  setnames(DT, tmp[,new_names])
}
