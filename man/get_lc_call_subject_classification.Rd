% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call-numbers-and-subject.R
\name{get_lc_call_subject_classification}
\alias{get_lc_call_subject_classification}
\title{Conversion from Library of Congress Call number to subject
classification}
\usage{
get_lc_call_subject_classification(
  x,
  subclassification = FALSE,
  already.parsed = FALSE,
  allow.bare = TRUE
)
}
\arguments{
\item{x}{A Library of Congress call number (string)}

\item{subclassification}{A logical indicating whether the letters of
call number past the first should be used to match to
a subject subclassification (default is \code{FALSE})}

\item{already.parsed}{Skips the extraction of the subject letters
and jumps to the subject matching (default is \code{FALSE})}

\item{allow.bare}{A logical indicating whether an LC Call with only
the letters should be considered valid
(default is \code{TRUE})}
}
\value{
Returns either the broad (top-level) subject classification
        description or the second level subject subclassification
        description. Returns "NA" if no subject could not be matched
        or call number is invalid
}
\description{
Takes a string representation of a Library of Congress
call number and returns either the broad subject
classification description (default) based on the first
letter, or a second-level subclassification
description based on the all the letters
}
\examples{

get_lc_call_subject_classification("ND 237.S18 $b S87 1997")
# Fine Arts

get_lc_call_subject_classification("ND 237.S18 $b S87 1997", subclassification=TRUE)
# Painting

get_lc_call_subject_classification("PQ2246.M3")
# Language and Literature

get_lc_call_subject_classification("PQ2246.M3",
                                   subclassification=TRUE)
# "French, Italian, Spanish, and Portuguese literature"

get_lc_call_subject_classification("PQ2246.M3", already.parsed=TRUE)
# NA

get_lc_call_subject_classification("PQ", already.parsed=TRUE,
                                   subclassification=TRUE)
# "French, Italian, Spanish, and Portuguese literature"

# vectorized
get_lc_call_subject_classification(c("ND 237", "\\\\\\\\$a ND 2", "PQ2246.M3"),
                                   subclassification=TRUE)
# c("Painting", NA, "French, Italian, Spanish, and Portuguese literature")


}
