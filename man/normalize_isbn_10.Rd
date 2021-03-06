% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bibcodes.R
\name{normalize_isbn_10}
\alias{normalize_isbn_10}
\title{Attempt to enforce validity and canonical form to ISBN 10}
\usage{
normalize_isbn_10(x, aggressive = TRUE, convert.to.isbn.13 = FALSE)
}
\arguments{
\item{x}{A string}

\item{aggressive}{A logical indicating whether aggressive measures
should be taken to try to get the "ISBN 10"
into a valid form. See "Details" for more info
(default is \code{TRUE})}

\item{convert.to.isbn.13}{A logical indicating whether the ISBN 10
should be converted into an ISBN 13
(default is \code{FALSE})}
}
\value{
Returns valid ISBN 10 if possible, NA if not
}
\description{
Takes a string representation of an ISBN 10. Strips all non-digit
and non-"X" characters and checks if it is valid (whether the
check digit works out, etc). User can specify whether "aggressive"
measures should be taken to salvage the malformed ISBN 10 string.
}
\details{
If \code{aggressive} is TRUE, aggressive measures are taken to
try to salvage the malformed ISBN 10 string. If the ISBN 10, for example,
is 9 digits, and either adding an "X" to the end, or leading "0"s fix it,
this function will return the salvaged ISBN 10. If the ISBN 10 has
garbage digits/characters in the front and has an "X" check digit,
it will return the salvaged ISBN 10.
}
\examples{

normalize_isbn_10("012491540x")                    # "012491540X"
normalize_isbn_10("012491540x xe32ea")             # "012491540X"
normalize_isbn_10("012491540x", convert.to.isbn.13=TRUE)
# "9780124915404"
normalize_isbn_10("513213012491540x")              # "012491540X"

}
\seealso{
\code{\link{normalize_isbn}} \code{\link{normalize_isbn_13}}
}
