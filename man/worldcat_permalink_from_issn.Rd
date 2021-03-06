% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/the-web.R
\name{worldcat_permalink_from_issn}
\alias{worldcat_permalink_from_issn}
\title{Get WorldCat catalog permalinks from ISSNs}
\usage{
worldcat_permalink_from_issn(x, normalize = TRUE)
}
\arguments{
\item{x}{A string (or vector of strings) of ISSNs}

\item{normalize}{a logical indicating whether the ISSNs should be
normalized prior to creating the permalink
(default is \code{TRUE})}
}
\value{
Worldcat permalinks using ISSNs.
}
\description{
Takes a string representation of ISSNs. Returns permalinks
to the WorldCat catalog entries using those ISSNs.
}
\details{
If normalize=TRUE and the ISSN is invalid, the permalink is NA.
If normalize=FALSE, the permalink may be invalid. No validity
check on the URL is performed
}
\examples{
worldcat_permalink_from_issn("0968-1221")   # http://www.worldcat.org/issn/0968-1221

worldcat_permalink_from_issn("2434-561X")   # http://www.worldcat.org/issn/2434561X

# vectorized
worldcat_permalink_from_issn(c("0968-1221", NA, "2434-561X"))

}
