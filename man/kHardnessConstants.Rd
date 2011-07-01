\name{kHardnessConstants}
\alias{kHardnessConstants}
\docType{data}
\title{
Table of Constants for OR DEQ Metals Criteria
}
\description{
These are the necessary constants for calculating Oregon DEQ metals criteria
using either Table 33a or Table 20.
}
\usage{data(kHardnessConstants)}
\format{
  A data frame with 56 observations on the following 7 variables.
  \describe{
    \item{\code{table}}{a factor with levels \code{table20} \code{table33a}}
    \item{\code{analyte}}{a factor with levels \code{cadmium} \code{cadmium, dissolved} \code{chromium} \code{chromium, dissolved} \code{copper} \code{copper, dissolved} \code{lead} \code{lead, dissolved} \code{nickel} \code{nickel, dissolved} \code{silver} \code{silver, dissolved} \code{zinc} \code{zinc, dissolved}}
    \item{\code{toxicity}}{a factor with levels \code{acute} \code{chronic}}
    \item{\code{m}}{a numeric vector}
    \item{\code{b}}{a numeric vector}
    \item{\code{cf.b}}{a numeric vector}
    \item{\code{cf.m}}{a numeric vector}
  }
}

\source{
http://www.deq.state.or.us/wq/standards/toxics.htm
}
\examples{
data(kHardnessConstants)
str(kHardnessConstants)
}
\keyword{datasets}
