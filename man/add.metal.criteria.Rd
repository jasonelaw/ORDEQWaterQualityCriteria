\name{add.metal.criteria}
\alias{add.metal.criteria}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
  A function to calculate metals criteria for water quality data in long format, 
  i.e. in a sample, analyte, result format.  This function will calculate a
  criteria for any analyte name that matches a name in KHardnessConstants.
}
\usage{
add.metal.criteria(sample, analyte, result, hardness.name = 'hardness, total', 
                   toxicity = c('acute', 'chronic'),
                   table = c('table33a', 'table20'))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
  a vector of sample identifiers
  }
  \item{analyte}{
  A vector of analyte names (including hardness).  Any for which a result will
  be calculated must match the names in KHardnessConstants.  If a name does not
  match, the criteria returned will be NA.
  }
  \item{result}{
  A vector of concentrations for the analytes.  These are necessary to extract
  the hardness values
  }
  \item{hardness.name}{
  The name of the hardness value in the analyte vector.
  }
  \item{toxicity}{
  acute or chronic
}
  \item{table}{
  which table of standards to use. See OR DEQ web site for difference.
}
}

\value{
  a vector of water quality critera, possibly with missing values for analytes
  that are not metals.
}
\author{
Jason Law <jason.law@portlandoregon.gov>
}

\examples{
 data <- expand.grid(analyte = c('copper', 'lead', 'benzene', 'hardness'), 
                     sample  = 1:4)
 data$result <- runif(16, 0, 10)
 add.metal.criteria(data$sample, data$analyte, data$result, 'hardness')
 
}

