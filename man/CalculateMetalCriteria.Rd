\name{CalculateMetalCriteria}
\alias{CalculateMetalCriteria}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate metals criteria for a vector for hardness concentrations and analyte names
}
\description{
Calculate metals criteria for a vector for hardness concentrations and analyte names
}
\usage{
CalculateMetalCriteria(hardness, analyte, use.toxicity = c("acute", "chronic"), use.table = c("table33a", "table20"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hardness}{
A vector of hardness values
}
  \item{analyte}{
A vector of metal analyte names
}
  \item{use.toxicity}{
whether to use acute or chronic toxicity
}
  \item{use.table}{
whether to use table 20 or table 33a standards.
}
}
\value{
A vector of hardness dependent metals criteria
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jason Law <jason.law@portlandoregon.gov
}
