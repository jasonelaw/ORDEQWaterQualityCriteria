#' Calculate metals criteria for a vector for hardness concentrations and
#' analyte names
#' 
#' Calculate metals criteria for a vector for hardness concentrations and
#' analyte names
#' 
#' 
#' @param hardness A vector of hardness values
#' @param analyte A vector of metal analyte names
#' @param toxicity whether to use acute or chronic toxicity
#' @return A vector of hardness dependent metals criteria
#' @author Jason Law <jason.law@@portlandoregon.gov>
#' @export
metalCriteria <- function(hardness, analyte, toxicity = c('acute', 'chronic')){
  stopifnot(hardness > 0, analyte %in% kHardnessConstants$analyte)
  if (any(analyte %in% c('Cd', 'Cu'))){
    warning('Cadmium acute and copper (acute and chronic) criteria are expressed as total recoverable values. These criteria
             should not be compared to dissolved sample result.')
    }
  toxicity  <- match.arg(toxicity)
  #n         <- length(hardness)
  const <- kHardnessConstants[kHardnessConstants$toxicity == toxicity, ]
  i <- match(analyte, const$analyte)
  const <- const[i,]
  #hardness <- pmin(400, pmax(25, hardness))
  f <- function(hardness, m, b, cf.b, cf.m){
    exp(m * log(hardness) + b) * (cf.b - log(hardness) * cf.m)
  }
  crit <- f(hardness = hardness,
            m        = const$m,
            b        = const$b, 
            cf.b     = const$cf.b,
            cf.m     = const$cf.m)
  return(crit)
}
