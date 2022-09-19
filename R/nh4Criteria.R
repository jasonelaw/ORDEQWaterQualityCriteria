#' Calculate Oregon DEQ water quality criteria for ammonia
#' 
#' This function calculates the Oregon DEQ pH and temperature dependent
#' ammonia criteria. Tables of values and formulas can be found at 
#' \url{http://www.deq.state.or.us/wq/standards/docs/tables303140.pdf}.
#' 
#' Table 30A acute, salmonids are a designated use.
#' Table 30b acute, salmonids are not a designated use.
#' Table 30c chronic, fish and aquatic life are a designated use.
#' 
#' @param pH a water pH for the criteria
#' @param Cel a water temperature in degrees Celsius
#' @param toxicity the type of criteria, \code{toxicity = 'acute'} 
#' gives the acute criteria (CMC) while \code{toxicity = 'chronic'} gives the chronic criteria (CCC)
#' @param salmonids logical, whether the criteria should be for streams with salmonid 
#' beneficial use (Table 30A). The default is \code{FALSE}. Ignored when \code{toxicity = 'chronic'}.
#' @return a numeric vector of water quality criteria
#' @export
nh4Criteria <- function(pH, Cel, toxicity = c('chronic', 'acute'), salmonids = FALSE){
  stopifnot(pH > 0, pH < 14, Cel > 0, Cel < 35)
  toxicity <- match.arg(toxicity)
  f.pH <- function(x, a, b, c){
    ((a/(1 + 10^(c - x))) + (b/(1 + 10^(x - c))))
  }
  f.T <- function(x, a, b){
    a * 10^(b * (20 - x))
  }
  if (identical(toxicity, 'chronic')){
    0.8876 * 
      f.pH(pH, 0.0278, 1.1994, 7.688) * 
      f.T(pmax(Cel, 7), 2.126, 0.028)
  } else if(salmonids){
    pmin(0.7249 *
           f.pH(pH, 0.0114, 1.6181, 7.204) *
           f.T(Cel, 23.12, 0.036),
         f.pH(pH, 0.275, 39, 7.204))
  } else {
    0.7249 *
      f.pH(pH, 0.0114, 1.6181, 7.204) *
      pmin(51.93, f.T(Cel, 23.12, 0.036))
  }
}