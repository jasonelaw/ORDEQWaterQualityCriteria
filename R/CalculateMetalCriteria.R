CalculateMetalCriteria <-
function(hardness, analyte, 
         use.toxicity = c('acute', 'chronic'),
         use.table    = c('table33a', 'table20')){
  # Calculates metal limits 
  data(kHardnessConstants)
  use.toxicity  <- match.arg(use.toxicity)
  use.table     <- match.arg(use.table)
  n         <- length(hardness)
  use.rows  <- kHardnessConstants$toxicity == use.toxicity & kHardnessConstants$table == use.table
  analyte.constants <- subset(x = kHardnessConstants, subset = use.rows)
  i <- match(analyte, analyte.constants$analyte)
  expanded.analyte.constants <- analyte.constants[i,]
  hardness <- pmin(400, pmax(25, hardness))
  f <- function(hardness, m, b, cf.b, cf.m){
    correction.factor <- cf.b - log(hardness) * cf.m
    exp(m * log(hardness) + b) * correction.factor
  }
  crit <- f(hardness = hardness, 
            m        = expanded.analyte.constants$m, 
            b        = expanded.analyte.constants$b, 
            cf.b     = expanded.analyte.constants$cf.b, 
            cf.m     = expanded.analyte.constants$cf.m)
  return(crit)
}

