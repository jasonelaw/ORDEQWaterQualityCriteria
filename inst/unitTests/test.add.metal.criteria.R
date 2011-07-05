# TODO: Add comment
# Author: JLAW
# Date: Oct 18, 2010
# Purpose:
# 
# 
###############################################################################

# Library and source statements
test.add.metal.criteria <- function(){
  library(reshape)
  #library(ORDEQWaterQualityCriteria)

  # Functions
  AddCriteria <- function(x){
    x$limit <- CalculateMetalCriteria(hardness = x$hardness, 
                                      analyte = x$analyte, 
                                      use.toxicity = unique(as.character(x$toxicity)),
                                      use.table = unique(as.character(x$table)))
    x
  }
  # Constants
  kAnalytes <- sort(c("copper", "copper, dissolved", "lead", "lead, dissolved",
                      "silver", "silver, dissolved", "nickel", "nickel, dissolved",
                      "zinc", "zinc, dissolved", "cadmium","cadmium, dissolved", 
                      "chromium", "chromium, dissolved"))
  kHardness <- c(25, 50, 75, 100)
  kToxicity <- c('chronic', 'acute')
  kTable    <- c('table20', 'table33a')

# Calculate all
  all.combs <- expand.grid(analyte  = kAnalytes, 
                           hardness = kHardness,
                           toxicity = kToxicity, 
                           table    = kTable)
  limits <- ddply(all.combs, .(toxicity, table), AddCriteria)
  limits$dissolved <- as.factor(grepl('dissolved', limits$analyte))
  levels(limits$dissolved) <- list('dissolved' = 'TRUE', 'total' = 'FALSE')
  limits$analyte <- gsub(', dissolved', '', limits$analyte)
  check <- read.csv('check.csv')
  ans <- merge(check, limits, by = c('analyte', 'hardness', 'toxicity', 'table', 'dissolved'))
  checkEquals(ans$limit.x, ans$limit.y)
}



