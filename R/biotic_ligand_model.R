#'Format data for BLM model
#'
#'Format data for BLM model.
#'@param location a vector of location identifiers
#'@param sample a vector of sample identifiers
#'@param analyte a vector of analyte names: must match name variable of unexported \code{kBLMParams} data.frame
#'@param result a vector of concentrations
#'@param ecoregion the ecoregion to allow regional defaults. Currently not used.
#'@param limit should the output be limited to samples with dissolved copper (or total copper if \code{total = TRUE}).
#'@param total should missing dissolved copper by filled with total copper if available
#'@export
formatForBLM <- function(location, sample, analyte, result, ecoregion = 'Willamette Valley', limit = TRUE, total = FALSE){
  map <- getOption('blm.analyte.map')
  checkmate::assertSubset(ecoregion, kBLMEcoregionDefaults$ecoregion)
  analyte <- factor(as.character(analyte), levels = map, labels = names(map))
  x <- data.frame(location, sample, analyte, result, ecoregion, stringsAsFactors = FALSE)
  x <- x[!is.na(analyte), ]
  ret <- tidyr::pivot_wider(
    data         = x, 
    id_cols      = c("ecoregion", "location", "sample"), 
    names_from   = "analyte", 
    values_from  = "result",
    names_sort   = TRUE,
    names_expand = TRUE
  )
  #ret <- reshape2::melt(x, measure.vars = 'result')
  #ret <- reshape2::dcast(ret, formula = location + sample + ecoregion ~ analyte, drop = FALSE)
  ret$Cu.d <- ifelse(total & is.na(ret$Cu.d), ret$Cu.t, ret$Cu.d)
  if (limit){
    ret <- ret[!is.na(ret$Cu.d), ]
  }
  ret
}

#'Fill missing values in BLM input data
#'
#'Fill missing values in BLM input data using simple linear
#'regression on conductivity. These equations were fit by Oregon
#'Department of Environnmental Quality using data collected in Oregon.
#'@param x a data.frame of BLM input data created by \link{formatForBLM}
#'@param HA a default value for humic acid
#'@param S a default value for sulfide ion
#'@export
imputeBLM <- function(x, HA = 10, S = 1e-6) {
  f <- function(x, cond){
    i <- match(cur_column(), kBLMParams$analyte)
    a <- kBLMParams$a[i]
    b <- kBLMParams$b[i]
    if_else(is.na(x), exp(log(cond) * a + b), x)
  }
  
  x |>
    mutate(
      HA = replace_na(HA, 10),
      S  = replace_na(S, 1e-6),
      across(Ca:Alk, ~ f(.x, Cond))
    )
}
# imputeBLM <- function(x, HA = 10, S = 1e-6){
#   k <- !is.na(kBLMParams$a)
#   a <- setNames(kBLMParams$a[k], kBLMParams$analyte[k])
#   b <- setNames(kBLMParams$b[k], kBLMParams$analyte[k])
#   # a%o%x + b
#   f <- function(x, a, b) exp(sweep(log(x) %o% a, 2, -b))
#   i <- names(a)
#   j <- is.na(x[,i])
#   if(any(j)){
#     x[,i][j] <- f(x$Cond, a, b)[j]
#   } 
#   x$S[is.na(x$S)]     <- S
#   x$HA[is.na(x$HA)]   <- HA
#   x$DOC[is.na(x$DOC)] <- 0.83 * x$TOC[is.na(x$DOC)]
#   #Impute regional defaults!
#   x
# }
# 
# imputeBLM2 <- function(x, HA = 10, S = 1e-6) {
#   par <- kBLMParams |> 
#     dplyr::filter(!is.na(a)) |> 
#     dplyr::select(analyte, a, b) |> 
#     tibble::column_to_rownames("analyte")
#   f <- function(x, par) exp(sweep(log(x) %o% par$a, 2, -par$b))
#   cols <- rownames(par)
#   x[,cols] <- coalesce(x[, cols], f())
# }

getBLMData <- function(){
  map <- getOption('blm.analyte.map')
  #kBLMParams$analyte
  kBLMParams$input <- plyr:::mapvalues(kBLMParams$analyte, names(map), map)
  kBLMParams
}