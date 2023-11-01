.onLoad <- function(libname, pkgname) {
  map <- c(
    t     = 'Temperature',
    pH    = 'pH', 
    Cu.d  = 'Copper, dissolved', 
    DOC   = 'Dissolved organic carbon', 
    HA    = 'Humic Acid',
    Ca    = 'Calcium',   
    Mg    = 'Magnesium',   
    Na    = 'Sodium',
    K     = 'Potassium', 
    SO4   = 'Sulfate',    
    Cl    = 'Chloride',
    Alk   = 'Alkalinity', 
    S     = 'Sulfur',
    Cond  = 'Conductivity',
    TOC   = 'Total organic carbon',
    Cu.t  = 'Copper'
  )
  
  metal.map <- c(
    Cd = "Cadmium", 
    Cr = 'Chromium', 
    Cu = "Copper", 
    Pb = "Lead", 
    Ni = "Nickel", 
    Ag = "Silver", 
    Zn = "Zinc"
  )
  
  op <- options()
  op.criteria <- list(
    blm.analyte.map = map,
    metal.analyte.map = metal.map
  )
  toset <- !(names(op.criteria) %in% names(op))
  if(any(toset)) options(op.criteria[toset])
  
  invisible()
}