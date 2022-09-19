.onLoad <- function(libname, pkgname) {
  map <- c(t          = 'Temperature',
           pH         = 'pH', 
           Cu.d       = 'Copper, dissolved', 
           DOC        = 'Dissolved organic carbon', 
           HA         = 'Humic Acid',
           Ca         = 'Calcium, dissolved',   
           Mg         = 'Magnesium, dissolved',   
           Na         = 'Sodium, dissolved',
           K          = 'Potassium, dissolved', 
           SO4        = 'Sulfate',    
           Cl         = 'Chloride',
           Alk        = 'Alkalinity', 
           S          = 'Sulfur',
           Cond       = 'conductivity - specific',
           TOC        = 'Total organic carbon',
           Cu.t       = 'Copper')
  
  metal.map <- c("Cd" = "Cadmium", 
                 "Cr" = 'Chromium', 
                 "Cu" = "Copper", 
                 "Pb" = "Lead", 
                 "Ni" = "Nickel", 
                 "Ag" = "Silver", 
                 "Zn" = "Zinc")
  
  op <- options()
  op.criteria <- list(
    blm.analyte.map = map,
    metal.analyte.map = metal.map
  )
  toset <- !(names(op.criteria) %in% names(op))
  if(any(toset)) options(op.criteria[toset])
  
  invisible()
}