##################################################
## Jessica Culhane
## September 20, 2018
## Description: Distributes circumcision to males by age category. (Neonates done separately in "demography_functions")
##################################################

distributeCirc <- function(dt, time_index) {
  
  ## Circumcision coverage by age
  circ_cov <- as.data.table(data.frame("age" = 1:12, prop =  
                                         sapply(seq(1, length(circ_prop)), function(x) {
                                           circ_prop[[x]][time_index]
                                         })
  ))
  props <- rbindlist(lapply(0:1, function(x, d) data.table(d, circ = x), d = circ_cov))
  props[circ == 0, prop := 1 - prop]
  props$male <- 1
  setkey(props, male, age, circ)
  
  ## Sums the population across age categories
  dt[, c("circ", "sum") := list(circ, sum(count)), by = .(hiv, age, male, risk, cd4, vl, vs, art, laart)]
  
  ## Multiplies the summed population by the CD4 proportions
  setkey(dt, male, age, circ)
  dt[props, count := sum * prop]
  
  dt[, sum := NULL]
  
}