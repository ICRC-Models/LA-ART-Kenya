##################################################
## Allen Roberts
## July 15, 2015
## Edited by Jessica Culhane on 9/20/18
## Description: Transmits infections based on lambda (risk of infection) and effect of interventions such as 
## PrEP, circumcision, and condoms.
##################################################

transmit <- function(dt, lambdas, time_index = tt) {

  ## Merge on risk reduction interventions.
  dt[, psi := 1]
  dt[circ == 1 & male == 1, psi := psi * (1 - risk_reduction[intervention == "circ", psi])]

  ## Merge on STI acquisition multipliers
  setkey(dt, male, age)
  setkey(sti_probs, male, age)
  dt[sti_probs, sti_acq := sti_acq]
  
  ## Merge on pregnancy acquisition multipliers
  preg_mult <- as.data.table(data.frame("male"=0, "age" = 1:12,
                   preg_acq = sapply(seq(1, length(preg_mult)), function(x) {preg_mult[[x]][time_index]})
  ))
  setkey(dt, male, age)
  setkey(preg_mult, male, age)
  dt[preg_mult, preg_acq := preg_acq]
  dt[male==1, preg_acq := 1]
  
  ## Merge on lambda
  setkey(dt, male, age, risk)
  setkey(lambdas, male, age, risk)
  dt[lambdas, lambda := lambda]
  
  ## Subtract from HIV negative population
  dt[hiv == 0, diff := diff - count * lambda * psi * preg_acq * sti_acq]

  ## Keep track of incidence
  new_infections <- dt[hiv == 0, list(time = tt, new_inf = sum(count * lambda * psi * preg_acq * sti_acq)), by = list(male, age)]
  setkey(new_infections, male, age, time)
  setkey(incidence, male, age, time)
  incidence[new_infections, horiz_infections := new_inf]
  
  ## Add to HIV positive population
  new_hiv <- copy(dt)
  new_hiv <- new_hiv[hiv == 0]
  new_hiv[, c("hiv", "new_infections") := list(1, count * lambda * psi * preg_acq * sti_acq)]
  
  ## Seed new HIV infections in vl = 1 and cd4 = 1
  new_hiv <- new_hiv[, list(new_infections = sum(new_infections)), by = list(hiv, age, male, risk, circ, vs, art, laart)]
  new_hiv[, c("cd4", "vl") := 1]
  setkeyv(new_hiv, all_keys)
  setkeyv(dt, all_keys)
  dt[new_hiv, diff := diff + new_infections]
  
  ## Clean up
  dt[, c("psi", "lambda", "preg_acq", "sti_acq") := NULL]
  
}