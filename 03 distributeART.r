##################################################
## Allen Roberts
## July 27, 2015
## Edited by Jessica Culhane on 12/10/18
## Description: Distributes HIV+ people onto ART to match expected coverage. 
##################################################

distributeART <- function(dt, time_index) {
  ## ART coverage by age and sex 
  art_f <- as.data.table(data.frame("male"=0, "age" = 1:12,
            prop = sapply(seq(1, length(art_base_f)), function(x) {art_base_f[[x]][time_index]})
  ))
  art_m <- as.data.table(data.frame("male"=1, "age" = 1:12,
            prop = sapply(seq(1, length(art_base_m)), function(x) {art_base_m[[x]][time_index]})
  ))
  coverage <- rbind(art_f, art_m)
  coverage <- rbindlist(lapply(1:5, function(x, d) data.table(d, cd4 = x), d = coverage))

  ## adjust probability of ART coverage by CD4 count
  # Calculate CD4 distribution 
  totpop <- pop[time==tt & hiv==1 & cd4<6, list(total = sum(count)), by= list(age, male)]
  cd4pop <- pop[time==tt & hiv==1 & cd4<6, list(cd4total = sum(count)), by = list(age, male, cd4)]

  setkey(totpop, age, male)
  setkey(cd4pop, age, male)
  cd4pop[totpop, cd4dist := ifelse(total==0, 0, cd4total/total)] # get distribution of pop by CD4 stage
  cd4pop$cd4dist <- ifelse(is.nan(cd4pop$cd4dist), 0, cd4pop$cd4dist)
  
  setkey(cd4pop, cd4)
  cd4pop[cd4_probs, cd4raw := cd4dist * artmult] # start by multiplying CD4 distribution by raw CD4/ART multipliers
  raw <- cd4pop[, list(rawtotal = sum(cd4raw)), by=list(age, male)] # total this up

  coverage$year <- year
  
  setkey(raw, age, male)
  setkey(coverage, age, male)
  coverage[raw, prop2 := prop / ifelse(year>=2003 | rawtotal<prop, 1, rawtotal)] # adjust for any difference between the total and the needed ART proportion
  coverage$prop2 <- ifelse(is.nan(coverage$prop2), 0,coverage$prop2)
 
  setkey(cd4_probs, cd4)
  setkey(coverage, cd4)
  coverage[cd4_probs, prop3 := prop2 * ifelse(year>=2003 | prop==prop2, 1, artmult)]
  
  coverage$prop2 <- NULL
  
  ## Create ART and LAART variables to measure oral vs long-acting ART coverage
  setkey(coverage, age)
  setkey(dropout, age)
  coverage[dropout, c("dropout", "adherent") := list(dropout, (1-dropout))]
  
  coverage[, artprop := prop3*ifelse(age==3 & year>=int_yr & (int_type=="LAART"), 1-(switch_prop_3_adh*adherent + switch_prop_3_nonadh*dropout), 
                              ifelse(age==4 & year>=int_yr & (int_type=="LAART"), 1-(switch_prop_4_adh*adherent + switch_prop_4_nonadh*dropout),
                              ifelse(age==5 & year>=int_yr & (int_type=="LAART"), 1-(switch_prop_5_adh*adherent + switch_prop_5_nonadh*dropout), 1)))]
  coverage[, laartprop := prop3*ifelse(age==3 & year>=int_yr & (int_type=="LAART"), (switch_prop_3_adh*adherent + switch_prop_3_nonadh*dropout), 
                                ifelse(age==4 & year>=int_yr & (int_type=="LAART"), (switch_prop_4_adh*adherent + switch_prop_4_nonadh*dropout), 
                                ifelse(age==5 & year>=int_yr & (int_type=="LAART"), (switch_prop_5_adh*adherent + switch_prop_5_nonadh*dropout), 0)))]
  coverage <- rbindlist(lapply(0:1, function(x, d) data.table(d, art = x), d = coverage))
  coverage[art == 0, artprop := 1 - artprop]
  coverage <- rbindlist(lapply(0:1, function(x, d) data.table(d, laart = x), d = coverage))
  coverage[laart == 0, laartprop := 1 - laartprop]

  coverage[art == 1, laartprop := 0]
  coverage[laart == 1, artprop := 0]

  ## Create viral suppression variable that accounts for nonadherence to ART
  ## Includes effect of interventions starting in 2019
  coverage[, artadherent :=
            ifelse(age>=3 & age<=5 & year>=int_yr & int_type=="SPEED", adherent*(1+speed_effect),
            ifelse(age==3 & year>=int_yr & (int_type=="LAART"), adherent*(1-switch_prop_3_adh) / (dropout*(1-switch_prop_3_nonadh) + adherent*(1-switch_prop_3_adh)),
            ifelse(age==4 & year>=int_yr & (int_type=="LAART"), adherent*(1-switch_prop_4_adh) / (dropout*(1-switch_prop_4_nonadh) + adherent*(1-switch_prop_4_adh)),
            ifelse(age==5 & year>=int_yr & (int_type=="LAART"), adherent*(1-switch_prop_5_adh) / (dropout*(1-switch_prop_5_nonadh) + adherent*(1-switch_prop_5_adh)),
                    adherent))))]
  coverage[, laartadherent :=
             ifelse(age>=3 & age<=5 & year>=int_yr & int_type=="LAART", laart_adh,
                     0)]
  coverage[!is.na(adherent), vsprop := 0] # initialize
  coverage[!is.na(adherent) & art==1 , vsprop := artadherent]
  coverage[!is.na(adherent) & laart==1 , vsprop := laartadherent]

  props <- rbindlist(lapply(0:1, function(x, d) data.table(d, vs = x), d = coverage))
  props[art == 0 & laart == 0 & vs == 1, vsprop := 0]
  props[vs == 0, vsprop := 1 - vsprop]
  props[, totalartprop := ifelse(art == 0 & laart == 0, artprop + laartprop - 1, artprop + laartprop)]
  props[, vsprop := vsprop * totalartprop]
  props$hiv <- 1
  setkey(props, hiv, cd4, age, male, vs, art, laart)
 
  ## Sums the population across age, sex, and CD4 categories
  dt[, c("vs", "sum") := list(vs, sum(count)), by = .(hiv, age, male, risk, cd4, vl, circ)]
  
  ## Multiplies the summed population by the age, sex, CD4, and ART proportions
  setkey(dt, hiv, cd4, age, male, vs, art, laart)
  dt[props, count:= sum * vsprop]
  
  dt[, sum := NULL]
  
}