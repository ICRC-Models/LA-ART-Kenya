##################################################
## Allen Roberts
## July 7, 2015
## Edited by Jessica Culhane on 9/20/18
## Description: Load demography functions for compartmental model
##################################################

## This file contains the demography functions for the compartmental model.  "addBirths" augments the diff 
## variable by multiplying fertility rates by counts of women. "subtractDeaths" decreases the diff variable 
## by multiplying deaths rates by counts.  "agePop" keeps track (in "diff") of the counts entering/leaving 
## each age group due to aging, and assigns risk group based on new age group.

## Note that "dt" stands for "data.table" and "time_index" is the iteration of the loop (corresponding to 
## the global variable tt), which represents the discrete time point. Functions that have "time_index" as 
## an argument are time-dependent.

## Demography functions time_index<-tt dt<-pop
addBirths <- function(dt, time_index = tt) {

  ## Fertility by CD4 count
  fert <- as.data.table(data.frame("age" = 1:12,
           gamma = sapply(seq(1, length(fert_rates)), function(x) {fert_rates[[x]][time_index]} )
  ))

  ## Add effect modification by CD4 count
  fert <- rbindlist(lapply(1:6, function(x, d) data.table(d, cd4 = x), d = fert))
  fert <- rbindlist(lapply(0:1, function(x, d) data.table(d, vs = x), d = fert))

  setkey(fert_coeffs, cd4, age, vs)
  setkey(fert, cd4, age, vs)
  fert[fert_coeffs, gamma := gamma * fert_red]
  
  ## Calculate prevalence in order to increase fertility for HIV- women
  fertpop <- pop[time==tt & male==0, list(total = sum(count)), by= list(age)]
  fertprev <- pop[, fert_group := 0]
  fertprev <- fertprev[cd4 == 1 | hiv == 0 | vs == 1, fert_group := 1]
  fertprev <- fertprev[time==tt & male==0, list(hiv_total = sum(count)), by = list(age, fert_group)]
 
  setkey(fertpop, age)
  setkey(fertprev, age)
  fertprev[fertpop, totprev := hiv_total/total]

  fertred <- fert_coeffs[vs == 0, fert_group := 0]
  setkey(fertred, age, fert_group)
  setkey(fertprev, age, fert_group)
  fertprev[fertred, redprev := totprev * fert_red]
  fertprev <- fertprev[fert_group == 0 & age>3 & age<11]
  
  # increase fertility for HIV- women so that total fertility = base fertility
  # prev*fert*fert_red+(1-prev)*fert*mult = fert (where "fert" = baseline fertility for all women)
  fertprev[, mult := (1 - redprev) / (1 - totprev)]
  fertprev <- rbindlist(lapply(0:1, function(x, d) data.table(d, hiv = x), d = fertprev))
  fertprev <- rbindlist(lapply(1:6, function(x, d) data.table(d, cd4 = x), d = fertprev))
  fertprev <- rbindlist(lapply(0:1, function(x, d) data.table(d, vs = x), d = fertprev))
  fertprev[(hiv == 1 & vs == 0 & cd4 > 1), mult := 1] # only apply multiplier to HIV-, acute, or VS
  setkey(fertprev, cd4, age, vs)
  setkey(fert, cd4, age, vs)
  fert[fertprev, gamma := gamma * mult]

  ## Adjust fertility by time step and convert to risk
  fert[, gamma := 1 - exp(-gamma * tstep)]
  fert$male <- 0

  setkey(fert, age, male, vs)
  setkey(dt, age, male, vs)

  ## All births
  dt[fert, births := count * gamma]

  ## Keep track of birth statistics
  birth_stats <- dt[male == 0, list(time = tt, num = sum(births)), by = list(hiv, age)]
  setkey(birth_stats, time, hiv, age)
  setkey(births, time, hiv, age)
  births[birth_stats, num_births := num]

  ## Calculate births from uninfected mothers. Count mothers on ART as "negatives"
  births_from_neg <- dt[hiv == 0 | vs == 1, sum(births, na.rm = TRUE)]

  ## Calculate  births from infected mothers
  births_from_pos <- dt[hiv == 1 & vs == 0, sum(births, na.rm = TRUE)]

  ## Calculate number of HIV+ births
  pos_births <- births_from_pos * vert_trans[time_index]

  ## Calculate number of HIV- births
  neg_births <- births_from_pos * (1 - vert_trans[time_index]) + births_from_neg

  ## Initialize births added
  dt[, births := 0]

  ## Distribute births added by sex
  dt[hiv == 0 & age == 1 & vl == 6 & cd4 == 6 & vs == 0 & art == 0 & laart == 0 & circ == 0, births := neg_births * 0.5]
  dt[hiv == 1  & age == 1 & vl == 1 & cd4 == 1 & vs == 0 & art == 0 & laart == 0 & circ == 0, births := pos_births * 0.5]

  ## Distribute total births added across risk status. 
  ## (High and low risk props are the same for age group 1.)
  setkey(dt, age, male, risk)
  dt[risk_props, births := births * prop_low]

  ## Add births to population
  dt[, diff := diff + births]

  ## Keep track of new infections
  hiv_births <- dt[hiv == 1 & age == 1, list(inf_births = sum(births), time = tt), by = list(age, male)]

  setkey(hiv_births, time, age, male)
  setkey(incidence, time, age, male)

  incidence[hiv_births, vert_infections := inf_births]

  ## Clean up
  dt[, c("fert_group", "births") := NULL]
}

subtractDeaths <- function(dt, time_index = tt) {

  ## Background mortality by age and sex
  back_mort_f <- as.data.table(data.frame("male"=0, "age" = 1:12,
      mu = sapply(seq(1, length(back_mort_base_f)), function(x) {back_mort_base_f[[x]][time_index]})
  ))
  back_mort_m <- as.data.table(data.frame("male"=1, "age" = 1:12,
      mu = sapply(seq(1, length(back_mort_base_m)), function(x) {back_mort_base_m[[x]][time_index]})
  ))
  back_mort <- rbind(back_mort_f, back_mort_m)

  ## Adjust mortality by time step and convert to risk
  if(year < 1990) {
    back_mort[age==1, mu := mu * 1.01^(1990-year)]
  }  #UN projected reduction in under 5 mortality
  if(year >= 2016) {
    back_mort[age==1, mu := mu * mortred^(year-2015)]
  }  #UN projected reduction in under 5 mortality
  back_mort[, mu := 1 - exp(-mu * tstep)]

  ## Subtract non-HIV deaths
  setkey(back_mort, age, male)
  setkey(dt, age, male)
  dt[back_mort, back_deaths := count * mu]

  ## Subtract HIV deaths
  setkey(dt, hiv, age, male, cd4, vs)
  dt[hiv_mort, hiv_deaths := count * alpha]
  dt[hiv == 0, hiv_deaths := 0]

  dt[, diff := diff - back_deaths - hiv_deaths]

  ## Keep track
  death_stats <- dt[, list(aids_deaths = sum(hiv_deaths), non_aids_deaths = sum(back_deaths), time = tt), by = list(hiv, age, male)]
  setkey(death_stats, time, hiv, age, male)
  setkey(deaths, time, hiv, age, male)
  deaths[death_stats, c("hiv_deaths", "back_deaths") := list(aids_deaths, non_aids_deaths)]

  ## Clean up
  dt[, c("back_deaths", "hiv_deaths") := NULL]

}


agePop <- function(dt, time_step, time_index = tt) {

  ## Efflux - subtract 1/5 of each compartment for each year
  dt[, diff := diff - count * time_step/5]

  ## Sums the population across risk categories, moves everyone to next age group
  riskcopy <- copy(dt)[, c("risk", "sum") := list(risk, sum(count)), by = .(hiv, age, male, cd4, vl, circ, vs, art, laart)]
  riskcopy <- riskcopy[, age := age + 1]
 
  ## Multiplies the summed population by the risk proportions defined in the initial parameters
  setkey(riskcopy, age, male, risk)
  setkey(risk_props, age, male, risk)
  riskcopy[risk_props, count := sum * (risk_int[time_index] * prop_high + (1 - risk_int[time_index]) * prop_low)]

  # Influx - for all but first age group, add 1/5 of corresponding compartment from previous age group
  setnames(riskcopy, "count", "count_prev")
  setkeyv(riskcopy, all_keys)
  setkeyv(dt, all_keys)
  dt[riskcopy, diff := diff + time_step/5 * count_prev]
}