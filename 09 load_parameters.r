##################################################
## Allen Roberts
## July 7, 2015
## Edited by Jessica Culhane on 9/20/18
## Description: Load model input parameters
##################################################

## This loads the epidemiological parameters that govern the compartmental model functions. See source Excel file for references.

## Initial population
init_pop <- as.data.table(readNamedRegion(datawb, name="initial_population", header=TRUE))
init_pop[, c("hiv", "circ", "vs", "art", "laart") := 0]
init_pop[, c("cd4", "vl") := 6]
setkey(init_pop, hiv, age, male, cd4, vl, circ, vs, art, laart)

## ART coverage
art_base <- as.data.table(readNamedRegion(datawb, name="art_coverage", header=TRUE))
art_base_f <- lapply(sort(unique(art_base$age)), function(age_ind) {
  df <- art_base[age==age_ind & male==0]
  interpolate(breaks = df$year, values = df$prop_art)
})
art_base_m <- lapply(sort(unique(art_base$age)), function(age_ind) {
  df <- art_base[age==age_ind & male==1]
  interpolate(breaks = df$year, values = df$prop_art)
})

cd4_probs <- as.data.table(readNamedRegion(datawb, name="cd4_probs", header=TRUE))

## ART dropout
dropout <- as.data.table(readNamedRegion(datawb, name="dropout", header=TRUE))
#sensitivity analyses:
dropout$dropoutlow <- 0.8*dropout$dropout
dropout$dropouthigh <- 1.2*dropout$dropout
#dropout$dropout <- ifelse(dropout$age %in% c(3,4,5), dropout$dropoutlow, dropout$dropout)
#dropout$dropout <- ifelse(dropout$age %in% c(3,4,5), dropout$dropouthigh, dropout$dropout)
# note sensitivity analysis for population-wide dropout starting in intervention year is in "00 run_model"

## Pregnancy - acquisition multipliers
preg <- as.data.table(readNamedRegion(datawb, name="pregnancy", header=TRUE))
preg_mult <- lapply(sort(unique(preg$age)), function(age_ind) {
  df <- preg[age==age_ind]
  interpolate(breaks = df$year, values = df$preg_acq)
})

rm(preg)

## Fertility - base fertility rate 
## in demography_functions code, we reduce fertility for HIV+ women and increase for HIV- women
fert_base <- as.data.table(readNamedRegion(datawb, name="base_fertility", header=TRUE))
fert_rates <- lapply(sort(unique(fert_base$age)), function(age_ind) {
  df <- fert_base[age==age_ind]
  interpolate(breaks = df$year, values = df$gamma)
})

fert_coeffs <- as.data.table(readNamedRegion(datawb, name="fert_red", header=TRUE))
fert_coeffs <- rbindlist(lapply(0:1, function(x, d) data.table(d, vs = x), d = fert_coeffs))
fert_coeffs[vs == 1, fert_red := 1] ## No reduction in fertility for mothers on ART

rm(fert_base)

## Vertical transmission
vert_trans <- as.data.table(readNamedRegion(datawb, name="vertical_transmission", header=TRUE))
vert_trans <- interpolate(breaks = vert_trans$year, values = vert_trans$vert)

## Circumcision status
circ_status <- as.data.table(readNamedRegion(datawb, name="circ", header=TRUE))

circ_prop <- lapply(sort(unique(circ_status$age)), function(age_ind) {
  df <- circ_status[age == age_ind]
  interpolate(breaks = df$year, values = df$circ_prop)
})
rm(circ_status)

## Background mortality (non-HIV) by age and sex. These are per-year mortality rates.
back_mort_base <- as.data.table(readNamedRegion(datawb, name="background_mortality", header=TRUE))
back_mort_base_f <- lapply(sort(unique(back_mort_base$age)), function(age_ind) {
  df <- back_mort_base[age==age_ind & male==0]
  interpolate(breaks = df$year, values = df$mu)
})
back_mort_base_m <- lapply(sort(unique(back_mort_base$age)), function(age_ind) {
  df <- back_mort_base[age==age_ind & male==1]
  interpolate(breaks = df$year, values = df$mu)
})
rm(back_mort_base)

## HIV-relative mortality
hiv_mort <- as.data.table(readNamedRegion(datawb, name="hiv_mortality", header=TRUE))
hiv_mort <- rbindlist(lapply(0:1, function(x, d) data.table(d, vs = x), d = hiv_mort))
hiv_mort[vs == 1, alpha := 0]
hiv_mort[, c("hiv", "alpha") := list(1, 1 - exp(-alpha * tstep))]
setkey(hiv_mort, hiv, age, male, cd4, vs)

## Disease progression - cd4_duration and vl_duration are average duration spent in that CD4 or VL category (respectively) in years
dis_prog <- as.data.table(readNamedRegion(datawb, name="disease_progression", header=TRUE))
dis_prog$cd4_duration <- as.numeric(dis_prog$cd4_duration)
dis_prog$vl_duration <- as.numeric(dis_prog$vl_duration)
dis_prog$hiv <- 1
dis_prog$vs <- 0

## Initialize mixing matrix.
mixing_matrix <- as.data.table(expand.grid(male, age, risk, male, age, risk))
## The "_p" indicates that the attribute corresponds to the individual's partner
setattr(mixing_matrix, 'names', c("male", "age", "risk", "male_p", "age_p", "risk_p"))
## Assuming only heterosexual transmission
mixing_matrix <- mixing_matrix[mixing_matrix$male != mixing_matrix$male_p]
mixing_matrix$prop <- 0

## Set deltas - these parameters govern the mixing pattern for "completely assortative" mixing, which isn't truly "completely assortative" 
## but is allowed to vary. The delta that gets loaded here is the proportion of partnerships that are with the same age group. 1-delta is 
## the proportion that are with the age group one above (females) or below (males).
deltas <- as.data.table(readNamedRegion(datawb, name="deltas", header=TRUE))
deltas <- interpolate(breaks = deltas$year, values = deltas$delta)

## Set epsilons - these parameters govern the extent to which mixing is random or assortative. 
epsilons <- as.data.table(readNamedRegion(datawb, name="epsilons", header=TRUE))
epsilons_age <- interpolate(breaks= epsilons$year, values = epsilons$epsilon_age)
epsilons_risk <- interpolate(breaks= epsilons$year, values = epsilons$epsilon_risk)

## Number of partners per year by age, sex, and risk
partners <- as.data.table(readNamedRegion(datawb, name="partners_per_year", header=TRUE))
# sensitivity analyses
 #partners$partners_high <- ifelse(partners$age<=5 & partners$age>=3,partners$partners_high*.8,partners$partners_high)
 #partners$partners_low <- ifelse(partners$age<=5 & partners$age>=3,partners$partners_low*.8,partners$partners_low)
# partners$partners_high <- ifelse(partners$age<=5 & partners$age>=3,partners$partners_high*1.2,partners$partners_high)
# partners$partners_low <- ifelse(partners$age<=5 & partners$age>=3,partners$partners_low*1.2,partners$partners_low)

## Theta - parameter that governs the extent to which differences in reported number of sexual partners between males and females is male (1) 
## or female (0) driven
theta <- 0.5

## Number of coital acts per partnership 
acts <- as.data.table(readNamedRegion(datawb, name="acts_per_partnership", header=TRUE))

## Per-act probability of HIV transmission by viral load of partner. 
trans_probs <- as.data.table(readNamedRegion(datawb, name="trans_probs", header=TRUE))

## Per-act probability of HIV transmission by STI status. 
sti_probs <- as.data.table(readNamedRegion(datawb, name="sti", header=TRUE))

## Calculate per-partnership probability of HIV transmission per year.  
## Depends on risk group of HIV-negative partner and viral load of HIV positive partner.
betas <- data.table(expand.grid(male, age, risk, vl, vs))
setattr(betas, 'names', c("male", "age", "risk", "vl", "vs"))

## Join transmission probabilities per act
setkey(betas, vl, vs)
setkey(trans_probs, vl, vs)
betas[trans_probs, chi_base := chi]
setkey(betas, male, age)
setkey(sti_probs, male, age)
betas[sti_probs, chi := chi_base*sti_trans]

## Note the variables actually correspond to the partner
setnames(betas, c("male", "age", "vl", "vs"), c("male_p", "age_p", "vl_p", "vs_p"))
betas[, male := 1-male_p]

## Join sexual acts per year per partnership
setkey(acts, male, risk)
setkey(betas, male, risk)
betas[acts, acts := acts]

## Calculate transmission probability per partnership per time step
betas[, transmission_risk := 1 - (1 - chi) ^ acts]

## Proportion of population in each risk group (by age)
risk_props <- as.data.table(readNamedRegion(datawb, name="risk_proportions", header=TRUE))
setkey(risk_props, age, male, risk)

risk_int <- as.data.table(readNamedRegion(datawb, name="risk_int", header=TRUE))
risk_int <- interpolate(breaks = risk_int$year, values = risk_int$risk_int)

## Risk reduction for HIV-negative partner based on intervention usage
risk_reduction <- as.data.table(readNamedRegion(datawb, name="risk_reduction", header=TRUE))

## DALYs
dalys <- as.data.table(readNamedRegion(datawb, name="DALYs", header=TRUE))

