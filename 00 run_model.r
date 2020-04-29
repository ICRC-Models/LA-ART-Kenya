##################################################
## Allen Roberts
## July 1, 2015
## Edited by Jessica Culhane on 12/12/18
##################################################

rm(list = ls())

library(install.load)
install_load("data.table","reshape2","openxlsx","XLConnect","ggplot2","RColorBrewer")

# Code directory
setwd("C:/Users/Jessica/OneDrive - UW/SPEED/Model/Code")
# Data/assumptions workbook
datawb = loadWorkbook("C:/Users/Jessica/OneDrive - UW/SPEED/Model/data and assumptions/Data and Assumptions.xlsx")

## Run name
date <- Sys.Date()
name <- "run_laart" 
dir.create(paste0("output/", date), recursive = TRUE)

## Global variables to specify intervention and effects
int_type <- "LAART"  
  # options are expressed in 03 distributeART code
  # BASE: base case (no intervention)
  # SPEED: SPEED intervention at "speed_effect" level specified below
  # LAART: Assumes proportion switching from oral to injectable ("switch_prop" below) is adherent at "laart_adh" level below
int_yr <- 2019  # start of intervention
speed_effect <- 0.15  #increase in adherence for AYA >= 2019 in SPEED scenario
switch_prop_3_adh <- 0.85 #proportion of 10-14yo adherent to oral ART that switch to LAART
switch_prop_4_adh <- 0.85  #proportion of 15-19yo adherent to oral ART that switch to LAART
switch_prop_5_adh <- 0.85  #proportion of 20-24yo adherent to oral ART that switch to LAART
switch_prop_3_nonadh <- 0.85 #proportion of 10-14yo nonadherent to oral ART that switch to LAART
switch_prop_4_nonadh <- 0.85  #proportion of 15-19yo nonadherent to oral ART that switch to LAART
switch_prop_5_nonadh <- 0.85  #proportion of 20-24yo nonadherent to oral ART that switch to LAART
laart_adh <- 0.94  #proportion of LAART users who are virally suppressed

## Global variables
year_start <- 1979
year_end <- 2030
t_horizon <- year_end - int_yr
tstep <- .05 # years
nsteps <- (year_end - year_start + 1) / tstep
mortred <- 0.98 # 0-4 annual reduction in mortality after 2015

## Attribute values
hiv <- c(0, 1)
age <- seq(1, 12)
male <- c(0, 1)
risk <- seq(1, 3)
cd4 <- seq(1, 6) ## CD4/VL = 6 means that they are uninfected
vl <- seq(1, 6)
circ <- c(0, 1)
vs <- c(0, 1)
art <- c(0, 1)
laart <- c(0, 1)

## Variable for all of the dimensions in our model.  Useful for switching the data.table keys
all_keys <- c("hiv", "age", "male", "risk", "cd4", "vl", "circ", "vs", "art", "laart")

## Source functions
source("01 interpolate.r")
source("02 seedInfections.r")
source("03 distributeART.r")
source("04 circumcision.r")
source("05 demography_functions.r")
source("06 progressDisease.r")
source("07 lambda_functions.r")
source("08 transmit.r")

## Load input epidemiological parameters
source("09 load_parameters.r")

## Initialize population matrix
pop <- as.data.table(expand.grid(sapply(all_keys, get)))
setattr(pop, 'names', c("hiv", "age", "male", "risk", "cd4", "vl", "circ", "vs", "art", "laart"))
pop$count <- 0
pop$diff <- 0

## Data tables for statistics
population <- as.data.table(expand.grid(hiv, age, male, risk, seq(1, nsteps)))
setattr(population, 'names', c("hiv", "age", "male", "risk", "time"))
population[, pop_size := 0]
setkey(population, time, hiv, age, male, risk)

births <- as.data.table(expand.grid(hiv, age, seq(1, nsteps)))
setattr(births, 'names', c("hiv", "age", "time"))
births[, num_births := 0]

deaths <- as.data.table(expand.grid(hiv, age, male, seq(1, nsteps)))
setattr(deaths, 'names', c("hiv", "age", "male", "time"))
deaths[, c("back_deaths", "hiv_deaths") := 0]

## Incidence
incidence <- as.data.table(expand.grid(age, male, seq(1, nsteps)))
setattr(incidence, 'names', c("age", "male", "time"))
incidence[, c("horiz_infections", "vert_infections") := 0]
  
## Distribution of CD4 and VL
dis_dist <- as.data.table(expand.grid(age, male, cd4, vl, art, laart, vs, seq(1, nsteps)))
setattr(dis_dist, 'names', c("age", "male", "cd4", "vl", "art", "laart", "vs", "time"))
dis_dist[, total := 0]
setkey(dis_dist, time, age, male, cd4, vl, art, laart, vs)

## Interventions
interventions <- as.data.table(expand.grid(hiv, age, male, vs, circ, seq(1, nsteps)))
setattr(interventions, 'names', c("hiv", "age", "male", "vs", "circ", "time"))
interventions[, total := 0]
setkey(interventions, time, hiv, age, male, vs, circ)

## Run model 
for(tt in 1:nsteps) {

  print(tt)
  
  if(tt == 1) {
    # Add intial populations.  Initially all are susceptible. 
    setkey(pop, hiv, age, male, cd4, vl, circ, vs, art, laart)
    pop[init_pop, count := pop]
    
    # Distribute by risk group.  
    setkey(pop, age, male, risk)
    pop[risk_props, count := count * (risk_int[tt] * prop_high + (1 - risk_int[tt]) * prop_low)]
    
    # Seed infections - this is currently adding 0.02% of total population to infected groups, but not subtracting them from the susceptible pool.  
    seedInfections(pop, 0.002) 
  }

  pop[, time := tt]
  ## Calculate calendar year
  year <- floor(year_start + (tt - 1) * tstep)

  # Distribute ART coverage
    # Sensitivity analysis for population-wide ART dropout
    # dropout <- as.data.table(readNamedRegion(datawb, name="dropout", header=TRUE))
    # dlow <- ifelse(year>=int_yr, .8, 1)
    # dhigh <- ifelse(year>=int_yr, 1.2, 1)
    # setkey(dropout, age)
    # dropout[, dropout := dropout*dlow]
    # dropout[, dropout := dropout*dhigh]
  distributeART(pop, tt)

  # Distribute circumcision coverage
  distributeCirc(pop, tt)
  
  ## Calculate statistics
  ## Populations
  pop_stats <- pop[, list(size = sum(count)), by = list(hiv, age, male, risk, time)]
  setkey(pop_stats, time, hiv, age, male, risk)
  population[pop_stats, pop_size := size]

  ## Disease distribution
  dis_stats <- pop[hiv == 1, list(size = sum(count)), by = list(vs, age, male, cd4, vl, art, laart, time)]
  setkey(dis_stats, time, age, male, cd4, vl, art, laart, vs)
  dis_dist[dis_stats, total := size]

  ## Intervention coverage
  int_stats <- pop[, list(size = sum(count)), by = list(hiv, age, male, vs, circ, time)]
  setkey(int_stats, time, hiv, age, male, vs, circ)
  interventions[int_stats, total := size]

  # Demography
  addBirths(pop)
  subtractDeaths(pop)
  agePop(pop, tstep)

  # Disease progression
  progressDisease(pop, tstep)

  # Transmission
  # Calculate the mixing matrix
  calcMixMat(pop, mixing_matrix, tt)

  # Calculate adjusted partnerships per year
  adjustPartnerships(pop, mixing_matrix)

  # Calculate lambda
  calcLambda(pop, mixing_matrix, adjusted_partners)

  # Transmit infections
  transmit(pop, lambda_mat)
  
  ## Compute end-of-year population and set difference back to zero for next iteration of loop
  pop[, c("count", "diff") := list(count + diff, 0)]

  # Increment time step
  tt <- tt + 1
}

# Save results
save(population, births, deaths, incidence, interventions, dis_dist, tstep, year_start, year_end, date, name, file = paste("output/", date, "/", name, ".RData", sep = ""))

## Plot results
source("10 plot_results.r")

## Calculate DALYs
source("11 DALYs.r")

