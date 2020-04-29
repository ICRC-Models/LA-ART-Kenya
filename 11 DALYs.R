##################################################
## Jessica Culhane
## September 23, 2018
## Description: Calculate deaths, new infections, and DALYs.
##################################################

year_start <- 1979

## Sum deaths (HIV+ and HIV-)
death_stats[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
deaths_tot <- death_stats[year>=int_yr & year<=(int_yr + t_horizon-1), list(tot_deaths = sum(hiv_deaths + back_deaths), hiv_deaths = sum(hiv_deaths)), by = list(year)]

## Sum new infections
incidence[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
infections <- incidence[year>=int_yr & year<=(int_yr + t_horizon-1), list(infections = sum(horiz_infections + vert_infections)), by = list(year)]

## Sum DALYs due to HIV, excluding deaths
# Note that this undercounts infections in the oldest age groups, since they
# are dropped from the model before the ten-year horizon is up.  This is
# okay considering the low prevalence of HIV in this group and the small 
# effect an adolescent intervention will have on this age group.
dis_stats[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
pop_dalys <- dis_stats[, list(total_pop = sum(total)*tstep), by = list(year, vs, cd4)]
setkey(dalys, vs, cd4)
setkey(pop_dalys, vs, cd4)
pop_dalys[dalys, YLD := DALYwt*total_pop]
pop_dalys[is.na(YLD), YLD := 0]
pop_dalys <- pop_dalys[year>=int_yr & year<=(int_yr + t_horizon-1), list(YLD = sum(YLD)), by = list(year)]

## Calc # of people on ART, LA-ART, and in each CD4 category for costs
art_stats <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & art==1, list(total_art = sum(total)*tstep), by = list(year)]
laart_stats <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & laart==1, list(total_laart = sum(total)*tstep), by = list(year)]
cd4_1 <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & cd4<=3 & art==0 & laart==0, list(cd4_1 = sum(total)*tstep), by = list(year)]
cd4_4 <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & cd4==4 & art==0 & laart==0, list(cd4_4 = sum(total)*tstep), by = list(year)]
cd4_5 <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & cd4==5 & art==0 & laart==0, list(cd4_5 = sum(total)*tstep), by = list(year)]

## Export to Excel
setkey(deaths_tot, year)
setkey(infections, year)
setkey(pop_dalys, year)
setkey(art_stats, year)
setkey(laart_stats, year)
setkey(cd4_1, year)
setkey(cd4_4, year)
setkey(cd4_5, year)
stats <- deaths_tot[infections,][pop_dalys,][art_stats,][laart_stats,][cd4_1,][cd4_4,][cd4_5,] # merge data tables together
writeWorksheetToFile(file=paste0("output/", date, "/", name, ".xlsx"), data=stats, sheet="stats")

## Calculate AYA viral suppression rate
aya_pop <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & age >= 3 & age <= 5 & (art==1 | laart==1), list(total = sum(total)*tstep, merger="AYA")]
vs <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & age >= 3 & age <= 5 & vs==1, list(vstotal = sum(total)*tstep, merger="AYA")]
setkey(aya_pop, merger)
setkey(vs, merger)
vs[aya_pop, denom := total]
vs <- vs[, list(aya_vs_rate = vstotal/denom)]

## Export to Excel
writeWorksheetToFile(file=paste0("output/", date, "/", name, ".xlsx"), data=vs, sheet="aya_vs")

# Calc % of AYA on LAART (as proportion of oral+LA)
aya_art <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & age >= 3 & age <= 5 & art==1, list(total_art = sum(total)*tstep)]
aya_laart <- dis_stats[year>=int_yr & year<=(int_yr + t_horizon-1) & age >= 3 & age <= 5 & laart==1, list(total_laart = sum(total)*tstep)]
laart <- cbind(aya_art, aya_laart)
laart <- laart[, list(aya_laart_prop = total_laart/(total_art + total_laart))]

## Export to Excel
writeWorksheetToFile(file=paste0("output/", date, "/", name, ".xlsx"), data=laart, sheet="aya_laart")

