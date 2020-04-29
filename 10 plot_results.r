##################################################
## Allen Roberts
## July 30, 2015
## Edited by Jessica Culhane on 9/20/18
##################################################

## Formatting
population[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
population$male <- factor(population$male, levels = c(0, 1), labels = c("Female", "Male"))
population$age <- factor(population$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))
population$hiv <- factor(population$hiv, levels = c(0, 1), labels = c("Negative", "Positive"))

births[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
births$age <- factor(births$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))
births$hiv <- factor(births$hiv, levels = c(0, 1), labels = c("Negative", "Positive"))

death_stats <- deaths
deaths[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
deaths$male <- factor(deaths$male, levels = c(0, 1), labels = c("Female", "Male"))
deaths$age <- factor(deaths$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))

incidence[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
incidence$male <- factor(incidence$male, levels = c(0, 1), labels = c("Female", "Male"))
incidence$age <- factor(incidence$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))

interventions[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)] 
interventions$male <- factor(interventions$male, levels = c(0, 1), labels = c("Female", "Male"))
interventions$age <- factor(interventions$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))
interventions$hiv <- factor(interventions$hiv, levels = c(0, 1), labels = c("Negative", "Positive"))

stopifnot(dis_dist[cd4 == 6 | vl == 6, sum(total)] == 0) ## No HIV-infected individual should have VL or CD4 = 6 - that's reserved for uninfected (hiv == 0) individuals
dis_stats <- dis_dist
dis_dist <- dis_dist[cd4 != 6 & vl != 6]
dis_dist[, c("year", "year_exact") := list(floor(year_start + (time - 1) * tstep), year_start + (time - 1) * tstep)]
dis_dist$male <- factor(dis_dist$male, levels = c(0, 1), labels = c("Female", "Male"))
dis_dist$age <- factor(dis_dist$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))
dis_dist$cd4 <- factor(dis_dist$cd4, levels = seq(1, 5), labels = c("Acute", "> 500", "350-500", "200-349", "< 200"))
#dis_dist$vl <- ifelse(dis_dist$vs == 1, 2, dis_dist$vl)
dis_dist$vl <- factor(dis_dist$vl, levels = seq(1, 5), labels = c("Acute", "< 1,000", "1,000-10,000", "10,000-50,000", "> 50,000"))


## Plot control
year_start <- 1980  ## set start of x axis

theme_set(theme_classic(base_size=10))
colors <- c("blue4", "green4")
names(colors) <- c("Female", "Male")
sexColors <- scale_colour_manual(name = "Sex", values = colors)

vl_colors <- rev(brewer.pal(5, "Spectral"))
names(vl_colors) <- c("Acute", "< 1,000", "1,000-10,000", "10,000-50,000", "> 50,000")
vlColors <- scale_fill_manual(name = "Viral Load", values = vl_colors)

cd4_colors <- rev(brewer.pal(5, "Spectral"))
names(cd4_colors) <- c("Acute", "> 500", "350-500", "200-349", "< 200")
cd4Colors <- scale_fill_manual(name = "CD4", values = cd4_colors)

#pd <- position_dodge(width=1) # staggers error bars so they don't overlap


## Population data
pop_data <- as.data.table(readNamedRegion(datawb, name="pop_data", header=TRUE))
pop_data$male <- factor(pop_data$male, levels = c(0, 1), labels = c("Female", "Male"))
pop_data$age <- factor(pop_data$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))


## AYA-specific population
aya_pop <- population[as.numeric(age) >= 3 & as.numeric(age) <= 5, list(total = sum(pop_size)), by= list(year_exact, year)]
aya_pop_data <- pop_data[as.numeric(age) >= 3 & as.numeric(age) <= 5, list(total = sum(pop)), by = list(year)]

aya_pop_plot <- ggplot() +
  geom_point(data = aya_pop, aes(x = year_exact, y = total / 1000000, colour="Model"), shape="-") +
  geom_point(data = aya_pop_data, aes(x = year, y = total / 1000000, fill="green4", colour = "Kenya data"), size=3) +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Kenya Population (Millions)") +
  scale_colour_manual(name="", limits=c("Model","Kenya data"), values=c("blue4", "green4"), labels=c("Model","Kenya data")) +
  guides(fill=FALSE, color=guide_legend(override.aes = list(shape=c(15, 17), size=c(1, 3), fill=c("blue4","green4")))) +
  ggtitle("AYA population")


## Total population
total_pop <- population[, list(total = sum(pop_size)), by= list(year_exact, year)]
total_pop_data <- pop_data[, list(total = sum(pop)), by = list(year)]

total_pop_plot <- ggplot() +
  geom_point(data = total_pop, aes(x = year_exact, y = total / 1000000, colour="Model"), shape=15, size=.7) +
  geom_point(data = total_pop_data, aes(x = year, y = total / 1000000, fill="green4", colour = "Kenya data"), size=3, shape=8) +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Kenya Population (Millions)") +
  scale_colour_manual(name="", limits=c("Model","Kenya data"), values=c("blue4", "green4"), labels=c("Model","Kenya data")) +
  guides(fill=FALSE, color=guide_legend(override.aes = list(shape=c(15, 8), size=c(1, 3), fill=c("blue4","green4")))) 
  #+ ggtitle("Total population")


## Age-specific population
age_pop <- population[, list(total = sum(pop_size)), by= list(age, male, year_exact, year)]

age_pop_plot <- ggplot(data = age_pop, aes(x = year_exact, y = total / 1000000)) +
  geom_line(aes(colour = male)) +
  geom_point(data = pop_data, aes(x = year, y = pop / 1000000, colour = male)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Kenya Population (Millions)") +
  facet_wrap(~age) +
  ggtitle("Age-specific population")


# Risk-specific population
risk_pop <- population[, list(total = sum(pop_size)), by= list(risk, male, year_exact, year)]

risk_pop_plot <- ggplot(data = risk_pop, aes(x = year_exact, y = total / 1000000)) +
  geom_line(aes(colour = male)) +
  sexColors +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Kenya Population (Millions)") +
  facet_wrap(~risk) +
  ggtitle("Risk-specific population")


## Birth rates
births <- births[, list(births = sum(num_births)), by = list(age, year_exact, year)]
births$male <- "Female"

setkey(births, age, year_exact, male)
setkey(age_pop, age, year_exact, male)

births[age_pop, denom := total]
births <- births[, list(birth_rate = sum(births)/sum(denom * tstep)), by = list(year, age, male)]

birth_rate_age_plot <- ggplot(data = births, aes(x = year, y = birth_rate)) +
    geom_line() +
    scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
    xlab("Year") + ylab("Annual birth rate") +
    facet_wrap(~age) +
    ggtitle("Age-specific fertility rates")


## Total deaths
tot_deaths <- deaths[, list(all_deaths = sum(back_deaths)+sum( hiv_deaths), hiv_deaths = sum(hiv_deaths)), by = list(year)]
hiv_deaths_plot <- ggplot(data = tot_deaths, aes(x = year, y = hiv_deaths)) +
  geom_line() + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Annual Deaths") +
  ggtitle("Total HIV Deaths")
tot_deaths_plot <- ggplot(data = tot_deaths, aes(x = year, y = all_deaths)) +
  geom_line() + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Annual Deaths") +
  ggtitle("Total Deaths")

tot_deaths <- deaths[, list(all_deaths = sum(back_deaths)+sum(hiv_deaths), hiv_deaths = sum(hiv_deaths)), by = list(year_exact, year)]
setkey(tot_deaths, year_exact)
setkey(total_pop, year_exact)
tot_deaths[total_pop, denom := total]
tot_deaths <- tot_deaths[, list(hiv_death_rate = sum(hiv_deaths)/sum(denom * tstep), death_rate = sum(all_deaths)/sum(denom * tstep)), by = list(year)]
tot_deaths <- melt(tot_deaths, id.vars = c("year"), measure.vars = c("hiv_death_rate", "death_rate"), variable.name = "type", value.name = "rate")
tot_deaths$type <- factor(tot_deaths$type, levels = c("hiv_death_rate", "death_rate"), labels = c("HIV", "Total"))
tot_death_rate_plot <- ggplot(data = tot_deaths, aes(x = year, y = rate)) +
  geom_line(aes(linetype = type)) +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Annual mortality rate") +
  ggtitle("HIV and overall mortality rates")


## AYA deaths
aya_deaths <- deaths[as.numeric(age) >= 3 & as.numeric(age) <= 5, list(hiv_deaths = sum(hiv_deaths)), by = list(year, male)]

aya_deaths_plot <- ggplot(data = aya_deaths, aes(x = year, y = hiv_deaths)) +
  geom_line(aes(colour = male)) + 
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Annual Deaths") +
  ggtitle("AYA HIV Deaths")


## AYA death rates
aya_deaths <- deaths[as.numeric(age) >= 3 & as.numeric(age) <= 5, list(non_aids_deaths = sum(back_deaths), aids_deaths = sum(hiv_deaths)), by = list(age, male, year_exact, year)]
aya_age_pop <- population[as.numeric(age) >= 3 & as.numeric(age) <= 5, list(total = sum(pop_size)), by= list(age, male, year_exact, year)]

setkey(aya_deaths, age, year_exact, male)
setkey(aya_age_pop, age, year_exact, male)

aya_deaths[aya_age_pop, denom := total]
aya_deaths <- aya_deaths[, list(hiv_death_rate = sum(aids_deaths)/sum(denom * tstep), back_death_rate = sum(non_aids_deaths)/sum(denom * tstep)), by = list(year, age, male)]

aya_deaths <- melt(aya_deaths, id.vars = c("age", "year", "male"), measure.vars = c("hiv_death_rate", "back_death_rate"), variable.name = "type", value.name = "rate")
aya_deaths$type <- factor(aya_deaths$type, levels = c("hiv_death_rate", "back_death_rate"), labels = c("HIV", "Background"))
aya_death_rate_age_plot <- ggplot(data = aya_deaths, aes(x = year, y = rate)) +
  geom_line(aes(colour = male, linetype = type)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Annual mortality rate") +
  facet_wrap(~age) +
  ggtitle("AYA HIV and background age-specific mortality rates")


## Death rates
deaths <- deaths[, list(non_aids_deaths = sum(back_deaths), aids_deaths = sum(hiv_deaths)), by = list(age, male, year_exact, year)]

setkey(deaths, age, year_exact, male)
setkey(age_pop, age, year_exact, male)

deaths[age_pop, denom := total]
deaths <- deaths[, list(hiv_death_rate = sum(aids_deaths)/sum(denom * tstep), back_death_rate = sum(non_aids_deaths)/sum(denom * tstep)), by = list(year, age, male)]

deaths <- melt(deaths, id.vars = c("age", "year", "male"), measure.vars = c("hiv_death_rate", "back_death_rate"), variable.name = "type", value.name = "rate")
deaths$type <- factor(deaths$type, levels = c("hiv_death_rate", "back_death_rate"), labels = c("HIV", "Background"))
death_rate_age_plot <- ggplot(data = deaths, aes(x = year, y = rate)) +
  geom_line(aes(colour = male, linetype = type)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Annual mortality rate") +
  facet_wrap(~age) +
  ggtitle("HIV and background age-specific mortality rates")


## HIV prevalence by age band
all_pop <- population[year %in% c("2003","2007","2012"), list(total = sum(pop_size)), by= list(male, age, year)] 
total_prev <- population[year %in% c("2003","2007","2012"), list(hiv_total = sum(pop_size)), by = list(hiv, male, age, year)]
setkey(all_pop, male, age, year)
setkey(total_prev, male, age, year)
total_prev[all_pop, prev := 100 * hiv_total/total]
total_prev <- total_prev[hiv == "Positive"]

prev_data <- as.data.table(readNamedRegion(datawb, name="age_specific_prevalence", header=TRUE))
prev_data$male <- factor(prev_data$male, levels = c(0, 1), labels = c("Female", "Male"))
prev_data$age <- factor(prev_data$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))
prev_data[, prev := prevalence * 100]
prev_data[, ci_low := ci_low * 100]
prev_data[, ci_high := ci_high * 100]

shapes <- c(1, 2)
names(shapes) <- c("Female", "Male")
sexShapes <- scale_colour_manual(name = "Sex", values = colors)

prev_plot <- ggplot(data = total_prev, aes(x = age, y = prev)) +
  geom_line(aes(group = male, colour = male)) +
  #geom_errorbar(data=prev_data, aes(x = age, ymin=ci_low, ymax=ci_high), width=.7) +
  geom_jitter(data = prev_data, width=.08, aes(x = age, y = prev, shape=male,colour = male)) +
  sexColors +
  scale_x_discrete() +
  xlab("Age Group") + ylab("HIV Prevalence (%)") +
  facet_grid(year~.) +
  ggtitle("HIV Prevalence Age Curve by Year") + 
  guides(shape = "none")


## HIV prevalence by age band - 2012 only
all_pop <- population[year %in% c("2012"), list(total = sum(pop_size)), by= list(male, age, year)] 
total_prev <- population[year %in% c("2012"), list(hiv_total = sum(pop_size)), by = list(hiv, male, age, year)]
setkey(all_pop, male, age, year)
setkey(total_prev, male, age, year)
total_prev[all_pop, prev := 100 * hiv_total/total]
total_prev <- total_prev[hiv == "Positive"]

prev_data <- as.data.table(readNamedRegion(datawb, name="age_specific_prevalence", header=TRUE))
prev_data_2012 <- prev_data[prev_data$year %in% c("2012"),]
prev_data_2012$male <- factor(prev_data_2012$male, levels = c(0, 1), labels = c("Female", "Male"))
prev_data_2012$age <- factor(prev_data_2012$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))
prev_data_2012[, prev := prevalence * 100]
prev_data_2012[, ci_low := ci_low * 100]
prev_data_2012[, ci_high := ci_high * 100]

shapes <- c(1, 2)
names(shapes) <- c("Female", "Male")
sexShapes <- scale_colour_manual(name = "Sex", values = colors)

prev_plot_2012 <- ggplot(data = total_prev, aes(x = age, y = prev)) +
  theme_classic(base_size=18)+
  theme(panel.spacing = unit(1.8, "lines"), axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_point(aes(group = male, colour = "Model"), shape=15, size=2) +
  geom_line(aes(group = male, colour = "Model"), size=1.3) +
  #geom_errorbar(data=prev_data_2012, aes(x = age, ymin=ci_low, ymax=ci_high), width=.2) +
  geom_point(data = prev_data_2012, aes(x = age, y = prev, fill="seagreen", colour ="Kenya data"), size=3, shape=8) +
  #sexColors +
  scale_x_discrete() +
  facet_grid(male~.) +
  xlab("Age Group") + ylab("HIV Prevalence (%)") +
  #ggtitle("HIV Prevalence Age Curve by Year") + 
  #guides(shape = "none")
  scale_colour_manual(name="", limits=c("Model","Kenya data"), values=c("darkslategrey", "seagreen"), labels=c("Model","Kenya data")) +
  guides(fill=FALSE, color=guide_legend(override.aes = list(shape=c(15, 8), size=c(2,2), fill=c("darkslategrey","seagreen")))) +
  theme(legend.position = "bottom")


## HIV prevalence 15-49
total_adult_pop <- population[as.numeric(age) > 3 & as.numeric(age) <= 10, list(total = sum(pop_size)), by= list(year_exact)] 
adult_prev <- population[as.numeric(age) > 3 & as.numeric(age) <= 10 , list(hiv_total = sum(pop_size)), by = list(hiv, year_exact)]
setkey(total_adult_pop, year_exact)
setkey(adult_prev, year_exact)
adult_prev[total_adult_pop, prev := 100 * hiv_total/total]
adult_prev <- adult_prev[hiv == "Positive"]

adult_prev_data <- as.data.table(readNamedRegion(datawb, name="adult_prevalence", header=TRUE))
adult_prev_data[, prev := adult_prev * 100]
adult_prev_data[, ci_low := ci_low * 100]
adult_prev_data[, ci_high := ci_high * 100]

adult_prev_plot <- ggplot() +
    geom_point(data = adult_prev, aes(x = year_exact, y = prev, colour="Model"), shape=15, size=.4) +
    #geom_errorbar(data=adult_prev_data, aes(x = year, ymin=ci_low, ymax=ci_high), width=.7) +
    geom_point(data = adult_prev_data, aes(x = year, y = prev, fill="seagreen", colour = "Kenya data"), size=3, shape=8) +
    scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
    xlab("Year") + ylab("HIV Prevalence (%)") +
    scale_colour_manual(name="", limits=c("Model","Kenya data"), values=c("darkslategrey", "seagreen"), labels=c("Model","Kenya data")) +
    guides(fill=FALSE, color=guide_legend(override.aes = list(shape=c(15, 8), size=c(1, 3), fill=c("darkslategrey","seagreen")))) 
    #+ ggtitle("HIV Prevalence (15-49)")


## HIV prevalence 15-24
total_ya_pop <- population[as.numeric(age) >= 4 & as.numeric(age) <= 5, list(total = sum(pop_size)), by= list(male, year_exact)] 
ya_prev <- population[as.numeric(age) >= 4 & as.numeric(age) <= 5 , list(hiv_total = sum(pop_size)), by = list(hiv, male, year_exact)]
setkey(total_ya_pop, male, year_exact)
setkey(ya_prev, male, year_exact)
ya_prev[total_ya_pop, prev := 100 * hiv_total/total]
ya_prev <- ya_prev[hiv == "Positive"]

ya_prev_data <- as.data.table(readNamedRegion(datawb, name="aya_prevalence", header=TRUE))
ya_prev_data[, prev := aya_prev * 100]
ya_prev_data[, ci_low := ci_low * 100]
ya_prev_data[, ci_high := ci_high * 100]

ya_prev_plot <- ggplot(data = ya_prev, aes(x = year_exact, y = prev)) +
  geom_line(aes(colour = male)) +
  geom_errorbar(data=ya_prev_data, aes(x = year, ymin=ci_low, ymax=ci_high), width=.7) +
  geom_point(data = ya_prev_data, aes(x = year, y = prev)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("HIV Prevalence (%)") +
  ggtitle("HIV Prevalence (15-24)")


## HIV prevalence 10-24
total_aya_pop <- population[as.numeric(age) >= 3 & as.numeric(age) <= 5, list(total = sum(pop_size)), by= list(male, year_exact)] 
aya_prev <- population[as.numeric(age) >= 3 & as.numeric(age) <= 5 , list(hiv_total = sum(pop_size)), by = list(hiv, male, year_exact)]
setkey(total_aya_pop, male, year_exact)
setkey(aya_prev, male, year_exact)
aya_prev[total_aya_pop, prev := 100 * hiv_total/total]
aya_prev <- aya_prev[hiv == "Positive"]

aya_prev_plot <- ggplot(data = aya_prev, aes(x = year_exact, y = prev)) +
  geom_line(aes(colour = male)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("HIV Prevalence (%)") +
  ggtitle("HIV Prevalence (Ages 10-24)")


## Age-specific prevalence
age_prev <- population[, list(hiv_total = sum(pop_size)), by = list(hiv, male, age, year_exact)]
setkey(age_prev, male, age, year_exact)
setkey(age_pop, male, age, year_exact)
age_prev[age_pop, prev := 100 * hiv_total/total]
age_prev <- age_prev[hiv == "Positive"]

age_prev_data <- as.data.table(readNamedRegion(datawb, name="age_specific_prevalence", header=TRUE))
age_prev_data <- age_prev_data[male  < 2]
age_prev_data$male <- factor(age_prev_data$male, levels = c(0, 1), labels = c("Female", "Male"))
age_prev_data$age <- factor(age_prev_data$age, levels = seq(1, 12), labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59"))
age_prev_data[, prev := prevalence * 100]
age_prev_data[, ci_low := ci_low * 100]
age_prev_data[, ci_high := ci_high * 100]

age_prev_plot <- ggplot(data = age_prev, aes(x = year_exact, y = prev)) +
  geom_line(aes(colour = male)) +
  geom_errorbar(data=age_prev_data, aes(x = year, ymin=ci_low, ymax=ci_high), width=.7) +
  geom_point(data = age_prev_data, aes(x = year, y = prev, colour = male), cex=1) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("HIV Prevalence (%)") +
  facet_wrap(~age) + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
  #+ ggtitle("Age-specific HIV prevalence")


## Risk-specific prevalence
risk_prev <- population[, list(hiv_total = sum(pop_size)), by = list(hiv, male, risk, year_exact)]
setkey(risk_prev, male, risk, year_exact)
setkey(risk_pop, male, risk, year_exact)
risk_prev[risk_pop, prev := 100 * hiv_total/total]
risk_prev <- risk_prev[hiv == "Positive"]

risk_prev_plot <- ggplot(data = risk_prev, aes(x = year_exact, y = prev)) +
  geom_line(aes(colour = male)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("HIV Prevalence (%)") +
  facet_wrap(~risk) +
  ggtitle("Risk-specific HIV prevalence")


## HIV Incidence
## Total
total_incidence <- incidence[, list(infections = sum(horiz_infections + vert_infections)), by = list(year, year_exact, male)]
total_pop <- population[, list(total = sum(pop_size)), by= list(male, year_exact, year)]
setkey(total_incidence, male, year_exact, year)
setkey(total_pop, male, year_exact, year)
total_incidence[total_pop, denom := total]
total_incidence <- total_incidence[, list(incidence_rate = sum(infections)/sum(denom * tstep)), by = list(year, male)]

incidence_rate_plot <- ggplot(data = total_incidence, aes(x = year, y = incidence_rate * 100)) +
  geom_line(aes(colour = male)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Incidence rate (%)") +
  ggtitle("Total HIV incidence")


total_incidence <- incidence[, list(infections = sum(horiz_infections + vert_infections)), by = list(year, year_exact)]
total_pop <- population[, list(total = sum(pop_size)), by= list(year_exact, year)]
setkey(total_incidence, year_exact, year)
setkey(total_pop, year_exact, year)
total_incidence[total_pop, denom := total]
total_incidence <- total_incidence[, list(incidence_rate = sum(infections)/sum(denom * tstep)), by = list(year)]

inc_data <- as.data.table(readNamedRegion(datawb, name="incidence", header=TRUE))
inc_data[, incidence := incidence / 10]
inc_data[, ci_low := lower / 10]
inc_data[, ci_high := upper / 10]

incidence_rate_plot2 <- ggplot() +
  geom_point(data = total_incidence, aes(x = year, y = incidence_rate * 100, colour="Model"), shape="-") +
  geom_line(data = total_incidence, aes(x = year, y = incidence_rate * 100, colour="Model")) +
  geom_errorbar(data=inc_data, aes(x = year, ymin=ci_low, ymax=ci_high), width=.7) +
  geom_point(data = inc_data, aes(x = year, y = incidence, fill="green4", colour = "Kenya estimates"), size=2) +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Incidence Rate (%)") +
  scale_colour_manual(name="", limits=c("Model","Kenya estimates"), values=c("blue4", "green4"), labels=c("Model","Kenya estimates")) +
  guides(fill=FALSE, color=guide_legend(override.aes = list(shape=c(15, 17), size=c(1, 3), fill=c("blue4","green4")))) +
  ggtitle("Total HIV incidence")


## By Age
age_incidence <- incidence[, list(infections = sum(horiz_infections + vert_infections)), by = list(age, male, year_exact, year)]

setkey(age_incidence, age, year_exact, male)
setkey(age_pop, age, year_exact, male)

age_incidence[age_pop, denom := total]
age_incidence <- age_incidence[, list(incidence_rate = sum(infections)/sum(denom * tstep)), by = list(year, age, male)]

incidence_rate_age_plot <- ggplot(data = age_incidence, aes(x = year, y = incidence_rate * 100)) +
  geom_line(aes(colour = male)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Annual incidence (%)") +
  facet_wrap(~age) +
  ggtitle("Age-specific HIV incidence")


## Intervention coverage
## ART
art_age <- interventions[hiv == "Positive", list(size = sum(total)), by = list(age, male, year_exact, vs)]
art_age <- art_age[vs == 1]
hiv_age_pop <- population[hiv == "Positive", list(denom = sum(pop_size)), by = list(age, male, year_exact)]
setkey(art_age, age, male, year_exact)
setkey(hiv_age_pop, age, male, year_exact)
art_age[hiv_age_pop, cov := 100 * size / denom]
art_age[is.na(cov), cov := 0]

art_age_plot <- ggplot(data = art_age, aes(x = year_exact, y = cov)) +
  geom_line(aes(colour = male)) +
  sexColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  xlab("Year") + ylab("ART Coverage (%)") +
  facet_wrap(~age) +
  ggtitle("Age-specific ART coverage")

art <- interventions[hiv == "Positive", list(size = sum(total)), by = list(year_exact, vs)]
art <- art[vs == 1]
hiv_pop <- population[hiv == "Positive", list(denom = sum(pop_size)), by = list(year_exact)]
setkey(art, year_exact)
setkey(hiv_pop, year_exact)
art[hiv_pop, cov := 100 * size / denom]
art[is.na(cov), cov := 0]

year <- c(2015, 2016, 2017)
cov <- c(50,55,63) # source: http://aidsinfo.unaids.org/
art_data <- data.frame(year, cov)

art_plot <- ggplot() +
  geom_point(data = art, aes(x = year_exact, y = cov, colour="Model"), shape=15, size=.3) +
  geom_point(data = art_data, aes(x = year, y = cov, fill="green4", colour = "Kenya data"), size=3, shape=8) +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  xlab("Year") + ylab("Virally Suppressed (%)") +
  scale_colour_manual(name="", limits=c("Model","Kenya data"), values=c("blue4", "green4"), labels=c("Model","Kenya data")) +
  guides(fill=FALSE, color=guide_legend(override.aes = list(shape=c(15, 8), size=c(.3, 3), fill=c("blue4","green4")))) 


## Circumcision
circ_age <- interventions[male == "Male", list(size = sum(total)), by = list(age, year_exact, circ)]
circ_age <- circ_age[circ == 1]
male_age_pop <- population[male == "Male", list(denom = sum(pop_size)), by = list(age, year_exact)]
setkey(circ_age, age, year_exact)
setkey(male_age_pop, age,  year_exact)
circ_age[male_age_pop, cov := 100 * size / denom]
circ_age[is.na(cov), cov := 0]

circ_age_plot <- ggplot(data = circ_age, aes(x = year_exact, y = cov)) +
  geom_line() +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  xlab("Year") + ylab("Circumcision Coverage (%)") +
  facet_wrap(~age) +
  ggtitle("Age-specific circumcision coverage")


## CD4 
cd4_dist <- dis_dist[, list(size = sum(total)), by = list(age, cd4, year_exact)]
cd4_dist[, pct := size / sum(size), by = list(age, year_exact)]
cd4_plot_age <- ggplot(data = cd4_dist, aes(x = year_exact, y = pct * 100)) +
  geom_area(aes(x = year_exact, y = pct * 100, fill = cd4)) +
  cd4Colors +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Percentage") +
  facet_wrap(~age) + ggtitle("CD4 Distribution among HIV+")

cd4_dist <- dis_dist[, list(size = sum(total)), by = list(cd4, year_exact)]
cd4_dist[, pct := size / sum(size), by = list(year_exact)]
cd4_plot <- ggplot(data = cd4_dist, aes(x = year_exact, y = pct * 100)) +
  geom_area(aes(x = year_exact, y = pct * 100, fill = cd4)) +
  cd4Colors +
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Percentage") 


## VL
vl_dist <- dis_dist[, list(size = sum(total)), by = list(age, vl, year_exact)]
vl_dist[, pct := size / sum(size), by = list(age, year_exact)]
vl_plot_age <- ggplot(data = vl_dist, aes(x = year_exact, y = pct * 100)) +
  geom_area(aes(fill = vl)) +
  vlColors + 
  scale_x_continuous(limits = c(year_start, year_end), breaks = seq(year_start, year_end, by = 10)) +
  xlab("Year") + ylab("Percentage") +
  facet_wrap(~age) + ggtitle("Viral Load Distribution among HIV+")


## Save as PDF
pdf(file = paste0("output/", date, "/", name, ".pdf"), width = 15, height = 8)
#print(aya_pop_plot)
print(total_pop_plot)
print(age_pop_plot)
print(birth_rate_age_plot)
#print(aya_deaths_plot)
print(death_rate_age_plot)
print(tot_death_rate_plot)
print(tot_deaths_plot)
print(hiv_deaths_plot)
#print(aya_death_rate_age_plot)
#print(aya_prev_plot)
print(ya_prev_plot)
print(adult_prev_plot)
print(prev_plot)
print(age_prev_plot)
#print(prev_plot_2012)
#print(incidence_rate_plot)
print(incidence_rate_plot2)
print(incidence_rate_age_plot)
print(risk_pop_plot)
print(risk_prev_plot)
print(art_age_plot)
print(circ_age_plot)
print(cd4_plot_age)
#print(cd4_plot)
print(vl_plot_age)
dev.off()
