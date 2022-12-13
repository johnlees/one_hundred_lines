### packages needed
library(tidyverse)
library(lubridate)
library(epiR)

### read in the data (SHOULD BE CLEANED DATA, CHOOSE CORRECT PATH)
# Linelist_raw <- read_csv("/rivm/s/klinkend/workshop100days/Linelist_grp4.csv")
Linelist_raw <- read_csv("Linelist_grp4.csv")

### Some plots and summaries to interpret the data
# incidence by location and exposure
Linelist_raw %>%
  ggplot(aes(x = onset_date, fill = exposure)) + 
  geom_histogram() + 
  facet_wrap(~ origin) 
  
# incidence by location and outcome
Linelist_raw %>%
  ggplot(aes(x = onset_date, fill = outcome)) + 
  geom_histogram() + 
  facet_wrap(~ origin)

# outcome vs exposure -> no association
Linelist_raw %>%
  mutate(outcome = if_else(is.na(outcome), "Alive", outcome)) %>%
  group_by(exposure, outcome) %>%
  summarise(aantal = n()) %>%
  pivot_wider(names_from = outcome, values_from = aantal) %>%
  ungroup() %>%
  select(-exposure) %>%
  as.matrix() %>%
  chisq.test()

# outcome vs origin -> no association
Linelist_raw %>%
  mutate(outcome = if_else(is.na(outcome), "Alive", outcome)) %>%
  group_by(origin, outcome) %>%
  summarise(aantal = n()) %>%
  pivot_wider(names_from = outcome, values_from = aantal) %>%
  ungroup() %>%
  select(-origin) %>%
  as.matrix() %>%
  chisq.test()

# onset-to-death distribution
Linelist_raw %>%
  mutate(onset2death = death_date + ymd("1900-01-01") - as.Date(onset_date)) %>%
  ggplot(aes(x = onset2death)) +
  geom_histogram()


### time-dependent death rate in urban areas
# conclusion: no trend, so no significant issues with censoring until day 133
Linelist_raw %>%
  filter(origin == "urban") %>%
  mutate(onset_date = as.Date(report_date),
         onset_week = epiweek(onset_date),
         outcome = if_else(is.na(outcome), "Alive", outcome),
         deadYN = outcome == "Died") %>%
  group_by(onset_week) %>%
  summarise(pdead = sum(deadYN)/n(),
            lowp = pdead - 1.96 * sqrt(pdead * (1 - pdead) / n()),
            highp = pdead + 1.96 * sqrt(pdead * (1 - pdead) / n())) %>%
  ungroup() %>%
  ggplot(aes(x = onset_week, y = pdead)) + 
  geom_pointrange(aes(ymin = lowp, ymax = highp))

### time-dependent hospitalisation rate: no trend, so no significant issues with censoring until day 133
Linelist_raw %>%
  filter(origin == "urban") %>%
  mutate(onset_date = as.Date(report_date),
         onset_week = epiweek(onset_date),
         outcome = if_else(is.na(outcome), "Alive", outcome),
         hospYN = !is.na(hospitalisation_date)) %>%
  group_by(onset_week) %>%
  summarise(phosp = sum(hospYN)/n(),
            lowp = phosp - 1.96 * sqrt(phosp * (1 - phosp) / n()),
            highp = phosp + 1.96 * sqrt(phosp * (1 - phosp) / n())) %>%
  ungroup() %>%
  ggplot(aes(x = onset_week, y = phosp)) + 
  geom_pointrange(aes(ymin = lowp, ymax = highp))

### probability of hospitalisation
# true infection prevalence at day 133
infectionprevalence <- epi.prev(pos = 166, tested = 2008, se = 0.80, sp = 0.95, method = "blaker", unit = 100, conf.level = 0.95)$tp
total_infected <- infectionprevalence * 2281951 # 2019 Cambodia census for Phnom Penh

# calculated hospitalisation and death rates -> death rate similar to seasonal flu
Linelist_raw %>%
  mutate(report_date = as.Date(report_date)) %>%
  filter(report_date <= ymd("2022-12-31") + 133,
         origin == "urban") %>%
  summarise(
    totalcases = n(),
    totalhospitalised = sum(!is.na(hospitalisation_date)),
    totaldeaths = sum(outcome == "Died", na.rm = T)
  )  %>%
  ungroup() %>%
  mutate(hospitalisation_rate = totalhospitalised / total_infected,
         death_rate = totaldeaths / total_infected)
  
