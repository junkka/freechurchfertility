# 5-descriptive-analysis.R

library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)

# load sources
sources <- paste0('libs/', list.files('libs', pattern="*.R"))
sapply(sources, source, .GlobalEnv)


load('data/spells.rda')

simp <- simple_spells(spells) %>% 
  # reccode variables
  transmute(
    # recode affiliation 4 (birth cohort free-church affiliation) to 2 (free-church affiliation)
    affiliation = ifelse(affiliation == 4, 2, affiliation),
    affiliation = factor(affiliation, labels = c("State","Free-church")),
    cohort      = factor(
                      cohort, 
                      labels = c("Marriage cohort 1", "Marriage cohort 2", "Birth cohort")),
    economy = ifelse(economy == 2, 1, economy),
    economy = factor(economy, labels=c("Rural","Coastal")),
    event = ifelse(event2_type == "birth", 1, 0),
    marr_age_wom = diff_days(marr_date, m_birth)/365.25,
    mothers_age = as.numeric(difftime(stop, m_birth, units="days")/365.25),
    pid, eid, marr_date, m_birth, event2_type,
    start, stop, prevint, nextint, closed, last, last_date, parity
  ) %>% arrange(pid, stop) %>% 
  group_by(pid) %>% 
  # To controll for complete observations create age at last event and an indicator for  
  #   observed last birth event by couple
  mutate(
    last_age = max(mothers_age, na.rm = T),
    last_ev = max(last)
  ) %>% ungroup() %>% 
  mutate(
    complete_reproduction = ifelse(last_age >= 45 & last_ev == 1, TRUE, FALSE)
  )

# Get marriage_age
marr_dat <- simp %>% 
  filter(
    parity == 0,
    complete_reproduction
  ) %>% 
  transmute(
    marr_age_wom  = diff_days(start, m_birth)/365.25,
    cohort, affiliation, economy, pid
  ) %>% 
  filter(marr_age_wom <= 45 & marr_age_wom >= 15)

# get time between marriage and first birth
first_dat <- simp %>% 
  filter(
    parity == 0,
    event  == 1
  ) %>% transmute(
    first_int = diff_days(stop, marr_date)/365.25,
    cohort, affiliation, economy, last, closed
  ) 

# get age at last birth
last_dat <- simp %>% 
  filter(
    complete_reproduction,
    last == 1
  ) %>% 
  transmute(
    last_age_wom = diff_days(stop, m_birth)/365.25,
    cohort, affiliation, economy, eid, pid
  ) %>% 
  group_by(pid) %>% 
  filter(rank(eid, ties.method="first")==1, pid %in% marr_dat$pid)

# get time between closed births
int_dat <- simp %>% 
  filter(
    closed == 1,
    event == 1,
    parity > 0
  ) %>% transmute(
    interval = diff_days(stop, start)/365.25,
    cohort, affiliation, economy
  )

# make marriage age plot, summarise marriage age by cohort and affiliation
marr <- marr_dat %>% 
  group_by(cohort, affiliation) %>% 
  summarise(value = mean(marr_age_wom, na.rm = T)) 
n <- marr_dat %>% group_by(cohort) %>% summarise(n = n())
levels(marr$cohort) <- paste(levels(marr$cohort), n$n, sep = '\nn = ')

p1 <- ggplot(marr, aes(cohort, value, group = affiliation, linetype = affiliation)) + 
  geom_line() +
  geom_point() +
  ylim(min(marr$value) ,min(marr$value) + 10) +
  labs(x = 'Cohort', y = 'Years') +
  cust_theme() + 
  theme(
    legend.position="bottom")
ggsave('figures/figure7-marriage-age.jpg', height = 3, plot = p1)

# make first birth plot, summarise by cohort and affiliation
first <- first_dat %>% 
  filter(first_int < 5) %>% 
  group_by(cohort, affiliation) %>% 
  summarise(value = mean(first_int, na.rm = T)) 
n <- first_dat %>% group_by(cohort) %>% summarise(n = n())
levels(first$cohort) <- paste(levels(first$cohort), n$n, sep = '\nn = ')

p2 <- ggplot(first, aes(cohort, value, group = affiliation, linetype = affiliation)) + 
  geom_line() +
  geom_point() +
  ylim(min(first$value), min(first$value) + 0.4) +
  labs(x = 'Cohort', y = 'Years') +
  cust_theme() + 
  theme(
    legend.position="bottom")
ggsave('figures/figure8-first-interval.jpg', height = 3, plot = p2)

# make age at last birth plot, summarise by cohort and affiliation
last <- last_dat %>% 
  group_by(cohort, affiliation) %>% 
  summarise(value = mean(last_age_wom, na.rm = T)) 
n <- last_dat %>% group_by(cohort) %>% summarise(n = n())
levels(last$cohort) <- paste(levels(last$cohort), n$n, sep = '\nn = ')

p3 <- ggplot(last, aes(cohort, value, group = affiliation, linetype = affiliation)) + 
  geom_line() +
  geom_point() +
  ylim(min(last$value), min(last$value) + 8) +
  labs(x = 'Cohort', y = 'Years') +
  cust_theme() + 
  theme(
    legend.position="bottom")
ggsave('figures/figure9-stop-age.jpg', height = 3, plot = p3)

# make time between births plot, summarise by cohort and affiliation
interval <- int_dat %>% 
  group_by(cohort, affiliation) %>% 
  summarise(value = mean(interval, na.rm = T))
n <- int_dat %>% group_by(cohort) %>% summarise(n = n())
levels(interval$cohort) <- paste(levels(interval$cohort), n$n, sep = '\nn = ')

p4 <- ggplot(interval, aes(cohort, value, group = affiliation, linetype = affiliation)) +
  geom_line() +
  geom_point() +
  ylim(min(interval$value), min(interval$value) + 1.1) +
  labs(x = 'Cohort', y = 'Years') +
  cust_theme() + 
  theme(
    legend.position="bottom")
ggsave('figures/figure10-spacing.jpg', height = 3, plot = p4)
