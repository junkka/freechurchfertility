# 3-descriptive.R

library(xtable)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)

# load sources
sources <- paste0('libs/', list.files('libs', pattern="*.R"))
sapply(sources, source, .GlobalEnv)

load("data/spells.rda")

# Get spells data for complete observations
# simle_spells() imported from libs/make_variables.R
complete <- simple_spells(spells) %>% 
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
  ) %>% 
  filter(complete_reproduction, marr_age_wom <= 45 & marr_age_wom >= 15)

# calculate complete marriages by cohort, affiliation and in total
com_cohort <- complete %>% select(pid, group = cohort) %>% 
  distinct() %>% 
  group_by(group) %>% 
  summarise(complete = n())
com_aff <- complete %>% select(pid, group = affiliation) %>% 
  distinct() %>% 
  group_by(group) %>% 
  summarise(complete = n())
com_all <- complete %>% mutate(group = "Total") %>% select(pid, group) %>% 
  distinct() %>% 
  group_by(group) %>% 
  summarise(complete = n())

# Get spells data for all observations
# make_variables() imported from libs/make_variables()
spells2 <- make_variables(spells)

# calculate number of marriages and observed births by cohort, affiliation and in total
marr <- spells2 %>% 
  select(pid, event, cohort) %>% 
  mutate(group = cohort) %>% 
  group_by(group) %>% 
  summarise(
    marriages = length(unique(pid)), 
    births = sum(event, na.rm=T)
  ) %>% cbind(com_cohort[ ,2])
aff <- spells2 %>% 
  select(pid, event, affiliation) %>% 
  mutate(group = affiliation) %>% 
  group_by(group) %>% 
  summarise(
    marriages = length(unique(pid)), 
    births = sum(event, na.rm=T)
  ) %>% cbind(com_aff[ ,2])
all <- spells2 %>% 
  select(pid, event) %>% 
  mutate(group = "Total") %>% 
  group_by(group) %>% 
  summarise(
    marriages = length(unique(pid)), 
    births = sum(event, na.rm=T)
  ) %>% cbind(com_all[ ,2])

# combine summary statistics
desc <- rbind(marr, aff, all)

colnames(desc) <- c('Group','Marriages','Births', 'Complete marriages')

# make table
print(
  xtable(as.data.frame(desc),
         caption="Number of events and marriges by cohort"
         ),
    file="figures/table1.tex",
    table.placement = "!h",
    caption.placement="bottom",
    comment=FALSE,
    include.rownames=FALSE,
    type="latex"
  )

# get first observation by couple
df_entry <- spells2 %>% 
  arrange(pid, start_date) %>% 
  group_by(pid) %>% 
  filter(row_number() == 1) %>% 
  mutate(event = 'entry', date = lubridate::year(marr_date)) %>% 
  select(event, date, cohort, affiliation)

# get last observation by couple
df_exit <- spells2 %>% 
  arrange(pid, stop_date) %>% 
  group_by(pid) %>% 
  filter(row_number() == max(row_number())) %>% 
  mutate(event = 'exit', date = lubridate::year(stop_date)) %>% 
  select(event, date, cohort, affiliation)

x <- rbind(df_entry, df_exit)
x2 <- as.data.frame(x) %>% rename(affiliation = cohort, cohort = affiliation)
x <- rbind(as.data.frame(x), x2)

p <- ggplot(x, aes(date)) +
  geom_histogram(data = x[x$event == "entry", ], 
    aes(date, fill = "black"), color = "white", alpha = 1, binwidth = 1) +
  geom_histogram(data = x[x$event == "exit", ], 
    aes(date, fill = "white"), color = "#3E3E3E", alpha = 0, binwidth = 1) +
  cust_theme() + 
  scale_fill_manual(name = 'Event', 
       values =c('black'='#909090','white'='white'), labels = c('Start','End')) + 
  labs(x = 'Year', y = 'Count')

p + facet_grid(cohort~. ,scales = "free") + theme(legend.position = "bottom")
ggsave(file = 'figures/figure3-source-desc.jpg', height = 8)