# 5-cohort-mfrt.R

library(survival)
library(ggplot2)
library(gridExtra)
library(plyr)
library(tidyr)
library(dplyr)

sources <- paste0('libs/', list.files('libs', pattern="*.R"))
sapply(sources, source, .GlobalEnv)

load("data/spells.rda")

# make_variables() imported from libs/make_variables()
spells2 <- make_variables(spells)

# recode variables
x <- spells %>% 
  mutate(
    age1 = diff_days(event_date, m_birth)/365.25,
    age2 = diff_days(event2_date, m_birth)/365.25,
    # recode affiliation 4 (birth cohort free-church affiliation) to 2 (free-church affiliation)
    affiliation = ifelse(affiliation == 4, 2, affiliation),
    affiliation = factor(affiliation, labels = c("State","Free-church")),
    cohort      = factor(
      cohort, 
      labels = c("Marriage cohort 1", "Marriage cohort 2", "Birth cohort")),
    event = ifelse(event2_type == "birth", 1, 0)
  )

# split data into 5 year age groups from 15-50
x_age_split <- survSplit(data = x, cut = seq(15,50, 5), end = "age2", event = "event", start = "age1", episode = "group") %>% 
  mutate(group = (group * 5) + 10)

# Calculate duration in each observation
y <- x_age_split %>% 
  mutate(
    age_group = group,
    duration  = age2 - age1
  ) %>% 
  # Keep only age groups 20-25 - 40-45
  filter(age_group > 15 & age_group < 50) %>%
  select(affiliation, cohort, duration, event, age_group)


# calculate age-specific marital fertility rates
calc_tmft <- function(x) {
  x <- x %>% group_by(affiliation, cohort, age_group) %>% 
  summarise(
    tmfr = sum(event)/sum(duration), 
    n = sum(event)
  ) %>% 
  group_by(affiliation, cohort) %>% 
  mutate(indexed_tmfr = tmfr/tmfr[age_group == min(age_group)])
}

ycr   <- calc_tmft(y)
yc    <- calc_tmft(mutate(y, affiliation = 'All'))
yr    <- calc_tmft(mutate(y, cohort = 'All'))
yall  <- calc_tmft(mutate(y, cohort = 'All', affiliation = 'All'))

# calculate total marital fertility rates
sum_tmfr <- rbind(
    ycr %>% 
    group_by(affiliation, cohort) %>% 
    summarise(
      tmfr    = sum(tmfr*5, na.rm=T),
      n       = sum(n)
    ),
    yc %>% 
    group_by(affiliation, cohort) %>% 
    summarise(
      tmfr    = sum(tmfr*5, na.rm=T),
      n       = sum(n)
    )
  ) %>% 
  as.data.frame()

y <- rbind(yall, yc, yr, ycr)

colnames(sum_tmfr)[1] <- "Affiliation"

# make plots
ggplot(sum_tmfr, aes(cohort, tmfr, group = Affiliation, linetype = Affiliation)) + 
  geom_point() + 
  geom_line() + 
  ylim(min(sum_tmfr$tmfr), min(sum_tmfr$tmfr) + 2.6) + 
  labs(x = '', y = 'TMFR') +
  cust_theme() + 
  theme(legend.position = "bottom")
ggsave('figures/figure5-ctmfr.jpg', height = 3.5)

tmfr_plot <- y %>% 
  filter(affiliation != "All" & cohort != "All")

ggplot(tmfr_plot, aes(age_group, tmfr, linetype=cohort)) + 
  geom_line() + 
  facet_wrap(~affiliation, ncol=4) +
  labs(x = "Age group", y = "TMFR") +
  cust_theme() + 
  theme(
    legend.position="bottom",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
ggsave('figures/figure6-age_cmfr.jpg',height = 3.5)
