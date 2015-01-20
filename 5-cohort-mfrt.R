# 5-cohort-mfrt.R

library(survival)
library(ggplot2)
library(gridExtra)
library(plyr)
library(tidyr)
library(dplyr)

sources <- paste0('scripts/', list.files('scripts', pattern="*.R"))
sapply(sources, source, .GlobalEnv)

load("data/spells.rda")

spells2 <- make_variables(spells)

x <- spells %>% 
  mutate(
    age1 = diff_days(event_date, m_birth)/365.25,
    age2 = diff_days(event2_date, m_birth)/365.25,
    affiliation = ifelse(affiliation == 4, 2, affiliation),
    affiliation = factor(affiliation, labels = c("State","Free-church")),
    cohort      = factor(
      cohort, 
      labels = c("Marriage cohort 1", "Marriage cohort 2", "Birth cohort")),
    event = ifelse(event2_type == "birth", 1, 0)
  )

x_age_split <- survSplit(data = x, cut = seq(15,50, 5), end = "age2", event = "event", start = "age1", episode = "group") %>% 
  mutate(group = (group * 5) + 10)

y <- x_age_split %>% 
  mutate(
    age_group = group,
    duration  = age2 - age1
  ) %>% 
  filter(age_group > 15 & age_group < 50) %>%
  select(affiliation, cohort, duration, event, age_group)


calc_tmft <- function(x) {
  x <- x %>% group_by(affiliation, cohort, age_group) %>% 
  summarise(
    tmfr = sum(event)/sum(duration), 
    n = sum(event),
    se = tmfr/sqrt(n),
    ci = 1.96 * se
  ) %>% 
  group_by(affiliation, cohort) %>% 
  mutate(indexed_tmfr = tmfr/tmfr[age_group == min(age_group)])
}

ycr   <- calc_tmft(y)
yc    <- calc_tmft(mutate(y, affiliation = 'All'))
yr    <- calc_tmft(mutate(y, cohort = 'All'))
yall  <- calc_tmft(mutate(y, cohort = 'All', affiliation = 'All'))

sum_tmfr <- rbind(
  ycr %>% 
  group_by(affiliation, cohort) %>% 
  summarise(
    tmfr    = sum(tmfr*5, na.rm=T),
    n       = sum(n),
    se      = tmfr/sqrt(n),
    ci_low  = tmfr - (se * 1.96),
    ci_high = tmfr + (se * 1.96)
  ),
  yc %>% 
  group_by(affiliation, cohort) %>% 
  summarise(
    tmfr    = sum(tmfr*5, na.rm=T),
    n       = sum(n),
    se      = tmfr/sqrt(n),
    ci_low  = tmfr - (se * 1.96),
    ci_high = tmfr + (se * 1.96)
  )
)

y <- rbind(yall, yc, yr, ycr)

colnames(sum_tmfr)[1] <- "Affiliation"

ggplot(sum_tmfr, aes(cohort, tmfr, group = Affiliation, linetype = Affiliation)) + 
  geom_point() + 
  geom_line() + 
  labs(x = '', y = 'TMFR') +
  cust_theme() + 
  theme(legend.position = "bottom")
ggsave('figures/figure5-ctmfr.jpg', height = 3.5)

tmfr_plot <- y %>% 
  filter(affiliation != "All" & cohort != "All")

ggplot(tmfr_plot, aes(age_group, tmfr, linetype=cohort)) + 
  geom_line() + 
  # scale_colour_manual(values = c("#7D00C2", "#87CA00")) +
  facet_wrap(~affiliation, ncol=4) +
  labs(x = "Age group", y = "TMFR") +
  cust_theme() + 
  theme(
    legend.position="bottom",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
ggsave('figures/figure6-age_cmfr.jpg',height = 3.5)
