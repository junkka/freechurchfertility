# Alternative method - Age Cohort Period

# The Age-Period-Cohort model (APC) attemts to isolate the 
# seperate effects of the three parameters. This script 
# creates two seperate APC models for each religious affiliaton,
# and visualized the differences of the seperate age-period-cohort
# effects by affiliation.

library(Epi)
library(eha)
library(survival)
library(dplyr)

sources <- paste0('libs/', list.files('libs', pattern="*.R"))
sapply(sources, source, .GlobalEnv)

# ## Prepare data

# The APC model requires a dataset with age (A), period (P), 
# events (D) and person years (Y).

load("data/spells.rda")

d <- tbl_df(spells) %>% 
  select(
    pid, start_date = m_birth, event2_type, event_date, event2_date, affiliation
  ) %>% 
  mutate(
    event = ifelse(event2_type == "birth", 1, 0),
    age1 = diff_days(event_date, start_date)/365.25,
    age2 = diff_days(event2_date, start_date)/365.25,
    affiliation = ifelse(affiliation == 4, 2, affiliation),
    affiliation = factor(affiliation, labels = c("State","Free-church"))
  ) %>% 
  filter(age2 < 50 & age1 >= 15)

# Split rows at every age group

d2 <- survSplit(d, 
    cut = seq(15,49,1), 
    end = "age2", 
    event = "event", 
    start = "age1",
    episode = "A"
  ) %>% 
  mutate(A = A + 14)

# Summarise events and person years by affilation period and age_group

d3 <- tbl_df(d2) %>% 
  mutate(time = eha::toTime(start_date) + age2, P = as.integer(time)) %>% 
  group_by(P, A, affiliation) %>% 
  summarise(
    D = sum(event, na.rm = T), 
    Y = sum(age2-age1, na.rm = T), 
    A2 = mean(age2), 
    time = mean(time)
  ) %>% 
  ungroup %>% mutate(A = A2, P = time)

# Model

ref_c <- 1830
ref_p <- 1860
state_d <- filter(d3, affiliation == "State") %>% select(P, A, D, Y)
fit_state <- apc.fit(state_d, ref.c = ref_c, ref.p = ref_p, parm = "ACP")
free_d <- filter(d3, affiliation == "Free-church") %>% select(P, A, D, Y)
fit_free <- apc.fit(free_d, ref.c = ref_c, ref.p = ref_p, parm = "ACP")

# Create plot and save

jpeg("figures/apc_plot.jpg", height = 500, width = 1200)
fp <- apc.plot(fit_state)
apc.lines(fit_free, frame.par=fp, lty = 3)
legend("topright", "Affiliation", c("State", "Free-church"), lty = c(1, 3), bg = "white")
dev.off()