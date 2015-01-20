# 6-timing.R

library(xtable)
library(survival)
library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)

# load sources
sources <- paste0('scripts/', list.files('scripts', pattern="*.R"))
sapply(sources, source, .GlobalEnv)

load("data/spells.rda")
spells2 <- make_variables(spells)

f <- spells2 %>% 
  mutate(
    start2 = start2/12,
    stop2  = stop2/12
  )
res <- ddply(f, .(cohort), function(a) {
  fit <- coxph(Surv(start2, stop2, event) ~strata(affiliation), data = a)
  ggsurv(fit)
})
colnames(res)[10] <- "Affiliation"
ggplot(res, aes(time, cumhaz, group = Affiliation, linetype = Affiliation)) + 
  geom_step() +
  # scale_colour_manual(values = c("#7D00C2", "#87CA00")) +
  coord_cartesian(xlim = c(0, 30)) + 
  facet_grid(.~cohort) + 
  cust_theme() +
  labs(x = 'Years since first birth', y = 'Cumulative hazard') +
  theme(legend.position = "bottom")
ggsave('figures/figure11-na.jpg', height = 3)

fit_context <- coxph(
  Surv(start2, stop2, event) 
    ~ affiliation 
    + age_group + strata(infant_death)  
    + occupation + offspring
    + economy + cohort
    + strata(parity) + cluster(pid), 
  data = spells2)

fit_inter <- coxph(
  Surv(start2, stop2, event)
    ~ affiliation
    + age_group + strata(infant_death) 
    + occupation + offspring
    + economy + cohort 
    + affiliation*cohort
    + strata(parity) + cluster(pid), 
  data = spells2)

fit_marr1 <- coxph(
  Surv(start2, stop2, event) 
    ~ affiliation 
    + age_group + strata(infant_death)
    + occupation + offspring
    + economy
    + strata(parity) + cluster(pid), 
  data = spells2[spells2$cohort == "Marriage cohort 1", ])

fit_marr2 <- coxph(
  Surv(start2, stop2, event) 
    ~ affiliation
    + age_group + strata(infant_death)
    + occupation + offspring 
    + economy
    + strata(parity) + cluster(pid), 
  data = spells2[spells2$cohort == "Marriage cohort 2", ])

fit_birth <- coxph(
  Surv(start2, stop2, event) 
    ~ affiliation 
    + age_group + strata(infant_death)
    + occupation + offspring   
    + economy
    + strata(parity) + cluster(pid), 
  data = spells2[spells2$cohort == "Birth cohort", ])

fit_free <- coxph(
  Surv(start2, stop2, event) 
    ~ age_group + strata(infant_death)
    + occupation + offspring
    + economy + cohort
    + strata(parity) + cluster(pid), 
  data = spells2[spells2$affiliation == "Free-church", ])

fit_state <- coxph(
  Surv(start2, stop2, event) 
    ~ strata(infant_death)  
    + age_group
    + occupation + offspring
    + economy + cohort
    + strata(parity) + cluster(pid), 
  data = spells2[spells2$affiliation == "State", ])

res  <- res_table(fit_context, fit_state, fit_free, cilevel = 0.95)
levels(res$var) <- c(
  'Affiliation', 'Age group', 'Occupation', 'Gender of surviving children', 'Region', 
  'Cohort', 'Summary')
pres <- format_rt(res)

colnames(pres) <- c('Variable', 'Full model', 'Free-church', 'State church')

print(
  xtable(
    pres,
    caption="Probability of having another birth for all couples and by religious 
    affiliation, Cox proportional hazard regressions."
  ),
  file="figures/table2.tex",
  table.placement = "!h",
  caption.placement="bottom",
  comment=FALSE,
  include.rownames=FALSE,
  type="latex",
  add.to.row = comment(pres)
)

res  <- res_table(fit_marr1, fit_marr2, fit_birth, fit_inter, cilevel = 0.95)
levels(res$var) <- c(
  'Affiliation', 'Age group', 'Occupation', 'Gender of surviving children', 'Region', 
  'Cohort', 'Interactions', 'Summary')
pres <- format_rt(res)
colnames(pres) <- c(
  'Variable', 'Marriage cohort 1', 'Marriage cohort 2',  
  'Birth cohort', 'Interaction')

p_table <- xtable(
  pres,
  caption="Probability of having another birth by cohort, Cox proportional hazard regressions."
)
align(p_table) <- "lm{5.5cm}m{1.6cm}m{1.6cm}m{1.6cm}m{1.6cm}"

print(
  p_table,
  file="figures/table3.tex",
  size="small",
  table.placement = "!h",
  caption.placement="bottom",
  comment=FALSE,
  include.rownames=FALSE,
  type="latex",
  add.to.row = comment(pres)
)

ff <- function(param) {
  # Function content
  births <- param %>% filter(event == 1)
  res <- rbind(
    births %>% 
      mutate(var = 'Affiliation', level = affiliation) %>% 
      group_by(var, level) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n / sum(n)),
    births %>% 
      mutate(var = 'Age group', level = age_group) %>% 
      group_by(var, level) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n / sum(n)),
    births %>% 
      mutate(var = 'Infant death', level = infant_death) %>% 
      group_by(var, level) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n / sum(n)),
    births %>% 
      mutate(var = 'Gender of surviving children', level = offspring) %>% 
      group_by(var, level) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n / sum(n)),
    births %>% 
      mutate(var = 'Occupation', level = occupation) %>% 
      group_by(var, level) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n / sum(n)),
    births %>% 
      mutate(var = 'Region', level = economy) %>% 
      group_by(var, level) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n / sum(n)),
    births %>% 
      mutate(var = 'Cohort', level = cohort) %>% 
      group_by(var, level) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n / sum(n))
  )
  return(res)
}

spells2$infant_death <- factor(as.logical(spells2$infant_death))
cres <- ddply(spells2, .(cohort), ff) %>% rename(model = cohort)
rres <- ddply(spells2, .(affiliation), ff) %>% rename(model = affiliation)
res_long <- rbind(cres, rres, ff(spells2) %>% mutate(model = 'All'))

res <- res_long %>% 
  mutate(value = paste(round(freq * 100, 1), ' (', n, ')', sep = '')) %>% 
  select(model, value, level, var) %>% 
  spread(model, value)

# pres <- format_rt(res)
res$index   <- seq_len(nrow(res))
res$level   <- paste(' - ', res$level)
vars        <- res[c(1:length(unique(res$var))), ]
vars$level  <- unique(res$var)
vars[ ,!(names(res) %in% c('level'))] <- NA
vars$index  <- c(1,3,9,11,15,20,22)
res$index   <- res$index + 0.1
pres        <- rbind(res, vars)
pres        <- pres[order(pres$index), ]

print(
  xtable(pres[ ,c(1,3:8)],
         caption="Frequency distribution of birth events by variable and model (n).",
         align = c("l", "l", rep("c", 3), rep("c", 3))
         ),
  file="figures/table4.tex",
  size="small",
  table.placement = "!h",
  caption.placement="bottom",
  comment=FALSE,
  include.rownames=FALSE,
  type="latex",
  floating.environment='sidewaystable'
  )

