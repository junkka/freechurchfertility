# 4-aggregate-fertility.R

library(ggplot2)
library(gridExtra)
library(stringr)
library(tidyr)
library(plyr)
library(dplyr)

# load sources
sources <- paste0('scripts/', list.files('scripts', pattern="*.R"))
sapply(sources, source, .GlobalEnv)

load('data/agg_mfrt.rda')

# Formating column names
colnames(agg_mfrt)[2:6] <- str_replace(str_replace(colnames(agg_mfrt)[2:6], "X", ""), "\\.", "-")

# Gathering age-specific rates to one row per observation
amfrt <- agg_mfrt %>% 
  gather(age_group, frt, -Year, -Context) 

# Summarising age-specific rates by Year and Context
tmfrt <- amfrt %>% 
  group_by(Context, Year) %>% 
  summarise(
    TMFRT = (sum(frt)*5)/1000
  )

# Filtering contexts
tmfrt <- tmfrt %>% filter(Context %in% c('Sweden', 'Vasternorrland', 'StockholmC'))

# Craeting plot
p <- ggplot(tmfrt, aes(Year, TMFRT, group = Context, linetype = Context)) + 
  geom_line() + 
  xlim(1874, 1950) +
  ylim(2,9) +
  cust_theme() + 
  theme(legend.position = "right")
# Adding source information to plot
g <- arrangeGrob(p, 
  sub = textGrob("Source: Hofsten and Lundström (1976)", 
    x = 0.5, 
    hjust = 0.5, 
    vjust=0.1, 
    gp = gpar(fontfamily = "serif", fontsize = 9, col = "#5B5B5B")
  )
)
ggsave('figures/figure4-agg-fert.jpg', plot = g, width = 7, height = 2.8)
