# 2-membership.R

library(ggplot2)
library(gridExtra)
library(zoo)
library(tidyr)
library(plyr)
library(dplyr)

sources <- paste0('scripts/', list.files('scripts', pattern="*.R"))
sapply(sources, source, .GlobalEnv)

load("data/movement.rda")
colnames(movement) <- tolower(colnames(movement)) 
colnames(movement) <- gsub('^me[dl]{2}', 'medl', colnames(movement))

replace_na <- function(x) {
  y <- ifelse(x %in% c(9999, 99999), NA, x)
}
replace_na0 <- function(x) {
  x <- replace_na(x)
  x[x == 0] <- NA
  return(x)
}
between <- function(a, b, d){
  x <- ifelse(a >= b & a <= d, TRUE, FALSE)
  x <- ifelse(is.na(b) | is.na(d), FALSE, x)
  return(x)
}

long <- movement %>% 
  gather(year, medl, -idnummer:-nedlagg2t) %>% 
  mutate(
    medl      = replace_na(medl),
    nedlagg1f = replace_na0(nedlagg1f),
    nedlagg1t = replace_na0(nedlagg1t),
    nedlagg2f = replace_na0(nedlagg2f),
    nedlagg2t = replace_na0(nedlagg2t),
    year      = as.integer(gsub('medl', '', year))
  ) %>% 
  filter(
    year >= inledar & 
    year <= slutar & 
    between(year, nedlagg1f, nedlagg1t) == FALSE & 
    between(year, nedlagg2f, nedlagg2t) == FALSE
  ) 

approx_na <- function(year, medl) {
  if (length(na.omit(medl)) < 2){
    return(as.numeric(medl))
  }
  b <- zoo(data.frame(year = year, medl = medl))
  bb <- na.approx(b)
  return(as.numeric(bb$medl))
}

long <- long %>% 
  group_by(idnummer) %>% 
  mutate(medl = approx_na(year, medl))

movement <- as.data.frame(long)


# parish codes of parishes in region
codes <- c(228104, 228109, 226203, 228111, 228113, 226202, 228110, 228105, 
  228106, 228103, 228101, 226201, 228108, 226204)

# filtering by codes and oraganisation type
b <- movement %>% 
  filter(forkod %in% codes & orgtypn == 'FRIK') %>% 
  # summarising membership by year
  group_by(year) %>% 
  summarise(members = sum(medl, na.rm=T)) %>% 
  select(years = year, members)

# Creating plot
p <- ggplot(b, aes(years, members)) + 
  geom_line() + 
  xlim(1881, 1921) + 
  ylim(0, 5000) +
  cust_theme() +
  labs(x = "Year", y = "Members")

# Adding source information to plot
g <- arrangeGrob(p, 
  sub = textGrob(
    "Source: Folkrörelsearkivet 1881-1950 (Andrae and Lundkvist 1998)", 
    x = 0.5, hjust = 0.5, vjust=0.1, 
    gp = gpar(fontfamily = "serif", fontsize = 9, col = "#000000")))
g
ggsave('figures/figure2-movement.jpg', plot = g,width = 7, height=3.2)