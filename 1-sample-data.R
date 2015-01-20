# 1-sample-data.R


# The main data for this study consists of digitalized parish registers from the Demographic Database, Umeå university. As the original data from DDB cannot be distributed publicly we provide a generated sample for testing of code. 

# ## Original spells data

# The original dataset consists of event history spells, where each row is a time spell in a couples life from one event to another. In this data the spells consists of events of marriage, births, child deaths and end of marriage. 

# ### Variables

# | Variable              | Type    | Descripition
# |:----------------------|:--------|:----------------------------------|
# | eid                   | num     | Unique event id                   |
# | pid                   | num     | Unique marriage id                |
# | event_date            | POSIXct | Start of observation              |
# | event2_date           | POSIXct | End of observation                |
# | event2_type           | chr     | End event type                    |
# | prev_birth            | POSIXct | Date of birth of previously born child |
# | parity                | num     | Parity of previously born child   |
# | first_birth           | POSIXct | Date of birth of first born child |
# | occupation            | int     | Occupational group                |
# | affiliation           | num     | Religious affiliation             |
# | economy               | int     | Main economic region residense    |
# | cohort                | num     | Cohort                            |
# | m_birth               | POSIXct | Mothers birth date                |
# | marr_date             | POSIXct | Marriage date                     |
# | complete_reproduction | int     | Indicator of complete reproductive history |
# | offspring             | num     | Offspring set                     |
# | infant_death          | num     | Indicator dor if previously born childs died within a year of birth |

# ## Generated spells data

# The generated data is built from the `fert` data in the `eha` package [@brostrom2014]. This data consists of fertility spells from 19th century Skellefteå, Sweden. The `fert` data is combined with sampled data from the original spells data. However as the `fert` data only consist of marriage, birth and end events the sample data do not have any child death events. Thereby the time varying variables of offspring and infnat_death is set at birth events, randomly sampled from the original data.

# ## Sample generation

# The generation of the sample is made through this function. where `x` is the original spells data object.


make_sample <- function(spells) {
  library(devtools)
  library(eha)
  library(lubridate)
  library(plyr)
  library(dplyr)

  data(fert, package='eha')
  times <- round(nrow(spells)/nrow(fert))
  new_fert <- fert
  for (i in 2:5){
    new_fert <- fert %>% mutate(id = id + (nrow(fert)*i)) %>% 
      rbind(new_fert)
  }
  n_spells  = nrow(new_fert)
  n_mothers = nrow(new_fert[is.na(new_fert$prev.ivl), ])
  set.seed(42)

  # Get real distribution of occupation, religion and economy by pid (couple id)
  spells_couples <- spells %>% 
    group_by(pid) %>%
    filter(rank(eid, ties.method="first")==1) %>% 
    select(
      occupation, affiliation, economy, cohort, complete_reproduction) %>% 
    filter(!is.na(cohort))  
  
  mother_sample <- sample_n(as.data.frame(spells_couples), n_mothers)
  # get all mothers in new_fert data 
  mothers <- new_fert %>% filter(is.na(prev.ivl)) %>% 
    # create time invarying variables
    mutate(
      cohort      = mother_sample$cohort,
      occupation  = sample(spells_couples$occupation, n_mothers),
      affiliation = sample(spells_couples$affiliation, n_mothers),
      economy     = sample(spells_couples$economy, n_mothers),
      m_birth     = lubridate::ymd(paste(year - age, '-07-02')),
      marr_date   = lubridate::ymd(paste(year, '-07-02')),
      first_birth = marr_date + lubridate::days(round(next.ivl * 365.25)),
      complete_reproduction = mother_sample$complete_reproduction
    ) %>% 
    select(id, first_birth, occupation, affiliation, economy, cohort, m_birth, 
      marr_date, complete_reproduction)
  
  # Add time invarying variables to gen_spells
  gen_spells <- new_fert %>% 
    group_by(id) %>% 
    mutate(marr_dur = cumsum(next.ivl))
  
  gen_spells <- left_join(as.data.frame(gen_spells), mothers, by = 'id')  %>% 
    transmute(
      eid = as.numeric(rownames(new_fert)),
      pid = id,
      event_date  = marr_date + lubridate::days(round((marr_dur - next.ivl) * 365.25)),
      event2_date = marr_date + lubridate::days(round(marr_dur * 365.25)),
      event2_type = ifelse(event == 1, 'birth', 'end'),
      prev_birth  = event_date,
      parity,
      first_birth, 
      occupation, 
      affiliation, 
      economy, 
      cohort, 
      m_birth, 
      marr_date,
      complete_reproduction
    )
  gen_spells$prev_birth[gen_spells$parity == 0] <- NA
  # Create time varying variables
  spells <- as.data.frame(gen_spells) %>% 
    mutate(
      offspring     = sample(spells$offspring, n_spells),
      infant_death  = ifelse(parity == 0, 0, sample(spells$infant_death, n_spells))
    ) %>% 
    filter(!is.na(marr_date))

  environ <- environment()
  save(spells, file = 'data/spells_gen.rda', compress = 'xz')
  return(spells)
}

path <- 'data/spells_orig.rda'
if (file.exists(path)){
  # generate spells_gen
  load(path)
  spells <- make_sample(spells)
  # use original data
  file.copy(path, 'data/spells.rda', overwrite = TRUE)
} else {
  file.copy('data/spells_gen.rda', 'data/spells.rda', overwrite = TRUE)
}