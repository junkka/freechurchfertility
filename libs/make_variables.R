#' Make variables for analysis
#'
#' Transforms variables in spells data for event history analysis
#'
#' @param spells data.frame containing fertility spells


make_variables <- function(spells) {

  # Create event, starting and stoping time for every spell
  spells  = spells %>% 
    mutate(
      # event
      event = ifelse(event2_type == "birth", 1, 0),

      # Analysis time form last birth
      start  = 0,
      stop   = 0,
      start  = as.numeric(difftime(event_date, prev_birth, units="days")/30.5),
      stop   = as.numeric(difftime(event2_date, prev_birth, units="days")/30.5),

      # Analysis time form first birth
      start2 = as.numeric(difftime(event_date, first_birth, units="days")/30.5),
      stop2  = as.numeric(difftime(event2_date, first_birth, units="days")/30.5),
      start3 = as.numeric(difftime(event_date, m_birth, units="days")/30.5),
      stop3  = as.numeric(difftime(event2_date, m_birth, units="days")/30.5),


      # Create late entry of X months
      first_diff = 9 - start,
      start  = ifelse(first_diff > 0, 9, start),
      start2 = ifelse(first_diff > 0, start2 + first_diff, start2),
      start3 = ifelse(first_diff > 0, start3 + first_diff, start3)
    ) %>% 
    filter(
      start2 < stop2 & start < stop
    ) %>% 
    # recode variables
    mutate(
      # combine urban and costal parishes to one group
      economy     = ifelse(economy == 2, 1, economy),
      economy     = factor(economy, labels=c("Rural","Coastal")),
      cohort      = factor(
                      cohort, 
                      labels = c("Marriage cohort 1","Marriage cohort 2", "Birth cohort")),
      # recode affiliation 4 (birth cohort free-church affiliation) to 2 (free-church affiliation)
      affiliation = ifelse(affiliation == 4, 2, affiliation),
      affiliation = factor(affiliation, labels = c("State","Free-church")),
      # husbands occupational group, combine high and middle to one group
      occupation  = ifelse(occupation == 1, 2, occupation),
      occupation  = factor(occupation, 
                      labels = c("Upper/Middle","Farmer","Farmworker","Industrial worker","other")),
      occupation  = relevel(occupation, "Farmer"),
      # label gender of surviving children
      offspring   = factor(offspring, 
                      labels = c("Mixed", "Only sons", "Only daughters", "None")),
      # create age group for mother at previous birth
      mothers_age = as.numeric(difftime(prev_birth, m_birth, units="days")/365.25),
      age_group   = cut(
                      mothers_age, 
                      breaks=c(15,20,25,30,35,40,50), 
                      labels=c("15-19","20-24","25-29","30-34","35-39","40+"),
                      right = FALSE),
      first       = ifelse(event_date == first_birth, 1, 0),
      exit        = ifelse(event2_type == "end", 1, 0),
      start_date  = event_date,
      stop_date   = event2_date,
      marr_age    = as.numeric(difftime(marr_date, m_birth, units="days")/365.25)
    ) %>% 
    select(
      eid, 
      pid, 
      start,
      stop,
      start2,
      stop2,
      start3,
      stop3,
      event,
      parity,
      age_group,
      mothers_age,
      marr_age,
      marr_date,
      infant_death,
      affiliation,
      occupation,
      offspring,
      economy,
      cohort,
      # cohort2,
      first,
      exit,
      start_date,
      stop_date,
      marr_date,
      complete_reproduction
    )
  # recode NA valus in infant death as child alive (0)
  spells$infant_death[is.na(spells$infant_death)] <- 0
  
  return(spells)
}

#' Simplify spells
#'
#' Reduce spells data to only birth and end events by removing all 
#'   time varying variables and recalculate intervals between events.
#'
#' @param x spells data.frame

simple_spells <- function(x) {
  msimp <- x %>% group_by(pid) %>% summarise(minp = min(parity)) %>% filter(minp == 0)

  simp1 <- x %>% filter(
    event2_type %in% c('birth', 'end'),
    pid %in% msimp$pid
  ) %>% mutate(
    start = as.POSIXct(
      ifelse(is.na(prev_birth), marr_date, prev_birth), 
      "%Y-%m-%d", 
      origin = '1970-01-01'),
    stop = event2_date
  ) %>% select(
    eid:stop, -event2_date, -prev_birth, -event_date
  ) %>% arrange(pid, stop)
  simp1$eid <- as.numeric(rownames(simp1))
  
  # Add next event data by self join
  simp3 <- simp1 %>% 
    mutate(eid = eid - 1) %>% 
    select(eid, pid, nexti = stop, nexte = event2_type) %>% 
    left_join(simp1, ., by = c('pid', 'eid'))

  # create closed and stopping
  # stoping = first nexti > 5 years
  # or if next event is end and end is at or after age 45 
  stoping <- simp3 %>% mutate(
    prevint = as.numeric(difftime(stop, start, units="days")/365.25),
    nextint = as.numeric(difftime(nexti, stop, units="days")/365.25),

    age     = as.numeric(difftime(nexti, m_birth, units = "days")/365.25),
    age_stop = ifelse(nexte == 'end' & age >= 45, TRUE, FALSE),
    last    = ifelse(nextint >= 5 | age_stop, 1, 0)
  ) %>% select(-age, -age_stop)  %>% arrange(pid, stop)

  stop_only <- stoping %>% filter(last == 1) %>% 
    group_by(pid) %>% 
    filter(rank(eid, ties.method="first")==1) %>% 
    select(pid, last_date = stop)

  simp4 <- left_join(stoping, stop_only, by='pid') %>% 
    mutate(
      closed = ifelse(stop < last_date, 1, 0),
      last   = ifelse(last_date == stop, 1, 0)
    )
  return(simp4)
}