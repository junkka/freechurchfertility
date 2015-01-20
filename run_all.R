# run_all.R

sources <- list.files(pattern="[0-9].*.R")
sapply(sources, source, .GlobalEnv)
