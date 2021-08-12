# This file builds the data for the paper from all the raw sources. Warning: This will take a long
# time, due in large part to measurement models, but also big spatial joins.

# PACKAGES NECESSARY TO RUN THIS SCRIPT
required_packages   <- c("tidyverse", "areal", "sf", "janitor", "vroom", "tidycensus", "tigris", "psych", "lwgeom", "GPArotation")

# SCRIPT NAMES
processing_scripts  <- list.files("./syntax/data_processing/", full.names = T)



# RUN DATA PROCESSING
initial_start_time <- Sys.time()
for(script in processing_scripts){
  message(paste0("Running: ", script))
  start_time <- Sys.time() 
  source(script)
  message(paste0("Script time: ", 
                 round(difftime(Sys.time(), start_time, units = "secs"), 1),
                 " seconds. Total time so far: ",
                 round(difftime(Sys.time(), initial_start_time, units = "mins"), 1), 
                 " minutes." ))
}

# Note I have not suppressed any messages or warnings here so you'll get a bunch
# of non-critical ones. Main ones that look scary are where lavaan drops empty
# cases in the measurement models. These are respondents who answered none of
# the indicators. I could filter them out prior but the result would be the same
# and keeping it explicit is kind of nice.


