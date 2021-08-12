# This script generates the plots used in the article itself.

# PACKAGES NECESSARY TO BUILD
required_packages   <- c("tidyverse", "ragg", "tigris")

plot_scripts  <- list.files("./syntax/plots/", full.names = T)

initial_start_time <- Sys.time()
for(script in plot_scripts){
  message(paste0("Running: ", script))
  start_time <- Sys.time() 
  source(script)
  message(paste0("Script time: ", 
                 round(difftime(Sys.time(), start_time, units = "secs"), 1),
                 " seconds. Total time so far: ",
                 round(difftime(Sys.time(), initial_start_time, units = "mins"), 1), 
                 " minutes." ))
}

table_scripts  <- list.files("./syntax/tables/", full.names = T)

initial_start_time <- Sys.time()
for(script in table_scripts){
  message(paste0("Running: ", script))
  start_time <- Sys.time() 
  source(script)
  message(paste0("Script time: ", 
                 round(difftime(Sys.time(), start_time, units = "secs"), 1),
                 " seconds. Total time so far: ",
                 round(difftime(Sys.time(), initial_start_time, units = "mins"), 1), 
                 " minutes." ))
}
