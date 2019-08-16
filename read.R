# Read in files from input dir --------------------------------------------
library(tidyverse)

#Set WD
if(getwd() != "/Users/williamelson/Dropbox/r_projects/professional_r/Iquitos_cohort"){
  setwd("Dropbox/r_projects/professional_r/Iquitos_cohort/")  
}

#Create vector of csv files in the input dir
files <- str_c("input/", list.files("input/"))

#Create vector of names 
file_names <- str_replace(list.files("input/", pattern = ".csv"), ".csv", "")

#Read into list of dfs
list_dfs <- map(files, function(x){read_csv(x)}) 

#Assign each df in the list to df in global environment
invisible(map2(file_names, list_dfs, ~ assign(.x, .y, envir = .GlobalEnv)))


