################################################################################
##  data_functions.R: Functions for downloading, manipulating, and summarising
##  the Roland and Matter butterfly data.
##
##  Author: Andrew Tredennick (atredenn@gmail.com)
################################################################################

#' @name get_butterfly_data
#' @author Andrew Tredennick
#' @param save_file Logical (T/F) for whether to save the file as a csv in the current working directory. Default = TRUE.
#' @return A tibble dataframe of the Roland and Matter butterfly data.
get_butterfly_data <- function(save_file = TRUE){
  # Packages
  require(rdryad)
  require(tidyverse)
  require(readxl)
  
  # Download data
  dfile <- dryad_files(doi = "10.5061/dryad.tp324")
  data_file <- "roland_matter_dryad_data.xlsx"
  readme_file <- "roland_matter_README.txt"
  dryad_fetch(dfile[1], destfile = data_file)
  dryad_fetch(dfile[2], destfile = readme_file)
  
  # Read in and format data
  butterfly <- readxl::read_excel(data_file) %>%
    mutate(Nt = 10^(logNt)-0.5) %>% # back transform logNt, see README
    select(Year, meada, Nt, everything()) # do some reordering
  
  # Save as CSV, optional
  if(save_file){
    write_csv(butterfly, "roland_matter_dryad_data.csv")
  }
  
  # Remove the xls file
  file.remove(data_file)
  
  # Return the tibble dataframe
  return(butterfly)
}
