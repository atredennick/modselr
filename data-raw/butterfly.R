library(rdryad)
library(tidyverse)
library(readxl)

# Download data
dfile <- dryad_files(doi = "10.5061/dryad.tp324")
data_file <- "roland_matter_dryad_data.xlsx"
dryad_fetch(dfile[1], destfile = data_file)
# readme_file <- "roland_matter_README.txt"
# dryad_fetch(dfile[2], destfile = readme_file)

# Read in and format data
butterfly <- readxl::read_excel(data_file) %>%
  mutate(Nt = 10^(logNt)-0.5) %>% # back transform logNt, see README
  select(Year, meada, Nt, everything()) # do some reordering

file.remove(data_file)

# Save as CSV
write_csv(butterfly, "data-raw/roland_matter_dryad_data.csv")
devtools::use_data(butterfly, overwrite = TRUE, compress = 'xz')



