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
  dplyr::mutate(Nt = 10^(logNt)-0.5) %>% # back transform logNt, see README
  dplyr::select(Year, meada, Nt, everything()) %>% # do some reordering
  dplyr::rename(year = Year)

# Remove meadows without full time series
full_pops <- butterfly %>%
  dplyr::select(year, meada, Nt) %>%
  tidyr::spread(year, Nt) %>%
  na.omit() %>%
  tidyr::gather(year, Nt, -meada) %>%
  dplyr::mutate(year = as.integer(year))

# Right join to remove meadows without full time series
butterfly <- butterfly %>%
  dplyr::right_join(full_pops, by = c("year", "meada", "Nt"))

# Remove the xls file
file.remove(data_file)

# Save as CSV
write_csv(butterfly, "data-raw/roland_matter_dryad_data.csv")

# Add to package
devtools::use_data(butterfly, overwrite = TRUE, compress = 'xz')
devtools::use_data_raw()


