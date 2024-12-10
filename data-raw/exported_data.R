# This script builds all information stored within corella/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

devtools::load_all()
library(dplyr)
library(glue)
library(readr)
library(usethis) # adding content to /data

# get full Dawrin Core dataset
# note given by TDWG looks about right
terms_versions_raw <- read_csv("https://raw.githubusercontent.com/tdwg/dwc/refs/heads/master/vocabulary/term_versions.csv")
darwin_core_terms <- terms_versions_raw |>
  filter(status == "recommended",
         grepl("Property$", rdf_type)) |>
  rename(url = term_iri,
         term = term_localName) |>
  mutate(class = basename(organized_in)) |>
  filter(!duplicated(term)) |>
  select(class, term, url, definition, comments, examples) |>
  mutate(class = case_when(class == 1.1 ~ "Generic",
                           class == "terms" ~ "Generic",
                           is.na(class) ~ "Generic",
                           .default = class)) |>
  filter(class != "UseWithIRI")

write_csv(darwin_core_terms, "./data-raw/darwin_core_terms.csv") # store on github as a backup

# add to package
darwin_core_terms <- read_csv("./data-raw/darwin_core_terms.csv")
country_codes <- read_csv("./data-raw/wikipedia_country_codes.csv") |>
  rename(name = country_name,
         year_added = year) |>
  relocate(name, .before = code)
# Fix bug where 'Namibia' country code is imported as 'NA'
country_codes$code[which(is.na(country_codes$code))] <- "NA"

# add to r/sysdata.rda
use_data(
  darwin_core_terms,
  country_codes,
  internal = FALSE,
  overwrite = TRUE)
