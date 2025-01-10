

library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(here)

df <- read_csv(here("inst", "extdata", "westerband_2022_wdate.csv"))

# take a small sample
df_filtered <- df |>
  select(Site, Species, Latitude, Longitude, LMA_g.m2, LeafN_area_g.m2, PNUE) |>
  slice(200:300)

df_nested <- df_filtered |>
  group_split(row_number(), .keep = FALSE) %>%
  purrr::map_dfr( ~ .x |>
                    nest(measurementOrFact = c(LMA_g.m2, LeafN_area_g.m2, PNUE)))
  # nest(measurementOrFact = c(LMA_g.m2, LeafN_area_g.m2, PNUE))

test_string <- c("g/m2", "g/m2", "who knows")
another_string <- c("leaf mass per area", "leaf Nitrogen per area", "PNUE")

df_nested |>
  dplyr::mutate(
    measurementOrFact = purrr::map(
      measurementOrFact,
      ~ .x |>
        pivot_longer(names_to = "column_name",
                     values_to = "measurementValue",
                     cols = everything()) |>
        mutate(
          measurementUnit = test_string,
          measurementType = another_string
          )
      )) |>
  unnest(measurementOrFact)


very_nested <- df_nested |>
  nest(data = c(Species, Latitude, Longitude, measurementOrFact))

very_nested


very_nested |>
  unnest(data)




test <- df_filtered |>
  # slice(200:300) |>
  use_measurements(cols = c(LMA_g.m2, LeafN_area_g.m2, PNUE),
                   unit = c("g/m2", "g/m2", "something else"),
                   type = c("leaf mass per area", "leaf nitrogen per area", "gibberish"))

test

test |>
  slice(1:3) |>
  unnest(measurementOrFact)


number <- 7490

floor(log10(number)) + 1














library(tibble)
library(lubridate)

df <- tibble(
  latitude = c(-35.310, "-35.273"), # deliberate error for demonstration purposes
  longitude = c(149.125, 149.133),
  date = c("14-01-2023", "15-01-2023"),
  time = c("10:23:00", "11:25:00"),
  month = c("January", "February"),
  day = c(100, 101),
  species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
  n = c(2, 3),
  crs = c("WGS84", "WGS8d"),
  country = c("Australia", "Denmark"),
  continent = c("Oceania", "Europe")
)


df |>
  use_occurrences(basisOfRecord = "humanObservation")




