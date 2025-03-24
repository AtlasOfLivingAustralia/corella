
<!-- README.md is generated from README.Rmd. Please edit that file -->

# corella <a href="https://corella.ala.org.au"><img src="man/figures/logo.png" align="right" height="139" alt="corella website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/corella)](https://CRAN.R-project.org/package=corella)
[![Codecov test
coverage](https://codecov.io/gh/AtlasOfLivingAustralia/corella/graph/badge.svg)](https://app.codecov.io/gh/AtlasOfLivingAustralia/corella)
<!-- badges: end -->

## Overview

`corella` is an R package that helps users standardize their data using
the [*Darwin Core*](https://dwc.tdwg.org) data standard, used for
biodiversity data like species occurrences. `corella` provides tools to
prepare, manipulate and validate data against the standard’s criteria.
Once standardized, data can be subsequently shared as a [*Darwin Core
Archive*](https://ipt.gbif.org/manual/en/ipt/latest/dwca-guide#what-is-darwin-core-archive-dwc-a)
and published to open data infrastructures like the [Atlas of Living
Australia](https://www.ala.org.au) and [GBIF](https://www.gbif.org/).

`corella` was built, and is maintained, by the [Science & Decision
Support Team](https://labs.ala.org.au) at the [Atlas of Living
Australia](https://www.ala.org.au) (ALA). It is named for the Little
Corella ([*Cacatua
sanguinea*](https://bie.ala.org.au/species/https%3A//biodiversity.org.au/afd/taxa/34b31e86-7ade-4cba-960f-82a6ae586206)).
The logo was designed by [Dax Kellie](https://daxkellie.com/).

If you have any comments, questions or suggestions, please [contact
us](mailto:support@ala.org.au).

## Installation

Install from CRAN:

``` r
install.packages("corella")
```

Install the development version of `corella` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("AtlasOfLivingAustralia/corella")
```

## Usage

Here we have a small sample of example data containing observations of
cockatoos. Using corella we can convert our data to use Darwin Core
Standard.

``` r
library(corella)
library(tibble)

# A simple example of species occurrence data
df <- tibble(
  species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
  latitude = c(-35.310, "-35.273"), # deliberate error for demonstration purposes
  longitude = c(149.125, 149.133),
  eventDate = c("14-01-2023", "15-01-2023"),
  status = c("present", "present")
)

df
#> # A tibble: 2 × 5
#>   species                  latitude longitude eventDate  status 
#>   <chr>                    <chr>        <dbl> <chr>      <chr>  
#> 1 Callocephalon fimbriatum -35.31        149. 14-01-2023 present
#> 2 Eolophus roseicapilla    -35.273       149. 15-01-2023 present
```

One of the most important aspects of Darwin Core Standard is using
standard column names (Darwin Core *terms*). We can update column names
in our data to match Darwin Core terms with `set_` functions.

Each `set_` function name corresponds to the type of data, and argument
names correspond to the available Darwin Core terms to use as column
names. `set_` functions support data wrangling operations &
`dplyr::mutate()` functionality, meaning columns can be changed or fixed
in your pipe. `set_` functions will indicate if anything needs fixing
because they also automatically run checks on each column data to make
sure each column is in the correct format.

``` r
suppressMessages( # for readability

df |>
  set_coordinates(
    decimalLatitude = as.numeric(latitude), # fix latitude
    decimalLongitude = longitude
    ) |>
  set_scientific_name(
    scientificName = species
    ) |>
  set_datetime(
    eventDate = lubridate::dmy(eventDate) # specify date format
    ) |>
  set_occurrences(occurrenceStatus = status)

)
#> # A tibble: 2 × 5
#>   eventDate  decimalLatitude decimalLongitude scientificName    occurrenceStatus
#>   <date>               <dbl>            <dbl> <chr>             <chr>           
#> 1 2023-01-14           -35.3             149. Callocephalon fi… present         
#> 2 2023-01-15           -35.3             149. Eolophus roseica… present
```

Not sure where to start? Use `suggest_workflow()` to know what steps you
need to make to make your data Darwin Core compliant.

``` r
df |> 
  suggest_workflow()
#> 
#> ── Matching Darwin Core terms ──────────────────────────────────────────────────
#> Matched 1 of 5 column names to DwC terms:
#> ✔ Matched: eventDate
#> ✖ Unmatched: latitude, longitude, species, status
#> 
#> ── Minimum required Darwin Core terms ──────────────────────────────────────────
#> 
#>   Type                      Matched term(s)  Missing term(s)                                                                
#> ✔ Date/Time                 eventDate        -                                                                               
#> ✖ Identifier (at least one) -                occurrenceID, catalogNumber, recordNumber                                       
#> ✖ Record type               -                basisOfRecord                                                                   
#> ✖ Scientific name           -                scientificName                                                                  
#> ✖ Location                  -                decimalLatitude, decimalLongitude, geodeticDatum, coordinateUncertaintyInMeters
#> 
#> ── Suggested workflow ──────────────────────────────────────────────────────────
#> 
#> To make your data Darwin Core compliant, use the following workflow:
#> df |>
#>   set_occurrences() |>
#>   set_coordinates() |>
#>   set_scientific_name()
#> 
#> ── Additional functions
#> ℹ See all `set_` functions at
#>   http://corella.ala.org.au/reference/index.html#add-rename-or-edit-columns-to-match-darwin-core-terms
```

Or, if your data is nearly ready and you want to run checks over all
columns that match Darwin Core terms, run `check_dataset()`.

``` r
df |>
  check_dataset()
#> ℹ Testing data
#> ✔ | E P | Column
#> ⠙ | 0 eventDate
#> ⠹ | 1 ✖ | eventDate ✔ | 1 ✖ | eventDate  [111ms]
#> ══ Results ═════════════════════════════════════════════════════════════════════
#> 
#> [ Errors: 1 | Pass: 0 ]
#> ℹ Checking Darwin Core compliance
#> ✖ Data does not meet minimum Darwin Core column requirements
#> ℹ Use `suggest_workflow()` to see more information.
#> ── Error in term ───────────────────────────────────────────────────────────────
#> 
#> eventDate must be a Date vector, not a character.
#> ℹ Specify date format with lubridate functions e.g. `ymd()`, `mdy()`, or
#> `dmy()`.
```

## Citing corella

To generate a citation for the package version you are using, you can
run:

``` r
citation(package = "corella")
```

The current recommended citation is:

> Kellie D, Balasubramaniam S & Westgate MJ (2025) corella: Tools to
> standardize biodiversity data to Darwin Core. R Package version 0.1.4.
