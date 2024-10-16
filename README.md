
<!-- README.md is generated from README.Rmd. Please edit that file -->

# corella <img src="man/figures/corella-logo.png" align="right" style="margin: 0px 10px 0px 10px;" alt="" width="120"/><br>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/corella)](https://CRAN.R-project.org/package=corella)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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
Australia](https://www.ala.org.au) (ALA). The package is named for an
endangered Australian species of [alpine
frog](https://bie.ala.org.au/species/https://biodiversity.org.au/afd/taxa/cbf9278d-64b7-43d6-9691-78b99b2eb65f#overview)
native to the marshlands and forests of ACT and NSW.

If you have any comments, questions or suggestions, please [contact
us](mailto:support@ala.org.au).

## Installation

You can install the development version of `corella` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AtlasOfLivingAustralia/corella")
```

## Example

## Citing corella

To generate a citation for the package version you are using, you can
run:

``` r
citation(package = "corella")
```

The current recommended citation is:

> Westgate MJ, Balasubramaniam S & Kellie D (2024) corella: Tools to
> standardize biodiversity data to Darwin Core. R Package version
> 0.1.0.9999.
