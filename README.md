
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RUBer <a href='https://richardmeyer-eppler.github.io/RUBer/'><img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/RUBer)](https://cran.r-project.org/package=RUBer)
<!-- badges: end -->

## Overview

RUBer is an R package created for parameterized reporting according to
the requirements at the Ruhr-Universität Bochum (RUB). So far, the
package was primarily used for the eigth reporting cycle for teaching
([*Lehrberichterstattung*](http://www.uv.ruhr-uni-bochum.de/dezernat1/aufgaben/abteilung1/qmp/instrumente/inst_lehrberichterstattung.html))
in 2018-19. The package provides a large example dataset containing fake
values, an R Markdown template in the RUB corporate design,
preconfigured `ggplot2` plotting functions, custom themes for `ggplot2`
and `flextable`, as well as functions making the RUB corporate design
colors available.

## Main features

  - An R Markdown template for use with `rmarkdown::draft`. The template
    uses `knitr::knit_expand()` to dynamically create code chunks
    containing figures, captions and subcaptions based on the data frame
    passed on as a parameter. Figure captions are automatically numbered
    via `bookdown` and a table of figures is inserted via `officedown`.
  - Vectorized figures in Microsoft’s Enhanced Metafile format using the
    `devEMF` package.
  - Five preconfigured `ggplot2` plotting functions.
  - A custom `ggplot2` theme.
  - A custom `flextable` theme.
  - Word reference document in the RUB corporate design for use with the
    R Markdown template.
  - Functions to access the RUB color palette.
  - Scale functions to make the RUB color palettes available in
    `ggplot2`.
  - A dataset with 249,492 rows, `df_fake`, that illustrates how we used
    this package in 2018 to create 64 reports with a total of 3,024
    unique figures. All values in the dataset are fake and
    algorithmically generated, all other columns, though, are identical
    to the confidential production dataset.
  - Data wrangling functions specific to the 2018 dataset. These
    function are probably of no interest externally, as access to the
    raw production dataset is confidential.

## Guiding design principles

1.  Output format for the paramterized reporting is Microsoft Word, so
    that the reports can be shared and edited as widely as possible.
2.  The generated reports adhere to the RUB corporate design and are
    ready for printing as is, with no further adjustments to layout or
    content required.
3.  As far as possible, the data manipulation is done independently of
    all the reporting steps. The idea is that the data passed to the R
    Markdown template can come from any source, not just R. While the
    data wrangling for the 2018 reports *was* done in R, this is not a
    requirement. If there is a need to create or alter datasets using
    Microsoft Excel, it is possible to use these datasets as basis for
    all reporting functions of `RUBer`.

## Project background

Every three years, the teaching and program quality of all degree
programs at the University of Bochum is assessed
([*Lehrberichterstattung*](http://www.uv.ruhr-uni-bochum.de/dezernat1/aufgaben/abteilung1/qmp/instrumente/inst_lehrberichterstattung.html)).
The university administration assists this process by providing data
reports that combine descriptive higher education statistics with survey
data. The University of Bochum regularly collects survey data in the
study entry phase (*Studieneingangsbefragung*), mid-study
(*Studienverlaufsbefragung*) and for its graduates
(*AbsolventInnenbefragung*, collected through the nationwide [Graduate
Survey Cooperation Project (KOAB)](https://istat.de/de/koab_a.html)).
Previously, for the data reports, these rich data sources were analyzed
and visualized by hand. `RUBer` automates this process, creating large
numbers of print-ready reports in Microsoft Word.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("RUBer")
```
