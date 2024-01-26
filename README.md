
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ordinalsim

<!-- badges: start -->

[<img alt="alt_text" src="https://img.shields.io/badge/OSF-https://osf.io/93h5j/-337AB7"/>](https://osf.io/93h5j/)
<!-- badges: end -->

This repository contains the code and functions to reproduce the
analysis and simulations presented in the paper *Ordinal regression
models made easy. A tutorial on parameter interpretation, data
simulation, and power analysis.* by Filippo Gambarota and Gianmarco
Altoè. This repository is organized as an R package:

- `R/`: folder with all custom functions used in the paper and the
  supplementary materials
  - `utils.R`: main functions for simulations
  - `utils-plotting.R`: functions for plots
  - `utils-project.R`: functions for the project and folders
    organization, mainly for internal usage
  - `utils-paper.R`: functions for the paper formatting, mainly for
    internal usage
- `paper/`: folder with the source of the paper (`.Rmd`) and the
  compiled pdf document
- `supplementary/`: folder with some extra documents (`.qmd`) with
  examples
- `tests/`: folder with unit tests for the main functions (`R/utils.R`)

## Using the functions

Once cloned or downloaded the repository, the functions can be used
running `devtools::load_all()` (the `devtools` package is required).
This is very similar to `library(package)` thus all functions are
available but not as objects in the global environment (compared to
using `source(file)`).

## Packages

These packages are used through the project:

- `rmarkdown` (2.25)
- `papaja` (0.1.1)
- `knitr` (1.45)
- `magick` (2.7.5)
- `xfun` (0.40)
- `cowplot` (1.1.1)
- `devtools` (2.4.5)
- `dplyr` (1.1.2)
- `ggeffects` (1.2.3)
- `ggplot2` (3.4.4)
- `here` (1.0.1)
- `kableExtra` (1.4.0)
- `ordinal` (2022.11.16)
- `purrr` (1.0.2)
- `tidyr` (1.3.0)
- `viridis` (0.6.4)
- `filor` (0.1.0)
- `distributional` (0.3.2)
- `ggdist` (3.3.0)
- `latex2exp` (0.9.6)
- `stringr` (1.5.0)
- `broom` (1.0.5)
- `flextable` (0.9.5.5)
- `fs` (1.6.3)
- `ftExtra` (0.6.1)
- `gt` (0.10.0)
- `RColorBrewer` (1.1.3)
- `scales` (1.2.1)
- `tools` (4.3.1)
- `ggtext` (0.1.2)
- `qpdf` (1.3.2)
- `renv` (1.0.1)
- `cli` (3.6.2)
- `sessioninfo` (1.2.2)
- `tidyverse` (2.0.0)
- `effects` (4.2.2)
- `MASS` (7.3.60)
- `car` (3.1.2)
- `bookdown` (0.35)
- `ordDisp` (2.1.1)
- `ordinalsim` (1.0.0)
- `testthat` (3.1.10)

## Session info

    #> version: R version 4.3.1 (2023-06-16 ucrt)
    #> os: Windows 10 x64 (build 19045)
    #> system: x86_64, mingw32
    #> ui: RTerm
    #> language: (EN)
    #> collate: English_United States.utf8
    #> ctype: English_United States.utf8
    #> tz: Europe/Rome
    #> pandoc: 3.1.8 @ C:/Users/user/AppData/Local/Pandoc/ (via rmarkdown)
