# pkgg

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/onwhenrdy/pkgg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/onwhenrdy/pkgg/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/github/onwhenrdy/pkgg/graph/badge.svg?token=qrLO7L1ldW)](https://codecov.io/github/onwhenrdy/pkgg)

<!-- badges: end -->

GG for handling your R packages

## Example

```r
library(pkgg)

manager <- pkg_manager(
  pkgs("PKNCA", "shiny", "shinyWidgets", "waiter", "bs4Dash", "DT"),
  pkgs_local(
    "Open-Systems-Pharmacology/OSPSuite.RUtils",
    "Open-Systems-Pharmacology/TLF-Library",
    "Open-Systems-Pharmacology/rSharp",
    .load = FALSE
  ),
  pkg_local("ospsuite=Open-Systems-Pharmacology/OSPSuite-R")
)
```
