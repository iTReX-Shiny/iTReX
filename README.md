# iTReX: interactive Therapy Response eXploration

## Overview

_iTReX_ is an R/Shiny web application for interactive analysis, visualization and exploration of mono- and combination therapy dose response profiling data and drug target interaction network mapping.
iTReX features an extended version of the drug sensitivity score (DSS_asym), integrating an advanced five-parameter logistic dose-response curve model and a differential combination drug sensitivity score (dcDSS) for combination therapy profiling.

_iTReX_ provides a complete workflow for analyzing therapy responses through five main functional modules:

i) the Quality Control and Normalization module (QCN-mod)

ii) the Mono-therapy Response Analysis module (MRA-mod)

iii) the Combination therapy Response Analysis module (CRA-mod)

iv) the drug Hits interaction Network mapping module (HitNet-mod)

v) a module for identifying potential sample-specific omics-based biomarkers from the drug target connectivity networks (Omics-mod)

## Prerequisites

1. An `R` interpreter ([www.r-project.org](https://www.r-project.org/)) -- we recommend version 4.0.0 or later.

1. The `pandoc` document converter ([pandoc.org](https://pandoc.org/)), version 1.12.3 or later.
   <!-- https://github.com/rstudio/rmarkdown/blob/5273047/R/render.R#L317-L320 -->

1. Several more standard system requirements such as `git` and `make`.

1. To run `shinytest`s, the `PhantomJS` headless browser ([phantomjs.org](https://phantomjs.org/)), version 2.1.1.

### On Windows and Mac

- `R` and `pandoc` are bundled in the free *RStudio Desktop* ([www.rstudio.com](https://www.rstudio.com/products/rstudio/#rstudio-desktop)).
  
  (You may need to add the `RStudio/bin/pandoc` folder to your `PATH` manually.)

### On Linux

- On Ubuntu 20.04, you may use

   ```bash
   sudo apt-get install -y git make pandoc perl libcairo2-dev \
       libcurl4-openssl-dev libgit2-dev libglpk-dev libgmp3-dev libicu-dev \
       libpng-dev libssh2-1-dev libssl-dev libxml2-dev zlib1g-dev
   ```

   to install `pandoc` and all system requirements. (If unsure, skip this step first.)

- To run tests, the following may be required:

   ```bash
   sudo apt-get install -y git gsfonts imagemagick make pandoc perl \
       libcairo2-dev libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev \
       libgit2-dev libglpk-dev libgmp3-dev libicu-dev libmagick++-dev \
       libpng-dev libssh2-1-dev libssl-dev libxml2-dev zlib1g-dev
   ```

## Installation

_iTReX_ can be installed from the `R` command prompt:

1. Install the `devtools` package from [CRAN](https://cran.r-project.org/package=devtools):

   ```r
   install.packages("devtools")
   ```

1. Install _iTReX_ from [GitHub](https://github.com/D-Harouni/iTReX):

   ```r
   devtools::install_github("D-Harouni/iTReX")
   ```

   After some up to 30 minutes, you will be greeted by

   ```text
   * DONE (iTReX)
   ```

1. Start _iTReX_:

   ```r
   iTReX::run()
   ```

## Running tests (optional)

1. Install _iTReX_ with optional dependencies:

   ```r
   devtools::install_github("D-Harouni/iTReX", dependencies = TRUE, force = TRUE)
   ```

1. Install `PhantomJS`:

   ```r
   shinytest::installDependencies()
   ```

1. Start _iTReX_ tests:

   ```r
   iTReX::test()
   ```

   After a few minutes, you might see the following ideal output:

   ```text
   Loading required package: shiny
   Running all_tabs.R combo_sample_qc.R  [...]
   ==== Comparing all_tabs... No changes.
   ==== Comparing combo_sample_qc... No changes.
   [...]
   ```

   Slight rendering differences across platforms and package versions are to be expected.
