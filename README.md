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

Regardless of your operating system, you will need at least:

1. An `R` interpreter ([www.r-project.org](https://www.r-project.org/)) – we recommend version 4.1.2 or later.

1. The `pandoc` document converter ([pandoc.org](https://pandoc.org/)), version 1.12.3 or later.
   <!-- https://github.com/rstudio/rmarkdown/blob/69e6f983/R/render.R#L316-L320 -->

   `pandoc` is bundled in the free *RStudio Desktop* ([www.rstudio.com](https://www.rstudio.com/products/rstudio/#rstudio-desktop)).\
   (You may need to add the `RStudio/bin/pandoc` folder to your `PATH` manually.)

Optional dependencies include the following:

1. To compile packages from source, several developer tools.

   Note that source compilation can often be skipped at prompts such as this:

   > Do you want to install from sources the packages which need compilation? (Yes/no/cancel)

1. To run `shinytest`s, the `PhantomJS` headless browser ([phantomjs.org](https://phantomjs.org/)), version 2.1.1.

   `PhantomJS` can be installed from with `R`, see "Installation" section below.

### On Windows

- Developer tools are bundled in the free `Rtools4` ([https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)).

  Don't forget to add them to your `PATH` environment variable.

### On Linux

- On Ubuntu 20.04, you may use

   ```bash
   sudo apt-get install -y git make pandoc perl libcairo2-dev \
       libcurl4-openssl-dev libgit2-dev libglpk-dev libgmp3-dev libicu-dev \
       libpng-dev libssh2-1-dev libssl-dev libxml2-dev zlib1g-dev
   ```

   to install `pandoc` and all system requirements. (If unsure, skip this step first.)

- To run `shinytest`s, the following may be required:

   ```bash
   sudo apt-get install -y git gsfonts imagemagick make pandoc perl \
       libcairo2-dev libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev \
       libgit2-dev libglpk-dev libgmp3-dev libicu-dev libmagick++-dev \
       libpng-dev libssh2-1-dev libssl-dev libxml2-dev zlib1g-dev
   ```

## Installation

_iTReX_ can be installed from the `R` command prompt:

1. Install the `remotes` package from [CRAN](https://cran.r-project.org/package=remotes) \
   (if you have it installed already, simply install again or ensure it is v2.4.2 or later, compare [r-lib/remotes#666](https://github.com/r-lib/remotes/pull/666)):

   ```r
   install.packages("remotes")
   ```

1. Install _iTReX_ from [GitHub](https://github.com/iTReX-Shiny/iTReX):

   ```r
   remotes::install_github("iTReX-Shiny/iTReX")
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
   remotes::install_github("iTReX-Shiny/iTReX", dependencies = TRUE, force = TRUE)
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
