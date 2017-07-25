
<!-- README.md is generated from README.Rmd. Please edit that file -->
datasus
=======

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/datasus)](https://CRAN.R-project.org/package=datasus) [![CRAC\_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/datasus)](https://CRAN.R-project.org/package=datasus)

The "datasus" R package seeks to provide direct access to the data of TABNET/DATASUS within the R environment much in the same way that is done in the online portal. For now the package allows access to the systematic record of mortality and survival data (Vital Statistics - Mortality and Live Births) through SIM and SINASC's systems

Installation
------------

Install the release version from CRAN:

``` r
install.packages("datasus")
```

or the development version from github

``` r
# install.packages("devtools")
devtools::install_github("rpradosiqueira/datasus")
```

Example
-------

In this example we will download the last mortality data for the IBGE's micro-region where the micro-regions are in the rows and the ICD-10 chapters in the columns:

``` r
sim_obt10_mun(linha = "Microrregião IBGE",
              coluna = "Capítulo CID-10")
```

To more examples, see the vignette ["Introduction to datasus"](https://CRAN.R-project.org/package=datasus/vignettes/Introduction_to_datasus.html).
