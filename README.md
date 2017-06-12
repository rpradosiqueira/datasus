
<!-- README.md is generated from README.Rmd. Please edit that file -->
datasus
=======

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/datasus)](http://cran.r-project.org/package=datasus) [![CRAC\_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/datasus)](http://cran.rstudio.com/web/packages/datasus/index.html)

The "datasus" R package seeks to provide direct access to the data of TABNET/DATASUS within the R environment much in the same way that is done in the online portal. For now the package allows access to the systematic record of mortality and survival data (Vital Statistics - Mortality and Live Births) through SIM and SINASC's systems

Installation
------------

To install the development version hosted on Github:

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
