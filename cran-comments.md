## Test environments

* local Windows 11 x64, R 4.6.0
* GitHub Actions: Windows, macOS and Ubuntu, R release
* GitHub Actions: Ubuntu, R-devel

## R CMD check results

0 errors | 0 warnings | 0 note

This release rebuilds the network layer, makes vignettes independent of live
services, fixes response encoding and endpoint routing, and adds automated
tests. It also introduces a shared query API and dataset catalog for SIH/SUS,
SIA/SUS, CNES, resident population, hospital morbidity, SINAN, PNI, SISCAN,
SISVAN and historical SUS financing tables.
This release also adds a generic OpenDataSUS client, disk caching,
provenance metadata, and current microdata access for SIVEP-Gripe, dengue and
Mpox.
It now also supports record-level SIM, SINASC and SIH DBC files through the
current native `datasusr` reader, with typed field standardization and stable
requested-column ordering.
The analytical layer now adds validated crude rates, exact Poisson confidence
intervals, Brazilian epidemiological weeks, moving averages and direct age
standardization.
SIM and SINASC now use the same declarative catalog and query engine as the
other TABNET systems. Historical entry points remain available as deprecated
compatibility wrappers.
The package now also bundles the current IBGE municipality hierarchy and
provides validated conversion, joining and completion helpers for geographic
codes and time series.
The integrated indicator layer now aggregates grouped rates, proportions,
ratios and case fatality, safely joins population denominators, and supplies
WHO, Segi and Scandinavian standard-population weights.
Current OpenDataSUS coverage now also includes anonymous ESAVI notifications,
state-level e-SUS Notifica influenza-like illness files, individual monthly
PNI doses and annual COVID-19 hospital occupancy records.
These contemporary sources now have curated schemas, schema-drift reporting,
column-selective parsing, transparent multipart resources and bounded-memory
chunk processing.
The contemporary schemas now cover all fields documented for ESAVI, e-SUS
Notifica, PNI doses and the current hospital-occupancy layout. Four new
task-oriented vignettes cover access, surveillance, analysis and large-file
workflows. Large downloads use a separate configurable timeout and empty
source archives are diagnosed explicitly.

## Reverse dependencies

There are no known reverse dependencies.

