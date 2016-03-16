bdm
===

Bayesian biomass dynamic model for estimation of population status from catch and abundance data

Instructions
=============
First, install the 'rstan' and 'devtools' packages from CRAN:

    # Install packages
    install.packages("devtools")
    install.packages("rstan")

Then install 'bdm' directly from GitHub:

    # devtools command to get bdm from GitHub
    devtools::install_github("cttedwards/bdm") 

The life history module 'lhm' is required to estimate the intrinsic growth rate:

    # devtools command to get lhm from GitHub
    devtools::install_github("cttedwards/lhm") 

