---
title: 'OCBCR'
author: 'Rachel Lui Win Tan'
output:  
  html_document:
    keep_md: true
---



The goal of OCBCR is to allow information from OCBC APIs to be accessed easily in an R-readable format.The Overseas-Chinese Banking Corporation Limited (OCBC) is a Singapore-based bank that has operations spanning 17 countries. OCBC bank provides developers with more than 250 APIs to work with. The APIs fall under the category of Informational and Transactional, however due to complications with the OAuth 2.0 for Transactional APIs, this package only focuses on making the Informational APIs accessible. I hope that this package can improve the accessibility of the APIs for developers looking to work with OCBC data. 

# Installation

You can install the development version from [GitHub](https://github.com/racheltlw) with:


```r
install.packages("devtools")
devtools::install_github("racheltlw/OCBCR", build_vignettes = TRUE)
```
# Vignettes and Documentation

You can find the full documentation of how to use the package [here]( https://racheltlw.github.io/OCBCR/)

# Contact 
For any bugs or issues please raise and issue on Github

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
