## Overview

This repository contains all code and input data corresponding to the analysis and results in our paper with the title: "Illustrating Emerging Good Practices for Quantitative Benefit-Risk Assessment: A Hypothetical Case Study of Systemic Biologic Treatments for Plaque Psoriasis" published in Value in Health in April 2023.

The "readme" file and codes are written by Leila Lackey (leila.lackey@fda.hhs.gov).

## R Code

ispor qbra accepted.R contains all R code. Required packages are: ggplot2, plyr, smaa, mvtnorm, dplyr, reshape2, tidyr

## CSV Files

et - induction.csv contains the induction phase model effects table
et - induction - ci.csv contains confidence intervals for effect estimates in the induction phase model
et - induction - n.csv contains sample sizes for effect estimates in the induction phase model
et - mainenance.csv contains the effects table for the maintenance phase model
et - nail.csv contains the effects table for the nail psoriasis model
ppi - induction.csv contains the induction phase model preference data
ppi - induction - ci.csv contains confidence intervals for effect estimates in the induction phase model
ppi - maintenance.csv contains preference data for the maintenance phase model
ppi - nail.csv contains preference data for the nail psoriasis model

## Output

Code produces all plots and results contained in our paper. 17 figures will be automatically saved in the working directory.