# The Kiwi Urban Liveability Index (KULI) for Auckland 

## Analysing intra-urban patterns of urban liveability in Auckland with a spatial composite indicator

This repository contains the scripts used for the creation of the KULI in Auckland, and its analysis as part of my disseratation project at the University of Leeds.

### Main files descriptions

- [network_generation.ipynb](https://github.com/jankomag/uli-nz/blob/master/network_generation.ipynb) : python notebook to generate walking, biking and driving networks for the study area using the OSMnx package
- [network-analysis.R](https://github.com/jankomag/uli-nz/blob/master/network-analysis.R): R script for calculating walking or driving distances to nearest of the selected points of interest
- [data_prep.R](https://github.com/jankomag/uli-nz/blob/master/data_prep.R): gathering data for the index construction; dealing with missing values and cleaning the data
- [index.R](https://github.com/jankomag/uli-nz/blob/master/index.R): construction of the KULI, with data transformations, and saving the final index
- [regression_analysis.R](https://github.com/jankomag/uli-nz/blob/master/regression_analysis.R): contains various methods of evaluating the index, including spatial autocorrelation, regression analysis, GWR, and plotting and mapping the results

Jan Magnuszewski, University of Leeds