# The Auckland Urban Liveability Index (AULI) 🏙 🏡 🚃 🇳🇿

## Analysing intra-urban patterns of urban liveability in Auckland with a spatial composite indicator

This repository contains the scripts used for the creation of the AULI, and its analysis as part of my disseratation project at the University of Leeds.

Interactive web map of the KULI and its components, created with Mapbox GL JS, can be accessed [here](https://jan.magnuszewski.com/auli)

### Main files descriptions

-   [network_generation.ipynb](https://github.com/jankomag/uli-nz/blob/master/network_generation.ipynb) : python notebook to generate walking, biking and driving networks for the study area using the OSMnx package; getting bus freqency data from GTFS for Auckland transport

-   [network-analysis.R](https://github.com/jankomag/uli-nz/blob/master/network-analysis.R): R script for calculating walking or driving distances to nearest of each of the amenities

-   [data_prep.R](https://github.com/jankomag/uli-nz/blob/master/data_prep.R): gathering data for the index construction; dealing with missing values and cleaning the data

-   [index.R](https://github.com/jankomag/uli-nz/blob/master/index.R): construction of the KULI, with data transformations, and saving the final index

-   [regression_analysis.R](https://github.com/jankomag/uli-nz/blob/master/regression_analysis.R): contains various methods of evaluating the index, including spatial autocorrelation, regression analysis, GWR, and plotting and mapping the results

-   The folder [web-map](https://github.com/jankomag/uli-nz/tree/master/web-map) contains the script for the web map

### Dataset
The AULI dataset will be available for download from...


Jan Magnuszewski, University of Leeds
