# diversity_resilience_analysis

This repository contains the code related toward reproducing the analysis and figures regarding the study entitled:
'Enhanced structural diversity increases European forest resilience and potentially compensates for climate-driven declines'
submitted for review to Nature Communications Earth & Environment.

## Overview
The study consists of an analysis of the relationship between Forest Structural Diversity (FSD) and resilience in selected European forests.
For full details of both the methodolgy and the results please see the published paper.
FSD metrics are derived from the GEDI instrument on board the ISS, and further information can be found: https://essd.copernicus.org/preprints/essd-2024-471/
Resilience metrics are developed from the variance and temporal autocorrelation of small perturbations in the (deseasonalised, detrended) time series (growing season only) of 500m forest MODIS kNDVI data, aggregated to a scale of 0.05°.
A random forest is used to isolate the relationship from confounding, spatially distinct environmental factors.
The emergent relationship is explored as an adaptation measure to preserve forest resilience under climate change.

Please find within separate repositories for:

## resilience_data_collection/
These scripts download and collect the publicly available data sources used in this analysis.
For users that have already downloaded the relevant input data of:
1) climate & vegetation timeseries, including, separately, a deseasonalised version (.nc format)
2) other relevant static datasets (.nc, .tif or .RDATA dataframe formats)
it is advised to skip to resilience_analysis.

## resilience_analysis/
This code is used for:
1) Data harmonisation and processing (ie building common dataframes)
2) Building the random forest model
3) Plotting and visualisation related to the performance of the RF model, cross-checking, and the interpretation of results

The code is streamlined for interpretability and producing a simplified version of the main figures only. It therefore does not necessarily include all cross checks and testing described in the methodology or the plotting of supplementary figures, or all of the sources of uncertainty in the uncertainty estimation, and so represents a scaled down version of the full analysis code.
As the Forest Structural Diversity (FSD) dataset used in the manuscript analysis (containing the mean GEDI values of various FSD metrics) differs slightly from the publicly available one (containing a spatially complete predictive model of FSD metrics), there may be differences between the final outputs.

We suggest starting with resilience_data_collection/ to download the data that will be used in the analysis.
We do provide an example dataframe: resilience_data_collection/data_processing/version_1/4_selections/df_all.RData
from which it is possible to immediately start building and plotting the RF model (beginning at resilience_analysis/2_analysis/ and resilience_analysis/3_create_figures/)
