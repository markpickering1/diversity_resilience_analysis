# resilience_data_collection
These scripts give an overview of the data collection of the initial input data used in the analysis.

The datasets can be found below, alongside the directories containing the scripts relating to them.

MODIS Terra & Aqua Surface Reflectance:
https://www.earthdata.nasa.gov/data/catalog/lpcloud-myd09ga-061
https://www.earthdata.nasa.gov/data/catalog/lpcloud-mod09ga-061
vegetation/

Forest structural diversity (FSD):
Public versions of the forest structural diversity metrics are available here:
https://figshare.com/s/daa9b652c12beb42e518
From the paper: https://essd.copernicus.org/preprints/essd-2024-471/
It should be noted this public version of the dataset uses a predictive machine learning model to predict FSD across all Europe including where there are no GEDI points.
The version used in the publication 'Enhanced structural diversity increases European forest resilience and potentially compensates for climate-driven declines' does not use the predictive model to fill in the gaps in GEDI coverage, instead using the average FSD values from the GEDI sampling.

Land cover dynamics:
https://doi.org/10.5067/MODIS/MCD12Q2.061
/ancillary/phenology/

Forest cover:
https://doi.org/10.1126/science.1244693
static_variables/hansen/

The ERA5 Land climate data:
https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=overview
climate/

The digital elevation model data:
https://doi.org/10.1029/2005RG000183
static_variables/srtm/

The soil organic carbon content data:
https://doi.org/10.5281/zenodo.1475457
static_variables/socc/

The Nitrogen deposition data:
https://www.emep.int/mscw/mscw_moddata.html
static_variables/Ndep/

European biogeographical regions:
https://sdi.eea.europa.eu/catalogue/srv/eng/catalog.search#/metadata/c6d27566-e699-4d58-a132-bbe3fe01491b

main/
Contains designed to be tailored to setup the working environment. The input data should be placed in directories and linked in the format found in each individual script.