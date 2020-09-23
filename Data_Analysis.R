# Processing of Worldview 3 satellite data

# After the raw satellite data is preprocessed it can be analyzed with the following 
# methods. Spectral ratios are calculated to enhance specific mineralogic compositions.
# To include landform analysis the topographic position index (TPI) is calculated.
# A principal component analysis (pca) is used to reduce the dimensions.


# load libraries
library(raster)               # for working with spatial data
library(rgdal)                # Provides bindings to the 'Geospatial' Data Abstraction Library
library(spatialEco)           # for spatial manipulation of data (tpi calculation)


# Set working directory
setwd("E:/R/Sossusvlei_Worldview3")

# Load data 
Wv3_Preprocessed <- stack('WV3_Preprocessed.gri')                         # preprocessed multi-band satellite image

dem = raster('AP_05540_FBS_F6680_RT1.dem.tif')                            # ALOS PALSAR DEM
dem = resample(dem, Wv3_Preprocessed)                                     # clip DEM to dimension of satellite image

GroundTruthing <- readOGR(dsn=".", layer = 'GroundTruthing_Points')       #Load Shapefile with Ground Truthing Points

# Calculation of selected band ratios
Ratio_AlOH <- (Ratio_AlOH[[9]]+Wv3_Preprocessed[[11]])/Wv3_Preprocessed[[10]]    # Mineralogic ratio for AlOH
writeRaster(Ratio_AlOH, "AlOH_Ratio", format="GTiff")                            # export results

Ratio_Amphibole <- Wv3_Preprocessed[[10]]/Wv3_Preprocessed[[12]]                 # Mineralogic ratio for amphibole
writeRaster(Ratio_Amphibole, "Amphibole_Ratio", format="GTiff")                  # export results

Ratio_FerricOxides <- Wv3_Preprocessed[[7]]/Wv3_Preprocessed[[4]]                # Mineralogic ratio for ferric oxides
writeRaster(Ratio_FerricOxides, "FerricOxides_Ratio", format="GTiff")            # export results


#Tpi (Topographic Position index)
tpi9 <- tpi(dem, scale=15)                         # calculate the TPI
plot(tpi9, main="tpi 9x9")                         # visually asses the TPI and change the scale if necessary
writeRaster(tpi9, "tpi", format="GTiff")           # export results

# Apply a principle component analysis (pca) to reduce dimensions and noise
pca<- prcomp(Wv3_Preprocessed, center=T, scale.=T)
writeRaster(pca, "pca", format="GTiff")            # export results


