# Preprocessing of World View 3 satellite data

# Worldview-3 (WV-3) is a multispectral satellite system covering the Earth's 
# surface in spectral bands.A package of four visible to near-infrared
# (VNIR; blue, green, red, near-infrared (NIR 1)) and eight short wave infra-red
# (SWIR) bands is used in this study.Digital Globe ortho-rectified the scene to 
# the standard level 2A. A topographic correction can be applied to limit the 
# influence of topographically induced shadows on the reflectances.  

# Workflow
# Pre-processing follows the workflow outlined by Kuester (2016) (Figure 4.2) and
# starts with converting digital numbers (DN) to at-sensor radiance and then
# calculating the top of the atmosphere (TOA) reflectance. The VNIR bands 
# (bands 1-4) are resampled to match spatial resolution of the SWIR bands using
# a nearest neighbour algorithm. Effects of atmospheric scattering are removed 
# using a dark object subtraction (Chavez, 1988).

# load the required libraries
library(raster)              # for working with spatial data
library(rgdal)               # Provides bindings to the 'Geospatial' Data Abstraction Library
library(readxl)              # for easy import of excel files

# Set working directory
setwd("E:/R/Sossusvlei_Worldview3")

# Read satellite data
VNIR <- stack('15APR29092052-M2AS-055210484020_01_P001.TIF')    # WV3 VNIR bands   
SWIR <- stack('15APR29092053-A2AS-055210484010_01_P001.TIF')    # WV3 SWIR bands

dem = raster('AP_05540_FBS_F6680_RT1.dem.tif')                  # ALOS PALSAR DEM

# Read parameter file (contains information for TOA calculation)
WV3_TOA_Parameter <- read_excel('WV3_TOA_Parameter.xlsx')

# Rename the specific bands for clarification
names(VNIR)=c('blue', 'green','red','NIR')                        
names(SWIR)=c('SWIR1','SWIR2','SWIR3','SWIR4','SWIR5','SWIR6','SWIR7','SWIR8')

# Lower the resolution of the VNIR bands to match the resolution of the SWIR bands
VNIR.resample=resample(VNIR, SWIR, method='ngb')

# Combine VNIR and SWIR bands to one multi-band image
VNIR.SWIR.Stack = stack(VNIR.resample, SWIR)


# Conversion of DN to at-sensor radiance
blue.radiance = ((VNIR.SWIR.Stack[[1]]*WV3_TOA_Parameter[[2,2]])/WV3_TOA_Parameter[[2,3]])
green.radiance = ((VNIR.SWIR.Stack[[2]]*WV3_TOA_Parameter[[3,2]])/WV3_TOA_Parameter[[3,3]])
red.radiance = ((VNIR.SWIR.Stack[[3]]*WV3_TOA_Parameter[[4,2]])/WV3_TOA_Parameter[[4,3]])
nir.radiance = ((VNIR.SWIR.Stack[[4]]*WV3_TOA_Parameter[[5,2]])/WV3_TOA_Parameter[[5,3]])
swir1.radiance = ((VNIR.SWIR.Stack[[5]]*WV3_TOA_Parameter[[6,2]])/WV3_TOA_Parameter[[6,3]])
swir2.radiance = ((VNIR.SWIR.Stack[[6]]*WV3_TOA_Parameter[[7,2]])/WV3_TOA_Parameter[[7,3]])
swir3.radiance = ((VNIR.SWIR.Stack[[7]]*WV3_TOA_Parameter[[8,2]])/WV3_TOA_Parameter[[8,3]])
swir4.radiance = ((VNIR.SWIR.Stack[[8]]*WV3_TOA_Parameter[[9,2]])/WV3_TOA_Parameter[[9,3]])
swir5.radiance = ((VNIR.SWIR.Stack[[9]]*WV3_TOA_Parameter[[10,2]])/WV3_TOA_Parameter[[10,3]])
swir6.radiance = ((VNIR.SWIR.Stack[[10]]*WV3_TOA_Parameter[[11,2]])/WV3_TOA_Parameter[[11,3]])
swir7.radiance = ((VNIR.SWIR.Stack[[11]]*WV3_TOA_Parameter[[12,2]])/WV3_TOA_Parameter[[12,3]])
swir8.radiance = ((VNIR.SWIR.Stack[[12]]*WV3_TOA_Parameter[[13,2]])/WV3_TOA_Parameter[[13,3]])

# calculation of the top of the atmosphere (TOA) reflectance
blue.reflectance = ((blue.radiance * pi * (WV3_TOA_Parameter[[2,6]])^2)/WV3_TOA_Parameter[[2,7]] * (cos(WV3_TOA_Parameter[[2,8]] / pi * 180)))
green.reflectance = ((green.radiance * pi * (WV3_TOA_Parameter[[3,6]])^2)/WV3_TOA_Parameter[[3,7]] * (cos(WV3_TOA_Parameter[[3,8]] / pi * 180)))
red.reflectance = ((red.radiance * pi * (WV3_TOA_Parameter[[4,6]])^2)/WV3_TOA_Parameter[[4,7]] * (cos(WV3_TOA_Parameter[[4,8]] / pi * 180)))
nir.reflectance = ((nir.radiance * pi * (WV3_TOA_Parameter[[5,6]])^2)/WV3_TOA_Parameter[[5,7]] * (cos(WV3_TOA_Parameter[[5,8]] / pi * 180)))
swir1.reflectance = ((swir1.radiance * pi * (WV3_TOA_Parameter[[6,6]])^2)/WV3_TOA_Parameter[[6,7]] * (cos(WV3_TOA_Parameter[[6,8]] / pi * 180)))
swir2.reflectance = ((swir2.radiance * pi * (WV3_TOA_Parameter[[7,6]])^2)/WV3_TOA_Parameter[[7,7]] * (cos(WV3_TOA_Parameter[[7,8]] / pi * 180)))
swir3.reflectance = ((swir3.radiance * pi * (WV3_TOA_Parameter[[8,6]])^2)/WV3_TOA_Parameter[[8,7]] * (cos(WV3_TOA_Parameter[[8,8]] / pi * 180)))
swir4.reflectance = ((swir4.radiance * pi * (WV3_TOA_Parameter[[9,6]])^2)/WV3_TOA_Parameter[[9,7]] * (cos(WV3_TOA_Parameter[[9,8]] / pi * 180)))
swir5.reflectance = ((swir5.radiance * pi * (WV3_TOA_Parameter[[10,6]])^2)/WV3_TOA_Parameter[[10,7]] * (cos(WV3_TOA_Parameter[[10,8]] / pi * 180)))
swir6.reflectance = ((swir6.radiance * pi * (WV3_TOA_Parameter[[11,6]])^2)/WV3_TOA_Parameter[[11,7]] * (cos(WV3_TOA_Parameter[[11,8]] / pi * 180)))
swir7.reflectance = ((swir7.radiance * pi * (WV3_TOA_Parameter[[12,6]])^2)/WV3_TOA_Parameter[[12,7]] * (cos(WV3_TOA_Parameter[[12,8]] / pi * 180)))
swir8.reflectance = ((swir8.radiance * pi * (WV3_TOA_Parameter[[13,6]])^2)/WV3_TOA_Parameter[[13,7]] * (cos(WV3_TOA_Parameter[[13,8]] / pi * 180)))

# Combine all reflectance bands to multi-band image
VNIR.SWIR.Reflectance = stack(blue.reflectance, green.reflectance, red.reflectance, nir.reflectance, swir1.reflectance, swir2.reflectance, swir3.reflectance, swir4.reflectance, swir5.reflectance, swir6.reflectance, swir7.reflectance, swir8.reflectance)

# Dark pixel subtraction as an easy substitute for the removal of atmospheric scattering
mnV = minValue(VNIR.SWIR.Reflectance)                          # calculation of the minimum reflectance value
VNIR.SWIR.Reflectance.dps = VNIR.SWIR.Reflectance - mnV        # subtraction of the minimum value 


# Correction of topographic effects
# resample and clip the DEM to the multi-band image
dem = resample(dem, VNIR.SWIR.Reflectance.dps)         

# Use the terrain() function to calculate slope and aspect
slp = terrain(dem, opt = 'slope')
asp = terrain(dem, opt = 'aspect')

# Create hillshade with Solar Azimuth and Sun Elevation angle
hs.wv <- hillShade(slp, asp, angle=44.6, direction=33.3) 

# Reclassification of hillshade and set values to 0.1
hs.wv <- reclassify(hs.wv, matrix(c(-Inf,0.1,0.1),1,3)) 

# Topographic normalization
VNIR.SWIR.topo <- VNIR.SWIR.Reflectance.dps*cos((90-44.6)/180*pi)/hs.wv 


# Create NDVI Mask to conceal vegetation
# Calculate NDVI
ndvi <- (VNIR.SWIR.topo[[4]]-VNIR.SWIR.topo[[3]])/(VNIR.SWIR.topo[[4]]+VNIR.SWIR.topo[[3]]) 

# Create a threshold for NDVI to effectively mask all vegetation
ndvi.threshold <- ndvi >0.25 

# Visually assess threshold and change it if it masks too much / too little
plot(ndvi.threshold)

#Create a binary mask for NDVI
ndvi.mask <- reclassify(ndvi.threshold, matrix(c(0,1,1,NA),2,2,byrow=T)) 

#Apply mask on multi-band image
VNIR.SWIR.mask <- VNIR.SWIR.topo*ndvi.mask 

# Visually review the preprocessed satellite image
plotRGB(VNIR.SWIR.mask, 3,2,1, stretch='lin')

# Export the multi-band satellite image as a raster with the 'EHdr' format to use it for further processing
writeRaster(VNIR.SWIR.Reflectance.dps, "WV3_Preprocessed")
