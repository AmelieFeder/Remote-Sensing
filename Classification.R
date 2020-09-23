# Supervised Classification with Random Forest algorithm

# The calibration and validation data were obtained by ground truthing in the field. 
# The input files are different analysis and calculations of the multispectral 
# satellite image and the DEM. Based on the satellite image or the research 
# topic, different analysis can be used.

# Load Libraries
library(raster)               # for working with spatial data
library(rgdal)                # Provides bindings to the 'Geospatial' Data Abstraction Library
library(randomForest)         # classification based on random forest machine learning algorithm

# Set working directory
setwd("E:/R/Sossusvlei_Worldview3")

#Load input files 
AlOH.abundance <-raster('AlOH_Ratio.tif')             # Mineralogic ratio for AlOH
Ferric.Oxides <-raster('FerricOxides_Ratio.tif')      # Mineralogic ratio for ferric oxides
FerrousIron <-raster('FerrousIron_Ratio.tif')         # Mineralogic ratio for ferrous iron
b12Homo <-raster('textures_b12.tif')                  # Homogeneity calculation of Gray Level Occurrence Matrix
tpi500 <-raster('TPI500.tif')                         # Topographic Position Index with moving window of 500
slp <-raster('Slope.tif')                             # Slope calculation
pca<-stack('pca.tif')                                 # Principal Component Analysis

pcaSubset<- subset(pca.img, 1:2)                      # Subsetting the pca, to only use PC1 and PC2

#Build Classification Inputification layer
ClassificationInputificationInput <- stack(AlOH.abundance, Ferric.Oxides, tpi500, slp, b12Homo, FerrousIron, pcaSubset)

# Load Calibration and Validation Shape File
calibrationData<-shapefile("Shape_calN.shp")           # calibration file
validationData<-shapefile("Shape_valN.shp")            # validation file

# Random Forest Supervised Classification
cal.ref <- extract(ClassificationInput, calibrationData, df=T)                                 # extract table with pixel values
cal.ClassificationInput <- as.factor(t(calibrationData@data[,1])[cal.ref2[,1]])                # conversion of classes as factor
cal.ref<-cal.ref2[,-1]                                                                         # omit class codes
val.ClassificationInput <- t(validationData@data[,1])                                 
randomForestParameter<-randomForest(cal.ref, cal.ClassificationInput, ntree=4500)              # Random Forest with 4500 trees
randomForestParameter
classifiedMap <- predict(ClassificationInput, randomForestParameter)                           # Algorithm applied to input data

# Accuracy assessment
confusionMatrix<-table(extract(classifiedMap, validationData), val.ClassificationInput)        # create confusion matrix    
row.names(confusionMatrix)<-unique(cal.ClassificationInput)
confusionMatrix
overAllAccuracy <-round(sum(diag(confusionMatrix))/sum(confusionMatrix),3);overAllAccuracy     # calculate overall accuracy
usersAccuracy<-round(diag(confusionMatrix)/apply(confusionMatrix,1,sum),2);users.acc           # calculate users accuracy
producersAccuracy <-round(diag(confusionMatrix)/apply(confusionMatrix,2,sum),2);producers.acc  # calculate producer accuracy
combinedAccuracyList <- list (overAllAccuracy, usersAccuracy, producersAccuracy)               # create a combined list
names(combinedAccuracyList) <- c("OverAllAccuracy", "UsersAccuracy", "ProducerAccuracy")       # label list

# Export result
writeRaster(classifiedMap, "ClassificationResultMap", format="GTiff")
