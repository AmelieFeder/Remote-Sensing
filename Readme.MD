# Remote Sensing Workflow

The files are to demonstrate how I mapped sedimentary facies.

In order to simplify the code, I split it in three parts: Preprocessing, Data Analysis and Classification. The analysis was based on WorldView 3 satellite data by Digital Globe and an ALOS PALSAR DEM.

#### Preprocessing
The Preprocessing prepares the satellite data for work in R-Studio and calculates the top of the atmosphere reflectances. Furthermore, it removes the effects of atmospheric scattering, applies a topographic normalization and creates a mask to hide vegetation. Based on the available satellite data and its quality the steps have to be changed accordingly.

#### Data Analysis
The Data Analyses were done in order to highlight sedimentological (mineralogical and textural) features. A different issue can require different analysis. The mineralogical ratios enhanced clay and iron oxide minerals. The TPI and slope included the geomorphology of the different depositional environments.

#### Classification
The Classification shows the appliance of the supervised classification algorithm ‘random forest’ by *Breiman (2001)*. All of the analysis files and ground truthing data are fed to the algorithm and a map based on sedimentary facies is created.

Further information about the remote sensing process can be found in the publication *Feder et. al (2018)*. The resulting facies map can be viewed in FaciesMap.pdf.
