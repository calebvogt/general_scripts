### h5_viewer.R 
# Working with HDF5 files
# http://bioconductor.org/packages/release/bioc/html/rhdf5.html

wd <- setwd("C:/Users/Caleb Vogt/Desktop/simba_test")
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("rhdf5")
library(rhdf5)
h5ls("C57-1008_Preference_D1_Estrus_030820_H2O-C57_MB2_CAM6_12_10_58_465_Boris_bandicut.mp4.predictions.slp")

# PULL OUT GROUPS FROM H5 FILE
frames <- h5read("C57-1008_Preference_D1_Estrus_030820_H2O-C57_MB2_CAM6_12_10_58_465_Boris_bandicut.mp4.predictions.slp", "frames")

instances <- h5read("C57-1008_Preference_D1_Estrus_030820_H2O-C57_MB2_CAM6_12_10_58_465_Boris_bandicut.mp4.predictions.slp", "instances")

meta <- h5read("C57-1008_Preference_D1_Estrus_030820_H2O-C57_MB2_CAM6_12_10_58_465_Boris_bandicut.mp4.predictions.slp", "metadata/H5I_GROUP")


library(hdf5r)
?h5read()
