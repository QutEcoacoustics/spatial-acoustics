library(lidR)
library(raster)
library(rgdal)


#Reading the .las file
fzone <- readLAS("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/EVB304/BatLiDAR/Fzone.las")
rzone <- readLAS("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/EVB304/BatLiDAR/Rzone.las")

#Reading the DEM
dem <- raster("C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/EVB304/GIS/DEM_5m_3857/1DEM_3857.tif")

#Checking objects - note crs
print(fzone)
print(dem)

#Putting everything in the same crs
dem <- projectRaster(dem, crs = "+proj=utm +zone=56 +south +datum=WGS84 +units=m +no_defs")

#Plotting the points 
dem_b4norm <- plot(dem)
fzone_b4norm <- plot(fzone)
fzoneZ_b4norm <- plot(fzone@data$Z ~ fzone@data$X)

#normalize_height
norm_z <- normalize_height(fzone, algorithm = dem, na.rm = T)

#Check differences before and after normalising
fzone_afternorm <- plot(norm_z)
fzonez_afternorm <- plot(norm_z@data$Z ~ norm_z@data$X)

#Calculates normalized Shannon vertical complexity index 
entropy_data <- entropy(fzone@data$Z)
entropy_norm <- entropy(norm_z@data$Z)

#Digital surface model - Choosing resolution: the advantage of the LiDAR is the accuracy and resolution. I suggest to run with different ones so you can see the difference and maybe plot it with the points so you can see better what is the best. to choose the algorithm I suggest to run with all of them and have a look which one looks best. They are all interpolation techniques and that's how usually we choose between them - unless you have a good reason for a preference. 
grid_canopy <- grid_canopy(las = norm_z, res = 0.5, algorithm = p2r())
grid_canopy <- plot(grid_canopy)
writeRaster(grid_canopy, "C:/Users/scarp/OneDrive - Queensland University of Technology/Documents/PhD/EVB304/GIS/GridCanopy.tif")

#Find the max point but it d0oesn't give any location
max <- max(norm_z@data$Z)

#Finding trees - Gives trees locations and the coordinates: see if its interesting and maybe ground truth a sample?
trees <- find_trees(norm_z, algorithm = lmf(10, shape = "square")) 
plot(trees)

#Mapping point density - what does this means? More points = more hits...
point_density <- grid_density(fzone, 5)
plot(point_density)
summary(point_density)
library(raster)
density(point_density)

#Leaf area density - dz = stratification: will measure leaf density every x metres
leaf_density <- LAD(fzone@data$Z, dz = 6.71)
plot(leaf_density)
