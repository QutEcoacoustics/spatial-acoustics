library(lidR)
library(raster)
library(rgdal)

fzone <- readLAS("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/EVB304/BatLiDAR/Fzone.las")
rzone <- readLAS("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/EVB304/BatLiDAR/Rzone.las")
dem <- raster("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/EVB304/GIS/DEM_5m_3857/1DEM_3857.tif")

plot(dem)
dem <- projectRaster(dem, crs = "+proj=utm +zone=56 +south +datum=WGS84 +units=m +no_defs")

print(fzone)
print(dem)

#Plotting the points 
plot(fzone)
plot(fzone@data$Z ~ fzone@data$X)

#normalize_height
norm_z <- normalize_height(rzone, algorithm = dem, na.rm = T)

#Check differences before and after normalising
plot(norm_z)
plot(norm_z@data$Z ~ norm_z@data$X)

#Calculates normalized Shannon vertical complexity index 
entropy <- entropy(norm_z@data$Z)

#Digital surface model - Choosing resolution: the advantage of the LiDAR is the accuracy and resolution. I suggest to run with different ones so you can see the difference and maybe plot it with the points so you can see better what is the best. to choose the algorithm I suggest to run with all of them and have a look which one looks best. They are all interpolation techniques and that's how usually we choose between them - unless you have a good reason for a preference. 
grid_canopy <- grid_canopy(las = norm_z, res = 0.5, algorithm = p2r())
plot(grid_canopy)

#Find the max point but it d0oesn't give any location
max <- max(norm_z@data$Z) 

#Finding trees - Gives trees locations and the coordinates: see if its interesting and maybe ground truth a sample?
trees <- find_trees(norm_z, algorithm = lmf(10, shape = "square")) 
plot(trees)

#Mapping point density - what does this means? More points = more hits...
point_density <- grid_density(norm_z, 5)
plot(point_density)

#Leaf area density - dz = stratification: will measure leaf density every x metres
leaf_density <- LAD(norm_z@data$Z, dz = 1)
plot(leaf_density)
