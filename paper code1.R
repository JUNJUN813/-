install.packages("dplyr")
library(readxl)
library(sp)
library(gstat)
library(sf)
library(ggplot2)
library(dplyr)
library(raster) 

#01 Draw a map of Beijing
install.packages("ggplot")
library(ggplot2)
install.packages("sf")
library(sf)
beijing_data <- st_read("/Users/jj/Desktop/paper/2(1)/beijing.json")
beijing_data
ggplot() +
  geom_sf(data = beijing_data) +
  labs(title = "Beijing Map")
#02 Add monitoring point locations to the Beijing map
points_data <-beijing_points
points_data

points_sf <- st_as_sf(points_data, coords = c("V2", "V3"), crs = st_crs(beijing_data))
points_sf
ggplot() +
  geom_sf(data = beijing_data) +
  geom_point(data = points_sf, aes(x = st_coordinates(points_sf)[,1], y = st_coordinates(points_sf)[,2], color = "red")) +
  labs(title = "Beijing Map with Points")

#03 Add monitoring point locations to the Beijing map (monitoring points are displayed in different colors according to concentration changes)
#03-1Read Beijing map data
beijing_data <- st_read("/Users/jj/Desktop/paper/2(1)/beijing.json")
beijing_data
#03-2Draw a map of Beijing
bj <- ggplot() +
  geom_sf(data = beijing_data) +
  labs(title = "Beijing Map")
#03-3Draw a scatter plot of PM2.5 concentration and spatial location
pm25 <- ggplot(beijing_points, aes(x = V2, y = V3, color = V4)) +
  geom_point() +
  scale_color_gradient(low = "green", high = "red") +  
  labs(title = "PM2.5 concentration",
       x = "longitude",
       y = "latitude",
       color = "PM2.5 concentration") +
  theme_minimal()

library(sf)
beijing
pm25_data_sf <- st_as_sf(beijing_points, coords = c("V2", "V3"), crs = st_crs(beijing_data))
#Add a scatter plot to the map
combined_plot <- ggplot() +
  geom_sf(data = beijing_data) +
  geom_point(data = pm25_data_sf, aes(x = st_coordinates(pm25_data_sf)[,1], y = st_coordinates(pm25_data_sf)[,2], color = V4)) +
  labs(title = "PM2.5 concentration distribution at each monitoring point",
       x = "longitude",
       y = "latitude",
       color = "PM2.5 concentration") +
  theme_minimal()
combined_plot

pm_data <- read_excel("/Users/jj/Desktop/Bei.xlsx")beijing_map <- st_read("/Users/jj/Desktop/Beijing.json")
pm_data
sp_data
#  Build model
variogram_model <- variogram(pm25 ~ 1, sp_data)  # Calculate the variogram
variogram_model
#Spherical Model
fit_model3 <- fit.variogram(variogram_model, vgm("Sph", nugget = 2, range = 20, psill = 2000))
fit_model3
plot(variogram_model,fit_model3)

#Exponential Model
#fit_model4 <- fit.variogram(variogram_model, vgm("Exp", nugget = 5, range = 15, psill = 1000))
#fit_model4
#plot(variogram_model,fit_model4)
fit_model4 <- fit.variogram(variogram_model, vgm("Exp", nugget =1, range = 10, psill = 600))
fit_model4
plot(variogram_model,fit_model4)

#Gaussian Model
fit_model5 <- fit.variogram(variogram_model, vgm("Gau", nugget = 5, range = 15, psill = 1000))
fit_model5
plot(variogram_model,fit_model5)

# nested spherical model
fit_model_nested <- fit.variogram(variogram_model,
                                  vgm(
                                    psill=60,
                                    model="Sph", 
                                    range=10,
                                    nugget=20,
                                    add.to = vgm(psill=230,model="Sph", range=30)
                                  )
)

fit_model_nested
plot(variogram_model,fit_model_nested)

#########
library(readxl)
library(sp)
library(gstat)
library(dplyr)
library(sf)
library(ggplot2)

#Leave-One-Out Cross-Validation 留一法交叉验证
pm_data <- Bei
pm_data 
coords <- pm_data[, c("x", "y")]
pm25 <- pm_data$pm2.5
pm25 
sp_data <- SpatialPointsDataFrame(coords, data.frame(pm25 = pm25),
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))
sp_data
fit_model3 <- fit.variogram(variogram_model, vgm("Sph", nugget = 2, range = 20, psill = 2000))
fit_model3
plot(variogram_model,fit_model3)

fit_model4 <- fit.variogram(variogram_model, vgm("Exp", nugget =1, range = 10, psill = 600))
fit_model4
plot(variogram_model,fit_model4)

fit_model5 <- fit.variogram(variogram_model, vgm("Gau", nugget = 5, range = 15, psill = 1000))
fit_model5
plot(variogram_model,fit_model5)

fit_model_nested <- fit.variogram(variogram_model,
                                  vgm(
                                    psill=60,
                                    model="Sph", 
                                    range=10,
                                    nugget=20,
                                    add.to = vgm(psill=230,model="Sph", range=30)
                                  )
)

fit_model_nested
plot(variogram_model,fit_model_nested)

models <- list(
  "Exponential" = vgm("Exp", nugget = 1, range = 10, psill = 600),
  "Gaussian" = vgm("Gau", nugget = 5, range = 15, psill = 1000),
  "Spherical" = vgm("Sph", nugget = 2, range = 20, psill = 2000),
  "Nested spherical" = vgm(
    psill=60,
    model="Sph", 
    range=10,
    nugget=20,
    add.to = vgm(psill=230,model="Sph", range=30)
  )
)


results <- lapply(models, function(model) {
  krige.cv(pm25 ~ 1, sp_data, model = model)
})

# Calculation performance indicators 计算性能指标
performance <- lapply(results, function(res) {
  residuals <- res$observed - res$var1.pred
  rmse <- sqrt(mean(residuals^2))             # RMSE
  mae <- mean(abs(residuals))                 # MAE
  mape <- mean(abs(residuals / res$observed)) * 100 # MAPE
  c(RMSE = rmse, MAE = mae, MAPE = mape)
})

# Convert to data frame 转换为数据框 RMSE      MAE     MAPE 
performance <- as.data.frame(do.call(rbind, performance))
performance$Model <- rownames(performance)
print(performance)


#########
#  Generate a higher density of interpolation points 生成更高密度的插值点
bbox <- st_bbox(beijing_map) # 提取边界框
x_range <- seq(bbox["xmin"], bbox["xmax"], length.out = 200) # 经度
y_range <- seq(bbox["ymin"], bbox["ymax"], length.out = 200) # 纬度
grid <- expand.grid(x = x_range, y = y_range) # 生成更高密度的网格
coordinates(grid) <- ~x+y
proj4string(grid) <- proj4string(sp_data)

#  Interpolation calculation and generation of raster 插值计算并生成栅格
krige_result <- krige(pm25 ~ 1, sp_data, newdata = grid, model = fit_model_nested)
krige_result
head(krige_result)
raster_result <- rasterFromXYZ(as.data.frame(krige_result)[, c("x", "y", "var1.pred")])

# Clip the raster to the Beijing area 裁剪栅格到北京市范围
beijing_map_sp <- as(beijing_map, "Spatial") # Convert to Spatial format
raster_clipped <- mask(raster_result, beijing_map_sp)

# 5.Visualization 可视化
raster_sf <- as.data.frame(as(raster_clipped, "SpatialPixelsDataFrame")) # Convert to dataframe
colnames(raster_sf) <- c("PM2.5", "x", "y") 

ggplot() +
  geom_sf(data = beijing_map, fill = NA, color = "black") +
  geom_raster(data = raster_sf, aes(x = x, y = y, fill = PM2.5), alpha = 0.8) +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "PM2.5 Kriging Interpolation",
       fill = "PM2.5") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal()

#####
install.packages("readxl") 
install.packages("sf") 
install.packages("spdep") 
install.packages("tmap") 
install.packages("tidyverse") 
install.packages("spatialreg") 
library(readxl)
library(sf)
library(spdep)
library(tmap)
library(tidyverse)   
library(spatialreg)

# Read observation data and map data
data<-Beislm_
data
beijing_map <- st_read("/Users/jj/Desktop/paper/2(1)/beijing.json")

# Name the columns (in order: number, X, Y, PM2.5, temperature, wind speed, variable3, variable4, variable5)
colnames(data) <- c("ID", "X", "Y", "PM25", "Popu", "Temp", "SO2", "O3", "NO2")
data$Popu <- as.numeric(data$Popu)

# Creating an sf object
coordinates <- st_as_sf(data, coords = c("X", "Y"), crs = 4326)
# Constructing the adjacency weight matrix (K nearest neighbors)
coords_mat <- st_coordinates(coordinates)
knn <- knearneigh(coords_mat, k = 4)
nb <- knn2nb(knn)
lw <- nb2listw(nb, style = "W")

# Remove geometric information and keep only data
data_nogeo <- st_drop_geometry(coordinates)

# Building the formula
formula <- PM25 ~ Popu + Temp + SO2 + O3 + NO2

# Fitting spatial lag models (SLM)
slm_model <- lagsarlm(formula, data = data_nogeo, listw = lw)

summary(slm_model)




library(sf)
library(spdep)
library(tmap)
library(dplyr)
# Initialize empty lines and groups lists
line_list <- list()
group_list <- c()
# Construct a LINESTRING for each edge and record its starting point ID (for group coloring)
for (i in 1:length(nb)) {
  for (j in nb[[i]]) {
    line <- st_linestring(rbind(st_coordinates(coordinates[i, ]), st_coordinates(coordinates[j, ])))
    line_list <- append(line_list, list(line))
    group_list <- append(group_list, i)  # 或 paste(i, j) 如果你想每条边唯一分组
  }
}

# Create sf object: lines + groups
edges_sf <- st_sf(
  group = as.factor(group_list),
  geometry = st_sfc(line_list, crs = st_crs(coordinates))
)

# Visualization
tm_shape(beijing_map) +
  tm_polygons(border_col = "grey70", lwd = 1) +
  tm_shape(edges_sf) +
  tm_lines(col = "group", lwd = 1.2) +  # 根据 group 分配颜色
  tm_shape(coordinates) +
  tm_dots(col = "red", size = 0.15) +
  tm_title("Spatial Neighborhood Map of Beijing Monitoring Sites（K = 4）")


residuals_sar <- residuals(slm_model)
# Moran's I test on residuals
moran_test <- moran.test(residuals_sar, lw)
print(moran_test)

residuals_sar <- residuals(slm_model)



#QQ plot
qqnorm(residuals_sar,
       main = "QQ plot: residual normality test")
qqline(residuals_sar, col = "red", lwd = 2)

hist(residuals_sar,
     breaks = 10,
     col = "skyblue",
     main = "残差直方图",
     xlab = "残差值")
shapiro.test(residuals_sar)

coordinates$PM25_pred <- predict(slm_model)
coordinates$residuals <- residuals(slm_model)

# Store the predicted value into the variable pre
pre <- coordinates$PM25_pred
pre
#Projection consistency
beijing_map <- st_transform(beijing_map, crs = st_crs(coordinates))
# Residual plot
coordinates$PM25_pred <- predict(slm_model)
tm_shape(beijing_map) +
  tm_polygons() +
  tm_shape(coordinates) +
  tm_symbols(col = "PM25_pred",
             size = 0.2,
             palette = "-RdYlGn",  # 从绿色到红色（绿色低，红色高）
             style = "quantile",
             title.col = "预测PM2.5") +
  tm_layout(title = "PM2.5 spatial lag model prediction")

###
install.packages("dplyr")
library(readxl)
library(sp)
library(gstat)
library(sf)
library(ggplot2)
library(dplyr)
library(raster) 
library(ggplot2)
library(sf)

pm_data <- slmBei_
pm_data 
coords <- pm_data[, c("x", "y")]
coords
pm25 <- pm_data$pm2.5
pm25 
sp_data_1 <- SpatialPointsDataFrame(coords, data.frame(pm25 = pm25),
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))
sp_data_1

variogram_model <- variogram(pm25 ~ 1, sp_data_1)  # Calculate the variogram
variogram_model

fit_model3 <- fit.variogram(variogram_model, vgm("Sph", nugget = 2, range = 20, psill = 2000))
fit_model3
plot(variogram_model,fit_model3)

fit_model4 <- fit.variogram(variogram_model, vgm("Exp", nugget =1, range = 10, psill = 600))
fit_model4
plot(variogram_model,fit_model4)

fit_model5 <- fit.variogram(variogram_model, vgm("Gau", nugget = 5, range = 15, psill = 1000))
fit_model5
plot(variogram_model,fit_model5)

fit_model_nested <- fit.variogram(variogram_model,
                                  vgm(
                                    psill=60,
                                    model="Sph", 
                                    range=10,
                                    nugget=20,
                                    add.to = vgm(psill=230,model="Sph", range=30)
                                  )
)

fit_model_nested
plot(variogram_model,fit_model_nested)

models <- list(
  "Exponential" = vgm("Exp", nugget = 1, range = 10, psill = 600),
  "Gaussian" = vgm("Gau", nugget = 5, range = 15, psill = 1000),
  "Spherical" = vgm("Sph", nugget = 2, range = 20, psill = 2000),
  "Nested spherical" = vgm(
    psill=60,
    model="Sph", 
    range=10,
    nugget=20,
    add.to = vgm(psill=230,model="Sph", range=30)
  )
)


results <- lapply(models, function(model) {
  krige.cv(pm25 ~ 1, sp_data, model = model)
})

# Calculation performance indicators 计算性能指标
performance <- lapply(results, function(res) {
  residuals <- res$observed - res$var1.pred
  rmse <- sqrt(mean(residuals^2))             # RMSE
  mae <- mean(abs(residuals))                 # MAE
  mape <- mean(abs(residuals / res$observed)) * 100 # MAPE
  c(RMSE = rmse, MAE = mae, MAPE = mape)
})

# Convert to data frame 转换为数据框 RMSE      MAE     MAPE 
performance <- as.data.frame(do.call(rbind, performance))
performance$Model <- rownames(performance)
print(performance)


#########
#  Generate a higher density of interpolation points 生成更高密度的插值点
bbox <- st_bbox(beijing_map) # 提取边界框
x_range <- seq(bbox["xmin"], bbox["xmax"], length.out = 200) # 经度
y_range <- seq(bbox["ymin"], bbox["ymax"], length.out = 200) # 纬度
grid <- expand.grid(x = x_range, y = y_range) # 生成更高密度的网格
coordinates(grid) <- ~x+y
proj4string(grid) <- proj4string(sp_data)

#  Interpolation calculation and generation of raster 插值计算并生成栅格
krige_result <- krige(pm25 ~ 1, sp_data, newdata = grid, model = fit_model4)
krige_result
head(krige_result)
raster_result <- rasterFromXYZ(as.data.frame(krige_result)[, c("x", "y", "var1.pred")])

# Clip the raster to the Beijing area 裁剪栅格到北京市范围
beijing_map_sp <- as(beijing_map, "Spatial") # Convert to Spatial format
raster_clipped <- mask(raster_result, beijing_map_sp)

# 5.Visualization 可视化
raster_sf <- as.data.frame(as(raster_clipped, "SpatialPixelsDataFrame")) # Convert to dataframe
colnames(raster_sf) <- c("PM2.5", "x", "y") 

ggplot() +
  geom_sf(data = beijing_map, fill = NA, color = "black") +
  geom_raster(data = raster_sf, aes(x = x, y = y, fill = PM2.5), alpha = 0.8) +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "PM2.5 Kriging Interpolation",
       fill = "PM2.5") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal()







