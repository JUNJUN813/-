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

#  Build model
variogram_model <- variogram(pm25 ~ 1, sp_data)  # Calculate the variogram
variogram_model
#球状模型Spherical Model
fit_model <- fit.variogram(variogram_model, vgm("Sph", nugget = 0.1, range = 20, psill = 1))
fit_model
plot(variogram_model,fit_model)
fit_model2 <- fit.variogram(variogram_model, vgm("Sph", nugget = 10, range = 15, psill = 100))
fit_model2
plot(variogram_model,fit_model2)
#If the data variation tends to be stable within a short range, can increase psill appropriately and reduce range to enhance the model's ability to fit the experimental variation function.。
fit_model3 <- fit.variogram(variogram_model, vgm("Sph", nugget = 5, range = 15, psill = 1000))
fit_model3
plot(variogram_model,fit_model3)
#Exponential Model
fit_model4 <- fit.variogram(variogram_model, vgm("Exp", nugget = 5, range = 15, psill = 1000))
fit_model4
plot(variogram_model,fit_model4)
#Gaussian Model
fit_model5 <- fit.variogram(variogram_model, vgm("Gau", nugget = 5, range = 15, psill = 1000))
fit_model5
plot(variogram_model,fit_model5)
# nested spherical model
fit_model_nested <- fit.variogram(variogram_model,
                                  vgm(
                                    psill=280,
                                    model="Sph", 
                                    range=15,
                                    nugget=5,
                                    add.to = vgm(psill=400,model="Sph", range=40)
                                  )
)

fit_model_nested
plot(variogram_model,fit_model_nested)

#########
# 加载必要的库
library(readxl)
library(sp)
library(gstat)
library(dplyr)
library(sf)
library(ggplot2)

#Leave-One-Out Cross-Validation 留一法交叉验证
pm_data <- Bei
coords <- pm_data[, c("x", "y")]
pm25 <- pm_data$pm2.5

sp_data <- SpatialPointsDataFrame(coords, data.frame(pm25 = pm25),
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))

fit_model3 <- fit.variogram(variogram_model, vgm("Sph", nugget = 5, range = 15, psill = 1000))
fit_model3
plot(variogram_model,fit_model3)

fit_model4 <- fit.variogram(variogram_model, vgm("Exp", nugget = 5, range = 15, psill = 1000))
fit_model4
plot(variogram_model,fit_model4)

fit_model5 <- fit.variogram(variogram_model, vgm("Gau", nugget = 5, range = 15, psill = 1000))
fit_model5
plot(variogram_model,fit_model5)

fit_model_nested <- fit.variogram(variogram_model,
                                  vgm(
                                    psill=280,
                                    model="Sph", 
                                    range=15,
                                    nugget=5,
                                    add.to = vgm(psill=400,model="Sph", range=40)
                                  )
)

fit_model_nested
plot(variogram_model,fit_model_nested)

models <- list(
  "Exponential" = vgm("Exp", nugget = 5, range = 15, psill = 1000),
  "Gaussian" = vgm("Gau", nugget = 5, range = 15, psill = 1000),
  "Spherical" = vgm("Sph", nugget = 5, range = 15, psill = 1000),
  "Nested spherical" = vgm(
    psill=280,
    model="Sph", 
    range=15,
    nugget=5,
    add.to = vgm(psill=400,model="Sph", range=40)
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

