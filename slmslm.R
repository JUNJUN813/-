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

# 读取观测数据和地图数据
data<-Beislm_
data
beijing_map <- st_read("/Users/jj/Desktop/paper/2(1)/beijing.json")

# 命名列（假设顺序为：编号, X, Y, PM2.5, 温度, 风速, 变量3, 变量4, 变量5）
colnames(data) <- c("ID", "X", "Y", "PM25", "Popu", "Temp", "SO2", "O3", "NO2")

data$Popu <- as.numeric(data$Popu)

# 创建 sf 对象
coordinates <- st_as_sf(data, coords = c("X", "Y"), crs = 4326)
# 构建邻接权重矩阵（K近邻）
coords_mat <- st_coordinates(coordinates)
knn <- knearneigh(coords_mat, k = 4)
nb <- knn2nb(knn)
lw <- nb2listw(nb, style = "W")

# 去掉几何信息，仅保留数据
data_nogeo <- st_drop_geometry(coordinates)

# 构建公式
formula <- PM25 ~ Popu + Temp + SO2 + O3 + NO2

# 拟合空间滞后模型（SLM）
slm_model <- lagsarlm(formula, data = data_nogeo, listw = lw)

# 查看模型摘要
summary(slm_model)




library(sf)
library(spdep)
library(tmap)
library(dplyr)
# 初始化空的线和分组列表
line_list <- list()
group_list <- c()
# 为每条边构建 LINESTRING 并记录其起点ID（用于分组着色）
for (i in 1:length(nb)) {
  for (j in nb[[i]]) {
    line <- st_linestring(rbind(st_coordinates(coordinates[i, ]), st_coordinates(coordinates[j, ])))
    line_list <- append(line_list, list(line))
    group_list <- append(group_list, i)  # 或 paste(i, j) 如果你想每条边唯一分组
  }
}

# 创建 sf 对象：线+分组
edges_sf <- st_sf(
  group = as.factor(group_list),
  geometry = st_sfc(line_list, crs = st_crs(coordinates))
)

# 可视化（v4语法）
tm_shape(beijing_map) +
  tm_polygons(border_col = "grey70", lwd = 1) +
  tm_shape(edges_sf) +
  tm_lines(col = "group", lwd = 1.2) +  # 根据 group 分配颜色
  tm_shape(coordinates) +
  tm_dots(col = "red", size = 0.15) +
  tm_title("Spatial Neighborhood Map of Beijing Monitoring Sites（K = 4）")


residuals_sar <- residuals(slm_model)
# 对残差进行 Moran's I 检验
moran_test <- moran.test(residuals_sar, lw)
print(moran_test)

residuals_sar <- residuals(slm_model)



# QQ 图
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

# 存储预测值到变量 pre
pre <- coordinates$PM25_pred
pre
#投影一致
beijing_map <- st_transform(beijing_map, crs = st_crs(coordinates))
#残差图
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
