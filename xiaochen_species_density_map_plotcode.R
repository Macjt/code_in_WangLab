
# 加载所需包
library(ggplot2)
library(reshape2)

setwd(" / / / /")
# 1. 准备数据
data <- data.frame(  x = c(1,1,1,2,2,2,3,3,3,4,4,4),
                     y = c(1,2,3,1,2,3,1,2,3,1,2,3),
                     z = c(98,89,103,116,98,109,135,116,121,78,104,120) )
library(readxl)
data<- read_excel("10.31粉虱处理.xls") 

# 2. 转换数据为矩阵形式（等高线图需要网格数据）
# 先按x和y重塑为宽表
# data_wide <- dcast(data, y ~ x, value.var = "z")
# # 提取数值部分作为矩阵（行= y，列= x）
# z_matrix <- as.matrix(data_wide[, -1])  # 去除第一列y的标签
# rownames(z_matrix) <- data_wide$y      # 行名对应y值
library(dplyr)
# ---- 进行二维插值（让图平滑） ----
interp_res <- akima::interp(
  x = data$x, y = data$y, z = data$z,
  xo = seq(min(data$x), max(data$x), length = 200),
  yo = seq(min(data$y), max(data$y), length = 200) )

# 将插值结果转换为可绘图的数据框
interp_df <- data.frame(
  x = rep(interp_res$x, times = length(interp_res$y)),
  y = rep(interp_res$y, each = length(interp_res$x)),
  z = as.vector(interp_res$z) ) %>% na.omit()
# 计算不同的标准化z值
interp_df$z_scale <- scale(interp_df$z)
interp_df$z_median <-    (interp_df$z / median(interp_df$z, na.rm = TRUE)) 
interp_df$z_1 <-  (interp_df$z - min(interp_df$z)) / (max(interp_df$z) - min(interp_df$z))

# ---- 绘制密度梯度图 ----
library(patchwork)
ap <- ggplot(data, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  geom_contour(aes(z = z), color = "white", alpha = 0.6) +
  scale_fill_gradientn( colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                        name = "Z Value" ) +
  coord_fixed() + theme_minimal(base_size = 14) +
  labs(title = "Map Based on Z Values", x = "X",  y = "Y")
bp <- ggplot(interp_df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  geom_contour(aes(z = z), color = "white", alpha = 0.6) +
  scale_fill_gradientn( colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                        name = "Z Value"  ) +
  coord_fixed() + theme_minimal(base_size = 14) +
  labs(title = "Density Gradient Map Based on Z Values", x = "X",  y = "Y")
cp <- ggplot(interp_df, aes(x = x, y = y, fill = z_scale)) +
  geom_tile() +
  geom_contour(aes(z = z), color = "white", alpha = 0.6) +
  scale_fill_gradientn( colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                        name = "Z Value"  ) +
  coord_fixed() + theme_minimal(base_size = 14) +
  labs(title = "Density Gradient Map Based on Z Values", x = "X",  y = "Y")
dp <- ggplot(interp_df, aes(x = x, y = y, fill = z_median)) +
  geom_tile() +
  geom_contour(aes(z = z), color = "white", alpha = 0.6) +
  scale_fill_gradientn( colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                        name = "Z Value"  ) +
  coord_fixed() + theme_minimal(base_size = 14) +
  labs(title = "Density Gradient Map Based on Z Values", x = "X",  y = "Y")
ep <- ggplot(interp_df, aes(x = x, y = y, fill = z_1)) +
  geom_tile() +
  # geom_contour(aes(z = z), color = "white", alpha = 0.6) +
  scale_fill_gradientn( colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                        name = "Z Value\n"  ) +
  coord_fixed() + theme_minimal(base_size = 14) + theme_bw()+
  theme( axis.text  = element_text(size=20,color = "black"),, 
         axis.title = element_text(size=20), 
         axis.ticks.length = unit(0.1,'cm'), 
         axis.ticks = element_line(colour = "black",size = .5),
         legend.title = element_text(size=18,color = "black"), 
         legend.text = element_text(size=18,color = "black"),  
         legend.key.height=unit(0.5,'inches'), legend.key.width=unit(0.3,'inches'), 
         plot.title = element_text(hjust = 0,size=20,color = "black" ))  + 
  labs(title = "Density Gradient Map", x = "X",  y = "Y") 
# (ap/ep)
ep
ggsave(ep, filename="1031粉虱处理密度梯度图.pdf",height=8, width = 10, dpi=300)
(ap/ ep)

library(metR)
ggplot(interp_df, aes(x = x, y = y, z = z_1)) +
  geom_contour_fill(breaks = MakeBreaks(binwidth = 0.05, exclude = 0)) +
  scale_fill_divergent(breaks = MakeBreaks(binwidth = 0.05, exclude = 0)) +
  scale_fill_gradientn( colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                        name = "Z Value\n"  ) 
ggplot(interp_df, aes(x = x, y = y, z = z)) +
  scale_fill_divergent(breaks = MakeBreaks(binwidth = 2, exclude = 0)) +
  geom_contour_fill(breaks = MakeBreaks(binwidth = 2, exclude = 0),na.fill = TRUE) +#,kriging=T
  scale_fill_gradientn( colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                        name = "Z Value\n"  ) 
