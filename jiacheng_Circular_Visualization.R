# https://jokergoo.github.io/circlize_book/book/
# devtools::install_github("jokergoo/circlize")
# https://ying-ge.github.io/FigureYa/FigureYa236circGroup/FigureYa236circGroup.html#1_Academic_Citation
# https://blog.csdn.net/qq_34069180/article/details/111637115
# https://vlambda.com/wz_wDEqJXscUQ.html

install.packages(c("plotly","sunburstR","tidyverse","ragg","magrittr","ggsunburst"))

# https://vlambda.com/wz_wDEqJXscUQ.html
library(plotly)
#https://stackoverflow.com/questions/12926779/how-to-make-a-sunburst-plot-in-r-or-python
library(sunburstR)
#ggsunburst 包的旭日图，参考自
#https://stackoverflow.com/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot/37015211
#rPython 仅在 Linux 或 Mac 下可用 install.packages() 安装
#Windows 下可通过该方法安装：https://github.com/cjgb/rPython-win
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('rPython')) install.packages('rPython')
install.packages('http://genome.crg.es/~didac/ggsunburst/ggsunburst_0.0.9.tar.gz', repos = NULL, type = 'source')
library(ggsunburst)


# https://blog.csdn.net/qq_34069180/article/details/111637115
# install.packages("tidyverse")
library(tidyverse)
# install.packages('ragg')
library(ragg)
library(magrittr)

# https://ying-ge.github.io/FigureYa/FigureYa236circGroup/FigureYa236circGroup.html#1_Academic_Citation
# Xiaofan Lu, et al. (2025). FigureYa: A Standardized Visualization Framework for 
# Enhancing Biomedical Data Interpretation and Research Efficiency. iMetaMed. 
# https://doi.org/10.1002/imm3.70005
source("install_dependencies.R")  # 加载依赖安装脚本 # Load dependency installation script








