# Pre-install "https://www.xquartz.org/"
# install.packages("webshot2")
# install.packages("flextable")

library(gtsummary)
theme_gtsummary_mean_sd() 
# To reset median IQR; reset_gtsummary_theme()
# .libPaths(c(.libPaths(), "/Users/Sueyoshi/opt/miniconda3/lib/R/library"))
library(flextable)
library(tidyverse)
library(MASS)
library(stringr)
library(pROC)

library(ggalluvial); library(cowplot); library(ggsignif)
