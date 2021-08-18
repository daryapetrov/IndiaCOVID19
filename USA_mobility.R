setwd("/Users/dpetrov/COVID-19-Nonparametric-Inference-master/USA_data_processing")

library(magrittr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(lubridate)
library(plyr)
library(gridExtra)

#Read Google mobility data
df_google = read.csv("../data/EconomicTracker/data/Google Mobility - State - Daily.csv",header = TRUE)
#df_google = read.csv("~/Dropbox/covid/data/EconomicTracker/data/Google Mobility - State - Daily.csv",header = TRUE)
df_google=df_google %>% na_if(".")
df_google[,5:ncol(df_google)]=apply(df_google[,5:ncol(df_google)], 2, as.numeric)
df_google$date=paste(paste(df_google$year, df_google$month, sep = "-"), df_google$day, sep = "-") %>% as.Date("%Y-%m-%d")
df_google=df_google[,-c(1,2,3)]

df_google$average <- rowMeans(df_google[,2:7], na.rm = T)

b = df_google
