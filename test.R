setwd("/Users/dpetrov/COVID-19-Nonparametric-Inference-India/India/Kerala")
load("keralaMobilityAvg.Rda")
load("kerala_data.Rda")

data2020 = data[-c(seq(275,364,1)),]

#we don't have data for the date 2020-10-16 in kerala_data.Rda
length(data2020[,1])
length(keralaMobilityAvg[,4])

mobility_rm = keralaMobilityAvg[keralaMobilityAvg$date != "2020-10-16", ]  
dim(mobility_rm)


