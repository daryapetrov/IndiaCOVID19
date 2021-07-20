#setwd("/Users/dpetrov/COVID-19-Nonparametric-Inference-master/code")
library(ggplot2)
library(VIM)

load("kerala_df.Rda")
data = kerala_df[,c("Date","State","Confirmed","Recovered","Deceased","Tested","Hospitalized")]

sum(data["Date"]==0) #0
sum(data["State"]==0) #0
sum(data["Confirmed"]==0) #0
sum(data["Recovered"]==0) #0
sum(data["Deceased"]==0) #0
sum(data["Tested"]==0) #0
sum(data["Hospitalized"]==0) #2
which(data["Hospitalized"]==0) #296,322
data["Hospitalized"][which(data["Hospitalized"]==0),]=NA

ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")

#2020-11-05, 218 wrong data input?
#2020-11-08,221 high peak, wrong data input?
data["Hospitalized"][c(218,221),] = NA

#impute 
data = kNN(data, variable = "Hospitalized",k=6)
ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")

#lowess
smoothedH = lowess(data$Date, data$Hospitalized,f=1/32)$y
ggplot(data, aes(x=Date, y=smoothedH/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")

#delta Hospital
deltaHospital = c(0,diff(data$Hospitalized))
smooth_deltaHospital = lowess(data$Date, deltaHospital,f=1/32)$y
ggplot(data, aes(x=Date, y=smooth_deltaHospital)) + geom_line() + ylab("Change in Daily Hospitalizations") + ggtitle("Change in Daily Hospitalizations")




#impute nearest 6 using mean
# impute_nearest_neighbors <- function(data, row_name, index, num_neighbors = 3){
#   total = num_neighbors*2
#   avg = (sum(data[row_name][(index-3):(index-1),]) + sum(data[row_name][(index+1):(index+3),]))/total
#   data[row_name][index,] = round(avg)
#   return(data)
# }
# 
# #impute NA data
# data = impute_nearest_neighbors(data = data,row_name = "Hospitalized",index = 296)
# data = impute_nearest_neighbors(data = data,row_name = "Hospitalized",index = 322)
# ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")
# 
# 
# #2020-11-05, 218 wrong data input?
# data = impute_nearest_neighbors(data = data,row_name = "Hospitalized",index = 218)
# ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")
# 
# 
# #2020-11-08,221 high peak, wrong data input?
# data = impute_nearest_neighbors(data = data,row_name = "Hospitalized",index = 221)
# ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")


# #lowess
# smoothedH = lowess(data$Date, data$Hospitalized,f=1/32)$y
# ggplot(data, aes(x=Date, y=smoothedH/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")
# 
# #delta hospitalization
# deltaHospital = c(0,diff(data$Hospitalized))
# plot(deltaHospital,type="l")
# 
# data = cbind(data, deltaHospital)
# ggplot(data, aes(x=Date, y=deltaHospital)) + geom_line() + ylab("Hospitalizations") + ggtitle("Hospitalizations")
# 
# smoothed_deltaH = lowess(data$Date, data$smoothedH,f=1/32)$y
# ggplot(data, aes(x=Date, y=smoothed_deltaH)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")
# 
# #what is going on? 
# m = c(0,diff(smoothedH))
# ggplot(data, aes(x=Date, y=m)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")
# 
# data = cbind(data,c(0,diff(smoothedH)))
# data$smoothed_deltaH = diff(smoothedH)
# #cumulative sum hospitalizations
# # data$CumHosp = unlist(cumsum(data["Hospitalized"]))
# # ggplot(data, aes(x=Date, y=CumHosp/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Cumulative Hospitalizations")
# 
# View(data)
# 
# unlist(data["Hospitalized"][,])


#2020-11-08,221

#delta hopsital
#fix outliers
#smoothing for pictorial purposes 
