##Tasks:
#1. create new column for transformed data (X)
#2. graphs from the paper 
#3. satarupa's code to fit the data

#calculate means for mobility data by taking the average of the columns for each row 
#
setwd("/Users/dpetrov/COVID-19-Nonparametric-Inference-India/India/Kerala")
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
ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")


#impute from library VIM
data = cbind(data, kNN(data, variable = "Hospitalized",k=6)$Hospitalized)
names(data)[names(data) == names(data)[8]] = "impHospitalized"
ggplot(data, aes(x=Date, y=impHospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")

#try sqrt(n) in lowess
n= dim(data)[1]
smoothed_impHospitalized = lowess(data$Date, data$impHospitalized,f=1/sqrt(n))$y
ggplot(data, aes(x=Date, y=smoothed_impHospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")

#delta Hospital
delta_impHospitalized = c(0,diff(data$impHospitalized))
ggplot(data, aes(x=Date, y=delta_impHospitalized)) + geom_line() + ylab("Change in Daily Hospitalizations") + ggtitle("Change in Daily Hospitalizations")
smoothed_deltaHospital = lowess(data$Date, delta_impHospitalized,f=1/sqrt(n))$y
ggplot(data, aes(x=Date, y=smoothed_deltaHospital)) + geom_line() + ylab("Change in Daily Hospitalizations") + ggtitle("Change in Daily Hospitalizations")
data = cbind(data,delta_impHospitalized )


save(data, file = "kerala_data.Rda")


#CIR
diff1 = diff(data$Confirmed)
diff2 = diff(diff1)
diff1_sm = lowess(diff1, f = 1/10)$y
diff2_sm = lowess(diff2, f = 1/10)$y
CIR = diff2_sm/diff1_sm[-1]
plot(CIR, type = "l", main="bandwidth = 1/10")
ggplot(data[1:362,], aes(x=Date, y=CIR)) + geom_line() + ylab("CIR")


#CFR
CFR = 100*data$Deceased/data$Confirmed
data = cbind(data, CFR)

plot(CFR,type="l", main="bandwidth = 1/17")
CFR_lowess = lowess(CFR, f=1/20)$y
plot(CFR_lowess,type="l", main="bandwidth = 1/17")
ggplot(data[1:364,], aes(x=Date, y=CFR_lowess)) + geom_line() + ylab("CFR")




##smoothing hospital first, then doing delta. very blocky. 

# data = cbind(data,smoothedH)
# delta_smoothH = c(0,diff(smoothedH))
# data = cbind(data,delta_smoothH)
# 
# 
# 
# #blocky
# ggplot(data, aes(x=Date, y=delta_smoothH)) + geom_line() + ylab("Change in Daily Hospitalizations") + ggtitle("Change in Daily Hospitalizations")
# #slightly smoother than smooth_deltaHospital
# smooth_delta_smoothH = lowess(data$Date, delta_smoothH,f=1/32)$y
# ggplot(data, aes(x=Date, y=smooth_delta_smoothH)) + geom_line() + ylab("Change in Daily Hospitalizations") + ggtitle("Change in Daily Hospitalizations")
# 
# plot(data$Hospitalized,type="l")
# smoothedH = lowess(data$Date, data$Hospitalized,f=1/10)$y
# 
# plot(data$Date,smoothedH,type="l")
# 
# delta_smoothH = c(0,diff(smoothedH))
# plot(data$Date,delta_smoothH,type="l")
# 



# smoothedH = lowess(data$Date, data$Hospitalized,f=1/10)$y
# 
# 
# plot(delta_smoothH,type="l")
# 
# plot(lowess(delta_smoothH,f=1/32)$y,type="l")
# plot(lowess(data$Date,delta_smoothH,f=1/32)$y,type="l")
# plot(smoothedH)









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
