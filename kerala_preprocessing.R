#setwd("/Users/dpetrov/COVID-19-Nonparametric-Inference-master/code")
library(ggplot2)

load("kerala_df.Rda")
data = kerala_df[,c("Date","State","Confirmed","Recovered","Deceased","Tested","Hospitalized")]

#hospitalized not cumulative like confirmed, deceased, recovered

sum(data["Date"]==0) #0
sum(data["State"]==0) #0
sum(data["Confirmed"]==0) #0
sum(data["Recovered"]==0) #0
sum(data["Deceased"]==0) #0
sum(data["Tested"]==0) #0
sum(data["Hospitalized"]==0) #2
which(data["Hospitalized"]==0) #296,322
data["Hospitalized"][which(data["Hospitalized"]==0),]=NA

#impute nearest 6 using mean
impute_nearest_neighbors <- function(data, row_name, index, num_neighbors = 3){
  total = num_neighbors*2
  avg = (sum(data[row_name][(index-3):(index-1),]) + sum(data[row_name][(index+1):(index+3),]))/total
  data[row_name][index,] = round(avg)
  return(data)
}

#impute NA data
data = impute_nearest_neighbors(data = data,row_name = "Hospitalized",index = 296)
data = impute_nearest_neighbors(data = data,row_name = "Hospitalized",index = 322)
ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")


#2020-11-05, 218 wrong data input?
data = impute_nearest_neighbors(data = data,row_name = "Hospitalized",index = 218)
ggplot(data, aes(x=Date, y=Hospitalized/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")

#lowess
smoothedH = lowess(data$Date, data$Hospitalized,f=1/16)$y
ggplot(data, aes(x=Date, y=smoothedH/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Hospitalizations")



#cumulative sum hospitalizations
data$CumHosp = unlist(cumsum(data["Hospitalized"]))
ggplot(data, aes(x=Date, y=CumHosp/1000)) + geom_line() + ylab("Hospitalizations(thousands)") + ggtitle("Cumulative Hospitalizations")

View(data)
#2020-11-08,221
