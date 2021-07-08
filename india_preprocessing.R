library(ggplot2)

setwd("/Users/dpetrov/OneDrive/College/2020-2021 (Yr 3)/Spring 2021/STA 199/R")
df_all = read.csv("states.csv")
states_india  = unique(unlist(df_all["State"], use.names=FALSE))
#state_wise_daily, with info on every state everyday, doesn't start recording until 14 Mar 2020
#index 548 starts dates 2020-04-01

states_india = states_india[-2] #remove India
df = df_all[-c(1:547),]
india_data = data.frame()
for(state in states_india){
  india_data = rbind(india_data,subset(df,State == state) )
}
# save(india_data, file = "indiaData.Rda")

kerala_data = subset(india_data,State == "Kerala")
kerala_data$Date <- as.Date( as.character(kerala_data$Date), format="%Y-%m-%d")


#last date
kerala_data$Date[length(kerala_data$Date)]

conf_sm = lowess(kerala_data$Confirmed, f = 1/10)$y
plot(conf_sm,type="l")
diff1 = diff(conf_sm)
plot(diff1,type="l")
#diff1 = diff(kerala_data$Confirmed)
#diff2 = diff(kerala_data$Confirmed[-1])
diff2 = diff(diff1)
plot(diff2,type="l")
CIR1 = diff2/diff1[-1]
plot(CIR1)
smoothedCIR = lowess(CIR, f=1/8)$y
plot(smoothedCIR,type="l")



shortDiff1 = head(diff1,-1)
CIR = diff2/diff1[-1]
#CIR = (diff2-shortDiff1)/diff1[-1]
plot(CIR,type="l")
smoothedCIR = lowess(CIR, f=1/17)$y
plot(smoothedCIR,type="l")

CFR = 100*kerala_data$Deceased/kerala_data$Confirmed
kerala_data = cbind(kerala_data, CFR)


CFR_lowess = lowess(CFR, f=1/20)$y
plot(CFR,type="l", main="bandwidth = 1/17")
#plot(smoothedCFR,type="l")
ggplot(kerala_data, aes(x=Date, y=CFR)) + geom_line() + ylab("CFR")
ggplot(kerala_data, aes(x=Date, y=CFR_lowess)) + geom_line() + ylab("CFR")




#PLOTS
#cumulative confirmed cases over time
kerala_data = subset(india_data,State == "Kerala")
kerala_data$Date <- as.Date( as.character(kerala_data$Date), format="%Y-%m-%d")
ggplot(kerala_data, aes(x=Date, y=Confirmed/1000)) + geom_line() + ylab("Cumulative Confirmed Cases (thousands)") + ggtitle("Cumulative confirmed cases count per day in Kerala")

#delta_C over time
#delta_C = rep(0, dim(kerala_data)[1])
#kerala_data = cbind(kerala_data, delta_C)
# for(i in 2:dim(kerala_data)[1]){
#   kerala_data$delta_C[i] = kerala_data$Confirmed[i] -kerala_data$Confirmed[i-1]
# }
delta_C = c(0,diff(kerala_data$Confirmed))
kerala_data = cbind(kerala_data, delta_C)


ggplot(kerala_data, aes(x=Date, y=delta_C/1000)) + geom_line() + ylab("Change in daily confirmed cases (thousands)") + ggtitle("Change in confirmed cases per day in Kerala")


smoothedC = lowess(kerala_data$Date, kerala_data$Confirmed)$y
ggplot(kerala_data, aes(x=Date, y=smoothedC)) + geom_line()

ggplot(kerala_data, aes(x=Date, y=delta_C)) + geom_line() + ylab("delta Confirmed")


bandwidth = 1/16
smoothed_deltaC = lowess(kerala_data$Date, kerala_data$delta_C, f=bandwidth)$y
ggplot(kerala_data, aes(x=Date, y=smoothed_deltaC/1000)) + geom_line() +  ylab("Change in daily confirmed cases (thousands)") + ggtitle("Change in confirmed cases per day in Kerala")

smoothed_deltaC = lowess(kerala_data$Date, kerala_data$delta_C)$y
ggplot(kerala_data, aes(x=Date, y=smoothed_deltaC)) + geom_line() + ggtitle("default bandwidth = 2/3") + ylab("delta Confirmed")

ggplot(kerala_data[-1], aes(x=Date, y=d)) + geom_line()



delta_C = diff(kerala_data$Confirmed)
kerala_data = cbind(kerala_data, delta_C)




# #cumulative recivered cases over time
kerala_data = subset(india_data,State == "Kerala")
kerala_data$Date <- as.Date( as.character(kerala_data$Date), format="%Y-%m-%d")
ggplot(kerala_data, aes(x=Date, y=Recovered)) + geom_line()

#delta_R over time
delta_R = rep(0, dim(kerala_data)[1])
kerala_data = cbind(kerala_data, delta_R)
for(i in 2:dim(kerala_data)[1]){
  kerala_data$delta_R[i] = kerala_data$Recovered[i] -kerala_data$Recovered[i-1]
}
ggplot(kerala_data, aes(x=Date, y=delta_R)) + geom_line()

bandwidth = 1/16
smoothed_deltaR = lowess(kerala_data$Date, kerala_data$delta_R, f=bandwidth)$y
ggplot(kerala_data, aes(x=Date, y=smoothed_deltaR)) + geom_line() + ggtitle("bandwidth = 1/16") + ylab("delta Recovered")


#cumulative deceased cases over time
kerala_data = subset(india_data,State == "Kerala")
kerala_data$Date <- as.Date( as.character(kerala_data$Date), format="%Y-%m-%d")
ggplot(kerala_data, aes(x=Date, y=Deceased)) + geom_line()

#delta_D over time
# delta_D = rep(0, dim(kerala_data)[1])
# kerala_data = cbind(kerala_data, delta_D)
# for(i in 2:dim(kerala_data)[1]){
#   kerala_data$delta_D[i] = kerala_data$Deceased[i] -kerala_data$Deceased[i-1]
# }
delta_D = c(0,diff(kerala_data$Deceased))
ggplot(kerala_data, aes(x=Date, y=delta_D)) + geom_line() + ylab("delta Deceased")

bandwidth = 1/14
smoothed_deltaD = lowess(kerala_data$Date,delta_D, f=bandwidth)$y
# kerala_data = cbind(kerala_data, smoothed_deltaD)
ggplot(kerala_data, aes(x=Date, y=smoothed_deltaD)) + geom_line() + ggtitle("bandwidth = 1/14") + ylab("delta Deceased")

#increase bandwidth = smoother, decrease bandwidth more rough

#diff

#######################################################################################



#Qt + Ht = Confirmed - Recovered - Death

#Plot Confirmed, Recovered Deceased


delta_C <- data.frame(t,rep(NA, length(dates_india)-1))
#CIR
for(date_index in 2:length(dates_india)){
  delta_C[date_index-1,2] = confirmed_india[confirmed_india$State=="Kerala",][date_index+1]-confirmed_india[confirmed_india$State=="Kerala",][date_index]
}
colnames(delta_C) = c("t","delta_C")
ggplot(delta_C, aes(x=t, y=delta_C)) + geom_point()

a <- data.frame(dates_india,rep(NA, length(dates_india)-1))


df1 = subset(df,State == "Lakshadweep")
df2 = subset(df,State == "Kerala")
df3 = rbind(df1,df2)


combined = rbind(df_test,df2)



dates_india = unique(unlist(df["Date"], use.names=FALSE))

#not every state has data for everyday
for (i in unique(states_india)){
  print(i)
  print(sum(df["State"]==i))
}


#create empty dataframe with dates as columns and states as rows
confirmed_india = data.frame(matrix(ncol = length(dates_india)+1, nrow = length(states_india)))
colnames(confirmed_india) = c("State", dates_india)
confirmed_india[,1] = states_india
colnames(confirmed_india)[0] = "State"


#inefficient way to do this 
for (state in states_india){
  for(date in dates_india){
    confirmed_india[confirmed_india$State==state,][date] = df[df$State== state & df$Date==date,]["Confirmed"]
  }
}

#confirmed
#create empty dataframe with dates as columns and states as rows
confirmed_india = data.frame(matrix(ncol = length(dates_india)+1, nrow = length(states_india)))
colnames(confirmed_india) = c("State", dates_india)
confirmed_india[,1] = states_india
colnames(confirmed_india)[0] = "State"


#inefficient way to do this 
for (state in states_india){
  for(date in dates_india){
    confirmed_india[confirmed_india$State==state,][date] = df[df$State== state & df$Date==date,]["Confirmed"]
  }
}


#recovered
recovered_india = data.frame(matrix(ncol = length(dates_india)+1, nrow = length(states_india)))
colnames(recovered_india) = c("State", dates_india)
recovered_india[,1] = states_india
colnames(recovered_india)[0] = "State"


#inefficient way to do this 
for (state in states_india){
  for(date in dates_india){
    recovered_india[recovered_india$State==state,][date] = df[df$State== state & df$Date==date,]["Recovered"]
  }
}

#deceased
deceased_india = data.frame(matrix(ncol = length(dates_india)+1, nrow = length(states_india)))
colnames(deceased_india) = c("State", dates_india)
deceased_india[,1] = states_india
colnames(deceased_india)[0] = "State"


#inefficient way to do this 
for (state in states_india){
  for(date in dates_india){
    deceased_india[deceased_india$State==state,][date] = df[df$State== state & df$Date==date,]["Deceased"]
  }
}

delta_C <- data.frame(t,rep(NA, length(dates_india)-1))
#CIR
for(date_index in 2:length(dates_india)){
  delta_C[date_index-1,2] = confirmed_india[confirmed_india$State=="Kerala",][date_index+1]-confirmed_india[confirmed_india$State=="Kerala",][date_index]
}
colnames(delta_C) = c("t","delta_C")
ggplot(delta_C, aes(x=t, y=delta_C)) + geom_point()

a <- data.frame(dates_india,rep(NA, length(dates_india)-1))
