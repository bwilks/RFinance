#Load t
#Install package for fread() and col_date()
#install.packages("data.table")
library(readr)
library(quantmod)
library(TTR)
library(broom)
library(tidyr)

#Read file "Stock_yahoo_individual_tasks.txt" for instructions on how to update
#Load the file into the GSPC data table. Convert Date to date format
#GSPC<-fread("https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=-631152000&period2=1666396800&interval=1wk&events=history&includeAdjustedClose=true", colClasses=c(Date=col_date(format = "%Y-%m-%d")))

#GSPCold<-fread("https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=-631152000&period2=1666396800&interval=1wk&events=history&includeAdjustedClose=true")

#Set date variable
Fromdate = "1958-01-01"
Todate = "2078-12-31"

# switch to quantmod
getSymbols(Symbols = "^GSPC", 
           src = "yahoo", 
           #periodicity = "weekly",
           periodicity = "daily",
           from = Fromdate,
           to = Todate,
           auto.assign = TRUE)

#Adds column to give the percentage change from the opening price on the first day of the year to the closing price on the last day of the year
#Not adjusted for inflation and not including dividends
#GSPC$WR<-weeklyReturn(GSPC)
GSPC$AR<-annualReturn(GSPC)

#Set highestclose to 1 to set start the loop
highestclose<-1

#Create new column named Highest and set it to highestclose
GSPC$GSPC.Highest<-highestclose

#Add the moving average
#GSPC$MA5 <-SMA(GSPC$GSPC.Close, n= 520)
#Gives the average annual return for the time period.
mean(na.omit(GSPC$AR))
#mean(na.omit(GSPC$WR))

#Figures out the amount of years  between the two values
daydiff <- as.Date(Todate)-as.Date(Fromdate)
numdays <- as.numeric(daydiff)
numyears <- round(numdays/365)
startyear <- as.numeric(format(as.Date(Fromdate), "%Y"))

#figure out the CAGR of the overall return. CAGR(Future value/Present value)^(1/number of years)
#It subtracts the close from the last day from the open of the first day
CAGRV<-NULL
CAGRV<-(last(GSPC$GSPC.Close)/first(GSPC$GSPC.Close))^(1/numyears)

#This is to create a line that shows the highest historical close up to that point
#This will take the find the highest value in GSPC$Close and print it to the GSPC$Highest column.
#for (row in 1:nrow(GSPC)) {
#  price <- GSPC[row, "Close"]

#    if(price > highestclose) {
#    highestclose <- price
    
#  }
#  GSPC[row,"Highest"]<-highestclose
#}

# recreate above loop using the quantmod data
#This will take the find the highest value in GSPC$Close and print it to the GSPC$Highest column.
for (row in 1:nrow(GSPC)) {
  price <- coredata(GSPC[row, 4])
  
  if(price > highestclose) {
    highestclose <- price
    
  }
  GSPC[row,7]<-highestclose
}

# This creates a data frame that gives a steady increase of the market since 1950.
#years<-data.frame("year"= seq(as.Date("1950/01/01"), by = "year", length.out = 75) ,"yearprice"=c(1950:2024))
#Update annual price by steady 7.860% (1/1950- 1/2021 https://dqydj.com/sp-500-return-calculator/). 1/2/1950 Close is 17.09
#updatedprice<-16.66
#for (row in 1:nrow(years)) {
#  years[row,"yearprice"]<-updatedprice   
#  updatedprice<- updatedprice *1.07697
#}



# This creates a data frame that gives a steady increase of the market based on the average of the time frame
years<-data.frame("year"= seq(as.Date(Fromdate), by = "year", length.out = numyears+1) ,"yearprice"=c(startyear:(startyear+numyears)))

#Update annual price by steady 7.860% (1/1950- 1/2021 https://dqydj.com/sp-500-return-calculator/). 1/2/1950 Close is 17.09
updatedprice<-drop(coredata(GSPC[1,4]))
for (row in 1:nrow(years)) {
  years[row,"yearprice"]<-updatedprice   
  #updatedprice<- updatedprice * (1+mean(na.omit(GSPC$AR)))
  updatedprice<- updatedprice * CAGRV
}


#Converts to something ggplot2 can work with 
#GSPCdf<-as.data.frame(GSPC)
GSPClong<-tidy(GSPC)
GSPCt<-spread(GSPClong,series, value)

# Presents the plot of weekly stock market close, highest historical return and average return. Requires packages:dplyr, ggplot2, scales
library(scales)
library(ggplot2)
library(dplyr) 
#Old version
#ggplot()  +
  #geom_line(data = GSPC,aes(y=GSPC.Highest,x=Date), size = 1)+
#  geom_line(data = GSPC,aes(y=Close,x=Date), color = "grey")+
#  geom_line(data = years,aes (y=yearprice, x=year), color = "blue")+
#  scale_x_date(date_minor_breaks = "1 year", date_breaks = "3 years", labels = date_format("%Y"))+ scale_y_continuous(n.breaks = 20)

#New Version using data frame converted from the xts version of GSPC. I'm having problem using the rowname for the new dataframe.  
#The date was converted to a rowname.
ggplot()  +
  #geom_line(data = GSPC,aes(y=GSPC.Highest,x=Date), size = 1)+
  geom_line(data = GSPCt,aes(y=GSPC.Close,x=index), color = "grey")+
  geom_line(data = years,aes (y=yearprice, x=year), color = "blue")+
  #geom_line(data = GSPCt,aes (y=MA5, x=index), color = "green")+
  scale_x_date(date_minor_breaks = "1 year", date_breaks = "3 years", labels = date_format("%Y"))+ scale_y_continuous(n.breaks = 20)

