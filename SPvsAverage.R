#Load t
#Install package for fread() and col_date()
install.packages("data.table")
library(readr)
#Load the file into the GSPC data table. Convert Date to date format
GSPC<-fread("https://query1.finance.yahoo.com/v7/finance/download/%5EGSPC?period1=-631152000&period2=1636761600&interval=1wk&events=history&includeAdjustedClose=true", colClasses=c(Date=col_date(format = "%Y-%m-%d")))

#Set highestclose to 1 to set start the loop
highestclose<-1
#This is to create a line that shows the highest historical close up to that point
#This will take the find the highest value in GSPC$Close and print it to the GSPC$Highest column.
for (row in 1:nrow(GSPC)) {
  price <- GSPC[row, "Close"]
  
  if(price > highestclose) {
    highestclose <- price
    
  }
  GSPC[row,"Highest"]<-highestclose
}

# This creates a data frame that gives a steady increase of the market since 1950.
years<-data.frame("year"= seq(as.Date("1950/01/01"), by = "year", length.out = 73) ,"yearprice"=c(1950:2022))
#Upate annnual price by steady 7.860% (1/1950- 1/2021 https://dqydj.com/sp-500-return-calculator/). 1/2/1950 Close is 17.09
updatedprice<-16.66
for (row in 1:nrow(years)) {
  years[row,"yearprice"]<-updatedprice   
  updatedprice<- updatedprice *1.0786
}


# Presents the plot of weekly stock market close, highest historical return and average return. Requires packages:dplyr, ggplot2, scales
library(scales)
library(ggplot2)
library(dplyr) 
ggplot()  +
  geom_line(data = GSPC,aes(y=Highest,x=Date), size = 1)+
  geom_line(data = GSPC,aes(y=Close,x=Date), color = "grey")+
  geom_line(data = years,aes (y=yearprice, x=year), color = "blue")+
  scale_x_date(date_minor_breaks = "1 year", date_breaks = "3 years", labels = date_format("%Y"))+ scale_y_continuous(n.breaks = 20)
