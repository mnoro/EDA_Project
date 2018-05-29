#
# Assignment for Exploratory Data Analysis
#
#
## 00. Libraries   ----
library(dplyr)
library(tidyr)
library(ggplot2)



## 00. Import data if it exists  ----
if(file.exists("data/Source_Classification_Code.rds")){
  d1 <- readRDS(file="data/Source_Classification_Code.rds")
}

if(file.exists("data/summarySCC_PM25.rds")){
  d2 <- readRDS(file="data/summarySCC_PM25.rds")
}

if(exists("d1") && exists("d2"))
{
  ##
  
  ## Q1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?   ----
  ## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
  ## for each of the years 1999, 2002, 2005, and 2008.
  
  result <- d2 %>%
    group_by(year) %>%
    summarise(total = sum(Emissions))
  
  plot(result$year, result$total, type="l", main = "Total PM2.5 in the US across Years",
       xlab = "Years", ylab="Total Emission PM2.5 (ton)")
  points(result$year, result$total, pch=19, col = "red")
  
  dev.copy(png, "data/plot1.png")
  dev.off()
}
