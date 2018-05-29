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
  ## Q2 - Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")  ----
  ## fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
  
  result <- d2 %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(total = sum(Emissions))
  
  plot(result$year, result$total, type="l", main = "Total PM2.5 in Baltimore City across Years",
       xlab = "Years", ylab="Total Emission PM2.5 (ton)")
  points(result$year, result$total, pch=19, col = "red")
  
  dev.copy(png, "data/plot2.png")
  dev.off()
}
