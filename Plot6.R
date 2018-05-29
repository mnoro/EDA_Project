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
  ## Q6 - Compare emissions from motor vehicle sources in Baltimore City ... ----
  ## with emissions from motor vehicle sources in Los Angeles County, California (fips=="06037"). 
  ## Which city has seen greater changes over time in motor vehicle emissions?
  
  # Identifies motor vehicle l using EI.Sector
  result2 <- d1 %>%
    filter(grepl("vehicle", EI.Sector, ignore.case = TRUE)) %>%
    select(SCC, EI.Sector) %>% 
    unique()
  
  result <- d2 %>%
    right_join(result2, by="SCC") %>%
    filter(fips == "24510" | fips == "06037") %>%
    group_by(fips, year) %>%
    summarise(total=sum(Emissions))
  
  ggplot(data=result, aes(year, total)) + facet_grid(.~factor(fips)) + 
    geom_line(lwd = 1.5, color="red") + geom_point(pch=4)

  
  ggsave("data/plot6.png")

  
  ###
}