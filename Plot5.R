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
  
  ## Q5 - How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?  ----
  # Identifies motor vehicle l using EI.Sector
  result2 <- d1 %>%
    filter(grepl("vehicle", EI.Sector, ignore.case = TRUE)) %>%
    select(SCC, EI.Sector) %>% 
    unique()
  
  result <- d2 %>%
    right_join(result2, by="SCC") %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(total=sum(Emissions)) 
  
  ggplot(data = result, aes(x=year, y = total)) + geom_line() + geom_point() +
    ggtitle("Emission from Motor Vehicle Sources in Baltimore City") +
    ylab("Total PM2.5 Emissions (ton)")
  
  ggsave("data/plot5.png")
  
}
