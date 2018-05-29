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
  ## Q4 - Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008? ----
  
  # Identifies fuel combustion and coal using EI.Sector
  result2 <- d1 %>%
    filter(grepl("Comb", EI.Sector, ignore.case = TRUE) &
             grepl("Coal", EI.Sector, ignore.case = TRUE)  ) %>%
    select(SCC, EI.Sector) %>% 
    unique()
  
  result <- d2 %>%
    right_join(result2, by="SCC") %>%
    group_by(year) %>%
    summarise(total=sum(Emissions)) 
  
  ggplot(data = result, aes(year, total)) +geom_point() + geom_line() +
    ggtitle("Emission from coal combustion-related sources") +
    ylab("Total PM2.5 Emissions (ton)")
  
  ggsave("data/plot4.png")
 
  
  
}
