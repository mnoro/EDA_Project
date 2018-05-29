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
  ## Q3 - Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, ----
  ## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
  result <- d2 %>%
    filter(fips == "24510") %>%
    transform(type = factor(type)) %>%
    group_by(year, type) %>%
    summarise(total = sum(Emissions))
  
  ggplot(data = result, aes(x=year, y= total)) +geom_point(pch=21)+ 
    facet_grid(.~type) + geom_line(col = "red") + xlab("Years") +ylab("Total PM2.5 Emission (ton)") + 
    ggtitle("Total PM2.5 Emission in Baltimore City per Type across Years")
  
  ggsave("data/plot3.png")

  ## 
}
