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

if(exists("d1") & exists("d2"))
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

dev.copy(png, "data/plot3.png")
dev.off()
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
  
dev.copy(png, "data/plot4.png")
dev.off()  
  


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

dev.copy(png, "data/plot5.png")
dev.off()

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
ggplot(data=result, aes(year, total)) + geom_line() + geom_point(data=result, shape = fips)

dev.copy(png, "data/plot6.png")
dev.off()

###
}
