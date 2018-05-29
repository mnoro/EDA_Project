#
# Assignment for Exploratory Data Analysis
#
#
## 00. Import data    ----

d1 <- readRDS(file="data/Source_Classification_Code.rds")
d2 <- readRDS(file="data/summarySCC_PM25.rds")

##
## 00. Libraries   ----
library(dplyr)
library(tidyr)
library(ggplot2)


## Q1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?   ----
## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
## for each of the years 1999, 2002, 2005, and 2008.

result <- d2 %>%
  group_by(year) %>%
  summarise(total = sum(Emissions))

plot(result$year, result$total, type="l", main = "Total PM2.5 in the US across Years",
     xlab = "Years", ylab="Total Emission PM2.5 (ton)")
points(result$year, result$total, pch=19, col = "red")

## Q2 - Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")  ----
## fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

result <- d2 %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(total = sum(Emissions))

plot(result$year, result$total, type="l", main = "Total PM2.5 in Baltimore City across Years",
     xlab = "Years", ylab="Total Emission PM2.5 (ton)")
points(result$year, result$total, pch=19, col = "red")

## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, ----
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
result <- d2 %>%
  filter(fips == "24510") %>%
  transform(type = factor(type)) %>%
  group_by(year, type) %>%
  summarise(total = sum(Emissions))

ggplot(data = result, aes(x=year, y= total)) +geom_point(pch=21)+ 
  facet_grid(.~type) + geom_line(col = "red") + xlab("Years") +ylab("Total PM2.5 Emission (ton)") + 
  ggtitle("Total PM2.5 in Baltimore City per Type across Years")

