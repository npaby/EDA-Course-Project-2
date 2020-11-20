##Question #1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Read the Data

summarySCC <- readRDS("./NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./NEI_data/Source_Classification_Code.rds")

# Subset the data

annual_emissions <- aggregate(Emissions ~ year,summarySCC,sum)

# Export the data plotted

png("Plot 1.png")

barplot(height=annual_emissions$Emissions/1000,names.arg = annual_emissions$year,col = (annual_emissions$year)*3,xlab="Year",ylab="PM2.5 Emission",main="Annuual Emission of PM2.5")

#Turn the device off

dev.off()
