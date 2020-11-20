## Question #2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland ( fips == “24510”) from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Input the data
summarySCC <- readRDS("./NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./NEI_data/Source_Classification_Code.rds")
# Select baltimore only by fips
baltimoredata <- summarySCC[summarySCC$fips=="24510",]
# Subset the data by the sum of emissions
baltimoredata <- aggregate(Emissions ~ year, baltimoredata,sum)
# Export the data in png format
png("Plot 2.png")
barplot(height=baltimoredata$Emissions/1000,names.arg = baltimoredata$year,col = (annual_emissions$year)*5.9,xlab="Year",ylab="PM2.5 Emission",main="Annuual Emission of PM2.5 of Baltimore")
# Turn the device off
dev.off()
