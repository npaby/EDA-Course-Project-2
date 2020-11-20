##Question #5 and #6: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

# Import dplyr and ggplot library
library("dplyr")
library("ggplot2")
# Store the data in specific variables
summarySCC             <- readRDS("./NEI_data/summarySCC_PM25.rds")
SCC                    <- readRDS("./NEI_data/Source_Classification_Code.rds")
# Merge the data by SCC
mergedData             <- merge(summarySCC,SCC, by="SCC")
# Subset the data with specific conditions
vehicle                <- grepl(pattern="vehicle",mergedData$EI.Sector,ignore.case = TRUE)
# Store the specific data with vehicle as x value from mergedData dataset
NEISCCVehicle          <- mergedData[vehicle,]
# Store the data specifically
baltimoreVehicles      <- NEISCCVehicle[NEISCCVehicle$fips=="24510",]
# Give it a name for labelling also
baltimoreVehicles$City <- "Baltimore City"
# Store the data specifically
losangelesVehicles     <- NEISCCVehicle[NEISCCVehicle$fips=="06037",]
# Give it a name for labelling also
losangelesVehicles$City<- "Los Angeles City"
# Merge as one data frame
vehicleData            <- rbind(baltimoreVehicles,losangelesVehicles)
# Call the graphic device for export
png("Plot 5 and 6.png")
# Plot the data to visualize
ggplot(vehicleData,aes(factor(year),round(Emissions/100,2), fill=City)) +
  geom_bar(stat="identity") +
  facet_grid(.~City) +
  xlab("Year") + 
  ylab("Total PM2.5 Emissiosn") + 
  ggtitle("Vehicle Sources Emission from 1999 to 2008")
# Turn off the device
dev.off()
