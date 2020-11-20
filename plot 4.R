##Question #4: Across the United States, how have emissions from coal combustion-related sources changed from 1999 to 2008?
# Import the dplyr and ggplot2 library
library("dplyr")
library("ggplot2")
# Store the data in specific variables
summarySCC <- readRDS("./NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./NEI_data/Source_Classification_Code.rds")
# Merge two data by SCC
mergedData <- merge(summarySCC,SCC, by="SCC")
# Subset the data with coal pattern in EI.Sector
coal <- grepl(pattern="coal",mergedData$EI.Sector,ignore.case = TRUE)
# Store the data to NEISCCoal with the same x value from the merged Data
NEISCCCoal <- mergedData[coal,]
# Subset the sata for plotting
coalData <- aggregate(Emissions ~ year, NEISCCCoal,FUN=sum)
# Call the device function for export
png("Plot 4.png")
# Make a plot of the data
ggplot(coalData,aes(factor(year),Emissions/10000,fill=year,label=round(Emissions/10000,2))) +
  geom_bar(stat="Identity") + 
  geom_label(aes(fill=year),colour="white",fontface="bold") +
  xlab("Year") + 
  ylab("Total PM2.5 Emissiosn") + 
  ggtitle("Total Emissions of Coal Sources from 1999 to 2008")
# Turn the device off
dev.off()
