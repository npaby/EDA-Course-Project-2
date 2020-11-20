##Question #3: Of the four types of sources indicated by the (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 19992008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
# Import the library
library("ggplot2")
# Input the data from the rds file
summarySCC <- readRDS("./NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./NEI_data/Source_Classification_Code.rds")
# Subset the data, choose only baltimore
baltimoredata <- summarySCC[summarySCC$fips=="24510",]
# Subset the data for plotting
baltimoredata <- aggregate(Emissions ~ year + type, baltimoredata,sum)
# Export the data in png format
png("Plot 3.png")
ggplot(data=baltimoredata,aes(x=factor(year),y=Emissions,fill=type)) + geom_bar(stat="Identity") + xlab("Year") + ylab("Total Emision") + ggtitle("Total Emissions of Baltimore from 1999 to 2008")
# Turn off the device
dev.off()
