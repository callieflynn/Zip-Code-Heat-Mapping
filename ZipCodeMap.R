#the following packes must be installed in order to run this code
library(ggmap)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgdal)

##Nice tutorial: http://www.r-bloggers.com/using-r-working-with-geospatial-data-and-ggplot2/ 
## or http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

# set working directory to source file's location
setwd("/Users/Callie/Desktop/ZipCode Mapping")

#load the state's zip code database
zipdf <- readRDS("SAZips.rds")

####basic plot of things####
#The following plot is merely a plot of the zip codes in the state with a dark grey border 
#and a regular grey fill
#x = long, y = lat, and group = group will always be inputs to the aes. The group specification
#tells r how to combine the lat long points to make a logical map. Otherwise, r will try to plot all
#points with a single line connecting them. coord_equal forces an equal aspect ratio between x and y.
#If you don't use this, your maps will look squishy. The rest of code just deals in aesthetics.

##Get the zipcode labels for the geom_text labeling
names <- aggregate(cbind(long, lat) ~ ZIP, data=zipdf, FUN=mean)

##FYI missing 78234(Fort Sam?), 78236 (Lackland AFB), and part of 78227
ggplot(data=zipdf) + geom_polygon(aes(long,lat, group = group), colour = "dark grey", fill = "grey") + coord_equal() + labs(x="", y="", title="San Antonio Zip Codes")+ #labels
  geom_text(data=names, aes(long, lat, label = ZIP), size=3) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #remove gridlines

#### choropleth of things ####
uniqueCodes <- unique(zipdf$ZIP) #curious about included zip codes
##read in ACA numbers
ACA <- read.csv("ACA.csv", header=TRUE, sep=",")

#do the merging of fake data onto the zip code points
choroDF <- merge(zipdf, ACA, by.x = "ZIP", by.y = "Zip", all.x = TRUE)

#the merge function sorts the dataframe by the merge variable... which makes the plots funky
#to make them unfunky, sort by group and plot order
choroDF <- arrange(choroDF, group, order)

#Now we can plot things: 
##*********************##
##FYI SKIP THIS PARAGRAPH FOR LABELED DATA
##*********************##

#We want the fill to be dynamic based on the values of enrollment. This is done by 1) specifying
#"fill = number" in the aesthetic and 2) telling the machine what color progression we
#would like. In scale_fill_continuous, there are arguments for the range of the scale, the 
#colours for each of the bounds, and a colour designated for any "NA" values in your data.
ggplot(data=choroDF) + geom_polygon(aes(long,lat, group = group, fill= Enrollment), colour="black", size = 0.001, alpha=.7) + scale_fill_continuous(limits=c(2, 4000), low = "#F88400", high= "#EE0F00", na.value = "white") + coord_equal() + labs(x="", y="", title="San Antonio 2015 Enrollment Data")+ #labels 
  geom_text(data=names, aes(long, lat, label = ZIP), size=3) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #remove gridlines

##Open a png to save the graph to
##png(filename="2015_Enrollment_SA.png", width=1000, height=1000)
pdf(file="2015_Enrollment_SA.pdf")

##Finally, map the data on top of San Antonio's roadmap for readability
gmap <- get_map(location="San Antonio", zoom = 10, maptype="roadmap", source = "google")
gg <- ggmap(gmap)
gg + geom_polygon(data=choroDF, aes(x=long,y=lat, group = group, fill= Enrollment), color="gray20", line="black", size = .3, alpha= 1 #transparency
                  ) + scale_fill_continuous(limits=c(2, 4000), low = "#F88400", high= "#EE0F00", na.value = "white") + coord_equal() + labs(x="", y="", title="San Antonio ACA Enrollment 2015")+ #labels 
  geom_text(data=names, aes(long, lat, label = ZIP), cex=1.5, color="white") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
                                                                     axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
                                                                     plot.title = element_text(lineheight=.8, face="bold", vjust=1), # make title bold and add space
                                                                     panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #remove gridlines

##Close the png
dev.off()
