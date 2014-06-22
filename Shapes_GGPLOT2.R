#The following code maps energy fluctuations by Chicago Community area using the 'rdgal' library to handle shape files and GGPlot2 to display results
#Source data and shape files available from https://data.cityofchicago.org/

library(maps)
library(rgdal) #this lets you handle shape files with readOGR()
library(maptools)
library(gpclib) 
gpclibPermit() #needs to be true - load maptools before gpclib
library(ggplot2)
library(plyr)
library(RColorBrewer)

energy = read.csv("/Users/kylehundman/Desktop/census.csv", header=T,na.strings=c("","*","NA"))

#Importing Shape files and Plotting with ggplot
commAreas = readOGR("/Users/kylehundman/Desktop/accenture/communityAreas", layer = "CommAreas") 
#Above: first is unzipped zipped folder with all shape files in it (available from City of Chicago data site),
#second is name of files inside without extension
commAreas@data$id = rownames(commAreas@data) 
label_points = as.data.frame(coordinates(commAreas))
commAreas.points = fortify(commAreas, region="id") 
commAreas_df = join(commAreas.points, commAreas@data, by="id")

#Convert to uppercase to ensure uniformity
energy$COMMUNITY_AREA_NAME = toupper(energy$COMMUNITY_AREA_NAME)

#Join data to be displayed with shape file data
library(sqldf)
commAreas.df = sqldf('SELECT * 
                     FROM commAreas_df as A INNER JOIN energy as B
                     WHERE A.COMMUNITY = B.COMMUNITY_AREA_NAME')

commAreas.df= rename(commAreas.df, c("Total_Census_Blocks"="Unstable_Census_Blocks"))

#Manual computation of census block centers (for labeling)
centers = sqldf('SELECT COMMUNITY, (avg(lat)) as lat, (avg(long)) as long
                FROM "commAreas.df"
                WHERE Total_Census_Blocks > 62
                GROUP BY COMMUNITY')

avg = sqldf('SELECT avg(Total_Census_Blocks) 
            FROM "commAreas.df"')

#Plot results
ggplot(commAreas.df) +
  aes(long,lat,group=COMMUNITY, fill=Unstable_Census_Blocks) + #handles aesthetics
  geom_polygon() + #tells ggplot how to plot your data
  geom_path(color="black") + #line color
  annotate("text", x = centers$long, y = centers$lat, label = centers$COMMUNITY, size = 3.5) +
  coord_equal() + #make x and y scales equal
  scale_fill_gradient2(low ="light blue", high="red", midpoint = 60) + #assign colors to "fill = CLASS" in aes()
  theme(line = element_blank(),
        line = element_blank(),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.background = theme_blank(),
        axis.ticks=element_blank())+
  xlab("") +
  ylab("") +
  ggtitle(expression(atop("Chicago Community Areas",
                          atop(italic("High Energy Fluctuations - Residential 2010"), ""))))
