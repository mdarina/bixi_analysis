#download the files from my GitHub repository: https://github.com/mdarina/bixi_analysis 
#place the files into a folder on your computer. It will be your working directory

setwd("~/Concordia/SASUniversityEdition/sas_code/portfolio/portfolio_sets") #change the directory


#If you don't have packages, then delete the hashtags to install them first.
#install.packages("scales","ggplot2","rgdal")


library(scales)
library(ggplot2)
library(rgdal)


#Printing quick statistics
statistics <- read.csv("quick_stat.csv")
as.character(statistics$rides)
#distance in km (imputed)
as.character(statistics$distance_km)
#2-minute rides were discarded
as.character(statistics$duration_hr)


#Importing a file with member pass purchases
mem_df <- read.csv("mem_df.csv")

#plotting member and non-member purchases
ggplot(mem_df, aes(category, total, fill=category))+ 
  geom_bar(stat = "identity", position = 'dodge')+
  scale_fill_manual(values=c("lavenderblush3","cornflowerblue"))+
  geom_text(aes(label=comma(total)), position=position_dodge(width=0.9), vjust=-0.25)+
  ylim(0,2510000)+
  theme(legend.position="top")+
  ggtitle("Purchases of Bixi Passes: 2014-2019")+
  xlab(" ")+ylab("Total rides")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())


#importing the member revenues set
members_tot_rev <- read.csv("member_revenues.csv")

#plotting member vs non-member revenues
ggplot(members_tot_rev, aes(category, revenues, fill=category))+ 
  geom_bar(stat = "identity", position = 'dodge')+
  scale_fill_manual(values=c("lavenderblush3","cornflowerblue"))+
  ylim(0,18000)+
  geom_text(aes(label=dollar(revenues)), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(legend.position="top")+
  ggtitle("Revenues from Members vs Non: 2014-2019")+
  xlab(" ")+ylab("Revenues ($), '000")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())



#Importing the set with mean values
mean <- read.csv("mean.csv")

# Plot member mean duration
ggplot(mean, aes(member, mean, group=1))+
  geom_line(color="navy")+
  geom_point(stat="identity",  size=3, shape=15, aes(colour=member))+
  ylim(0,22)+
  scale_colour_manual(values=c("darkgrey","cornflowerblue"))+
  geom_text(aes(label=paste(label,"(", member,")")), position=position_dodge(width=0.9), vjust=-0.8)+
  ggtitle("Mean Ride Duration by Membership: 2014-2019")+
  xlab(" ")+ylab("Minutes")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())



#Importing the rides accummulated by hour
hours_allm <- read.csv("hours_members.csv")

# Plot member mean duration
#Plot member and non-members hours
ggplot(hours_allm, aes(Hour, Rides, fill=Category))+ 
  geom_bar(stat = "identity", position = 'dodge')+
  scale_fill_manual(values=c("darkgrey","cornflowerblue"))+
  theme(legend.position="top")+
  ggtitle("Member and Non-member Rides: By Hour (2014-2019)")+
  xlab("Hour")+ylab("Rides")


#Subsetting the data
member_hours <- subset(hours_allm, Member==1)
member_hours$Peak_Hour <- ifelse(member_hours$Hour %in% c(8, 16,17,18),
                                 "Peak", "Non-Peak")

#Plot only member rides by hours
ggplot(member_hours, aes(Hour, Rides/1000, fill=Peak_Hour))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("gray74","indianred1"))+
  ggtitle("Member Rides: By Hour (2014-2019)")+
  ylab("Rides, '000")+
  theme(legend.position="top")


#Subsetting the data
nonmember_hours <- subset(hours_allm, Member==0)
nonmember_hours$Peak_Hour <- ifelse(nonmember_hours$Hour %in% c(13:18),
                                    "Peak", "Non-Peak")

#Plot only member rides by hours
ggplot(nonmember_hours, aes(Hour, Rides/1000, fill=Peak_Hour))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("gray74","indianred1"))+
  ggtitle("Non-Member Rides: By Hour (2014-2019)")+
  ylab("Rides, '000")+
  theme(legend.position="top")


#Importing the data aggregated on a weekday basis and subsetting it by members
week <- read.csv("week_allm.csv")
week_member <- subset(week, Member==1)

#Plot member weekday rides
ggplot(week_member, aes(x = Weekday, y=Rides/1000)) +
  geom_bar(aes(fill = Rides), stat = "identity") +
  scale_fill_gradient(low = "gray74", high = "indianred1", na.value = NA)+
  ggtitle("Member Rides: Day of the Week (2014-2019)")+
  xlab(" ")+
  ylab("Rides, '000")+
  scale_x_discrete(limit = c("Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday", "Sunday"))


#Subsetting the data aggregated by weekdays by non-members
week_nonmember <- subset(week, Member==0)

#Plotting non-member rides
ggplot(week_nonmember, aes(x = Weekday, y=Rides/1000)) +
  geom_bar(aes(fill = Rides), stat = "identity") +
  scale_fill_gradient(low = "gray74", high = "indianred1", na.value = NA)+
  ggtitle("Non-Member Rides: Day of the Week (2014-2019)")+
  xlab(" ")+
  ylab("Rides, '000")+
  scale_x_discrete(limit = c("Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday", "Sunday"))


#Importing the data accumulated on a monthly basis (excluding 2019 data)
#The reason for that is that one month of 2019 is missing
month <- read.csv("month1418.csv")

#Subsetting the data by member=1
month_member <- subset(month, Member==1)

#Plot member month rides
ggplot(month_member, aes(x = Bixi_Months, y=Rides/1000, fill=Peak_Months)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("gray74", "indianred1"))+
  ggtitle("Member Rides: Months (2014-2018)")+
  xlab(" ")+
  ylab("Rides, '000")


#Subsetting the data by member=0
month_nonmember <- subset(month, Member==0)

#Plot non-member month rides
ggplot(month_nonmember, aes(x = Bixi_Months, y=Rides/1000, fill=Peak_Months)) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values=c("gray74", "indianred1"))+
  ggtitle("Non-Member Rides: Months (2014-2018)")+
  xlab(" ")+
  ylab("Rides, '000")


#Importing the weather data
weather_allr <- read.csv("weather_allr.csv")
#Creating a new category of wet and dry days
#Dry days are when the level of precipitation = 0
weather_allr$wet <- ifelse(weather_allr$Precip>0,"Wet","Dry")

#Creating a scatter plot: Rides vs Mean Temperature
ggplot(weather_allr, aes(x=Mean_temp, y=Rides))+
  ggtitle("Total Bixi Rides vs Mean Temperature (2014-2019)")+
  xlab("Mean daily temperature (C)")+ylab("Rides")+
  geom_point(colour="steelblue4", alpha=0.5, size=3)+
  geom_smooth(method=lm, colour="black", se=FALSE)


#Creating a scatter plot: Rides vs Mean Temperature with Dry or not days
ggplot(weather_allr, aes(x=Mean_temp, 
                         y=Rides)) + 
  geom_point(data = weather_allr, aes(x = Mean_temp, y = Rides, 
                                      color = wet),alpha=0.5, size=3) +
  scale_color_manual(values = c("Dry" = "black", "Wet" = "cornflowerblue"))+
  geom_smooth(method=lm, colour="navy", se=FALSE)+
  ggtitle("Rides vs Mean Temperature and Precipitation (2014-2019)")


#Reading the shape file
#Go here to download the shape files (not only one, you'll need all of them!):
#http://donnees.ville.montreal.qc.ca/dataset/polygones-arrondissements
mySHP <- readOGR("LIMADMIN.shp")
dfSHP <- fortify(mySHP)

#Importing members' most popular routes in 2019
members_top10_routes<- read.csv("members_top10_routes.csv")

#Plotting top 10 most popular routes in Montreal (2019) among members
ggplot()+geom_polygon(data=dfSHP, aes(x=long,y=lat, 
                                      group=group), color="black", fill="lightgrey")+
  geom_point(data=members_top10_routes, aes(x=long, y=lat, color=station,size=rides), 
             alpha=0.5)+
  scale_color_manual(values = c("start" = "indianred2", "end" = "cornflowerblue"))+
  geom_line(data=members_top10_routes, aes(x=long, y=lat, group=group),
            color="navy")+
  labs(title = "Top Member Routes: 2019", 
       x="Longitude", y="Latitude")+
  coord_map(xlim = c(-73.59, -73.49), ylim=c(45.475,45.56))+
  scale_size(range=c(.3,5), name="Rides")+
  geom_text(data=members_top10_routes, 
            aes(x=long, y=lat, 
                label=as.character(name),
                hjust=0, vjust=0), size=2.5)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())


#Importing non-members' most popular routes in 2019
nonmembers_top10_routes<- read.csv("nonmembers_top10_routes.csv")

#Plotting top 10 most popular routes in Montreal (2019) among non-members
ggplot()+geom_polygon(data=dfSHP, aes(x=long,y=lat, 
                                      group=group), color="black", fill="lightgrey")+
  geom_point(data=nonmembers_top10_routes, aes(x=long, y=lat, color=station, size=rides), 
             alpha=0.5)+
  scale_color_manual(values = c("start" = "indianred2", "end" = "cornflowerblue"))+
  geom_line(data=nonmembers_top10_routes, aes(x=long, y=lat, group=group),
            color="navy")+
  labs(title = "Top Non-Member Routes: 2019", 
       x="Longitude", y="Latitude")+
  coord_map(xlim = c(-73.59, -73.49), ylim=c(45.475,45.56))+
  scale_size(range=c(.3,5), name="Rides")+
  geom_text(data=nonmembers_top10_routes, 
            aes(x=long, y=lat, 
                label=as.character(name),
                hjust=0, vjust=0), size=2.5)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())


#The Appendices section
#How are rides correlated with the temperature?
cor.test(weather_allr$Mean_temp,weather_allr$Rides, method="pearson")

#How are rides correlated with precipitation?
cor.test(weather_allr$Precip,weather_allr$Rides, method="pearson")

#Plot Rides vs Precipitation
ggplot(weather_allr, aes(x=Precip, 
                         y=Rides))+
  ggtitle("Bixi Rides vs Precipitation (2014-2019)")+
  xlab("Precipitation (mm)")+ylab("Rides")+
  geom_point(colour="steelblue4", alpha=0.5, size=3)+
  geom_smooth(method=lm, colour="black", se=FALSE)

#Plotting a subset
ggplot(subset(weather_allr, wet=="Wet"), aes(x=Mean_temp, 
                                             y=Rides)) + 
  geom_point(data = subset(weather_allr, wet=="Wet"), aes(x = Mean_temp, y = Rides, 
                                                          color = wet), alpha=0.5, size=3) +
  scale_color_manual(values = c("Dry" = "black", "Wet" = "cornflowerblue"))+
  geom_smooth(method=lm, colour="navy", se=FALSE)+
  ylim(0,45000)+xlim(-10,33)+
  ggtitle("Bixi Rides on Wet Days (2014-2019)")+
  theme(legend.position = "none")

#Plotting a subset
ggplot(subset(weather_allr, wet=="Dry"), aes(x=Mean_temp, 
                                             y=Rides)) + 
  geom_point(data = subset(weather_allr, wet=="Dry"), aes(x = Mean_temp, y = Rides, 
                                                          color = wet),alpha=0.5,
             size=3) +
  scale_color_manual(values = c("Dry" = "black", "Wet" = "cornflowerblue"))+
  geom_smooth(method=lm, colour="navy", se=FALSE)+
  ylim(0,45000)+xlim(-10,33)+
  ggtitle("Bixi Rides on Dry Days (2014-2019)")+
  theme(legend.position = "none")

