crimeURL <- "http://data.denvergov.org/download/gis/crime/csv/crime.csv"
download.file(crimeURL,destfile="./data/crime.csv",method="curl")

crime <- read.csv("./data/crime.csv")

# Data Clean Up
# Convert the string date to an actual date
# Create a column to indicate the count of incedents.  Since each row is one incedent, we assign each
# value to 1 (anything * 0 + 1 = 1)
# Create a column that is the julian date of the offence date
crime$OccDate <- as.Date(as.character(crime$FIRST_OCCURRENCE_DATE),format="%Y-%m-%d")
crime$ReportCount <- crime$INCIDENT_ID*0+1
crime$OccDateJulian <- julian(crime$OccDate)

# Data Focus
# Focus in on only the vehicle and home break ins in west wash park
crimewwp <- crime[crime$NEIGHBORHOOD_ID == "washington-park-west",]
crimewwp_house <- crimewwp[crimewwp$OFFENSE_TYPE_ID == "burglary-residence-by-force" | crimewwp$OFFENSE_TYPE_ID == "burglary-residence-no-force",]
crimewwp_car <- crimewwp[crimewwp$OFFENSE_TYPE_ID == "theft-items-from-vehicle" | crimewwp$OFFENSE_TYPE_ID == "theft-of-motor-vehicle",]

# Perform analysis on the increase or decrease of both reported car and house crime
# First prep the data as an aggragate of incedents by month
crimewwp_house_bymonth <- as.data.frame(rowsum(crimewwp_house$ReportCount, format(crimewwp_house$OccDate,"%Y-%m")))
crimewwp_car_bymonth <- as.data.frame(rowsum(crimewwp_car$ReportCount, format(crimewwp_car$OccDate,"%Y-%m")))
crimewwp_house_bymonth$date <- as.Date(as.character(c("2008-01-01","2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01","2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01","2008-12-01","2009-01-01","2009-02-01","2009-03-01","2009-04-01","2009-05-01","2009-06-01","2009-07-01","2009-08-01","2009-09-01","2009-10-01","2009-11-01","2009-12-01","2010-01-01","2010-02-01","2010-03-01","2010-05-01","2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01","2010-11-01","2010-12-01","2011-01-01","2011-02-01","2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01","2011-10-01","2011-11-01","2011-12-01","2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01","2013-01-01","2013-02-01")),format="%Y-%m-%d")
crimewwp_car_bymonth$date <- as.Date(as.character(c("2008-01-01","2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01","2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01","2008-12-01","2009-01-01","2009-02-01","2009-03-01","2009-04-01","2009-05-01","2009-06-01","2009-07-01","2009-08-01","2009-09-01","2009-10-01","2009-11-01","2009-12-01","2010-01-01","2010-02-01","2010-03-01","2010-04-01","2010-05-01","2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01","2010-11-01","2010-12-01","2011-01-01","2011-02-01","2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01","2011-10-01","2011-11-01","2011-12-01","2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01","2013-01-01","2013-02-01")),format="%Y-%m-%d")

# Linear Model Analysis of the growth in break ins
lm_crimewwp_house_bymonth = lm(crimewwp_house_bymonth$V1 ~ crimewwp_house_bymonth$date)
summary(lm_crimewwp_house_bymonth)
confint(lm_crimewwp_house_bymonth,level=0.95)
lm_crimewwp_car_bymonth = lm(crimewwp_car_bymonth$V1 ~ crimewwp_car_bymonth$date)
summary(lm_crimewwp_car_bymonth)
confint(lm_crimewwp_car_bymonth,level=0.95)

# Plot the results
par(mfrow=c(2,1))
plot(crimewwp_house_bymonth$date,crimewwp_house_bymonth$V1,xlab="Month",ylab="Number of House Incedences")
lines(crimewwp_house_bymonth$date,lm_crimewwp_house_bymonth$fitted,lwd=3,col="darkgrey")
plot(crimewwp_car_bymonth$date,crimewwp_car_bymonth$V1,xlab="Month",ylab="Number of Car Incedences")
lines(crimewwp_car_bymonth$date,lm_crimewwp_car_bymonth$fitted,lwd=3,col="darkgrey")

# Perform analysis on the 'migration' of crime from Broadway toward Washington Park
# First just check out the data and see how it plots, and get berrings with known corners of the wwp area
# pulled from google maps
par(mfrow=c(1,1))
plot(crimewwp_house$GEO_LON,crimewwp_house$GEO_LAT)
points(-104.974245,39.691414,col=5)  # South East Point of WWP in light blue
points(-104.98736,39.701871,col=10)  # South West Point of WWP in red
points(-104.98706,39.710884,col=15)  # North West Point of WWP in yellow
points(-104.973713,39.710917,col=20) # North East Point of WWP in dark blue

# Prep the data into aggragate average of the longitude value per month.  The higher the value
# the closer it is to Washington Park, therefor any increase in average value over time would
# indicate a 'migration' of crime east
crimewwp_house_latbymonth <- aggregate.data.frame(crimewwp_house$GEO_LON,by=list(format(crimewwp_house$OccDate,"%Y-%m")),FUN="mean")
crimewwp_car_latbymonth <- aggregate.data.frame(crimewwp_car$GEO_LON,by=list(format(crimewwp_car$OccDate,"%Y-%m")),FUN="mean")
crimewwp_house_latbymonth$date <- as.Date(as.character(c("2008-01-01","2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01","2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01","2008-12-01","2009-01-01","2009-02-01","2009-03-01","2009-04-01","2009-05-01","2009-06-01","2009-07-01","2009-08-01","2009-09-01","2009-10-01","2009-11-01","2009-12-01","2010-01-01","2010-02-01","2010-03-01","2010-05-01","2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01","2010-11-01","2010-12-01","2011-01-01","2011-02-01","2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01","2011-10-01","2011-11-01","2011-12-01","2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01","2013-01-01","2013-02-01")),format="%Y-%m-%d")
crimewwp_car_latbymonth$date <- as.Date(as.character(c("2008-01-01","2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01","2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01","2008-12-01","2009-01-01","2009-02-01","2009-03-01","2009-04-01","2009-05-01","2009-06-01","2009-07-01","2009-08-01","2009-09-01","2009-10-01","2009-11-01","2009-12-01","2010-01-01","2010-02-01","2010-03-01","2010-04-01","2010-05-01","2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01","2010-11-01","2010-12-01","2011-01-01","2011-02-01","2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01","2011-10-01","2011-11-01","2011-12-01","2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01","2013-01-01","2013-02-01")),format="%Y-%m-%d")


# Linear Model Analysis of the growth in break ins
lm_crimewwp_house_latbymonth = lm(crimewwp_house_latbymonth$x ~ crimewwp_house_latbymonth$date)
summary(lm_crimewwp_house_latbymonth)
confint(lm_crimewwp_house_latbymonth,level=0.95)
lm_crimewwp_car_latbymonth = lm(crimewwp_car_latbymonth$x ~ crimewwp_car_latbymonth$date)
summary(lm_crimewwp_car_latbymonth)
confint(lm_crimewwp_car_latbymonth,level=0.95)

# Plot the results
par(mfrow=c(2,1))
plot(crimewwp_house_latbymonth$date,crimewwp_house_latbymonth$x,xlab="Month",ylab="Mean Longitude of House Crime")
lines(crimewwp_house_latbymonth$date,lm_crimewwp_house_latbymonth$fitted,lwd=3,col="darkgrey")
plot(crimewwp_car_latbymonth$date,crimewwp_car_latbymonth$x,xlab="Month",ylab="Mean Longitude of Car Crime")
lines(crimewwp_car_latbymonth$date,lm_crimewwp_car_latbymonth$fitted,lwd=3,col="darkgrey")

# Initial Data Investigation
# barplot(t(rowsum(crimewwp_housecar[crimewwp_housecar$OFFENSE_TYPE_ID=="burglary-residence-by-force",]$ReportCount, format(crimewwp_housecar[crimewwp_housecar$OFFENSE_TYPE_ID=="burglary-residence-by-force",]$OccDate,"%Y-%m"))),las=2)
# barplot(t(rowsum(crime_housecar[crime_housecar$OFFENSE_TYPE_ID=="burglary-residence-by-force",]$ReportCount, format(crime_housecar[crime_housecar$OFFENSE_TYPE_ID=="burglary-residence-by-force",]$OccDate,"%Y-%m"))),las=2)
# crimewwp_housecar_agg <- aggregate(crimewwp_housecar$ReportCount, by=list(crimewwp_housecar$OccDateJulian,crimewwp_housecar$OFFENSE_TYPE_ID), FUN=sum)
# plot(crimewwp_housecar_agg$Group.1,crimewwp_housecar_agg$x,col=as.factor(crimewwp_housecar_agg$Group.2))
# hist(crimewwp_housecar$GEO_X,breaks=10000,xlim=c(3144000,3148900))
# hist(crimewwp_housecar$GEO_Y,breaks=10000)
# par(mar=c(8,6,2,2))
# plot(crimewwp_housecar$NEIGHBORHOOD_ID,las=2)
# plot(jitter(crimewwp_housecar$GEO_X),jitter(crimewwp_housecar$GEO_Y),col=as.factor(crimewwp_housecar$OFFENSE_TYPE_ID))
# hist(ts(crimewwp_housecar$FIRST_OCCURRENCE_DATE, frequency=12, start=c(1987,1)))
# plot(crimewwp_housecar$OccDate)
# table(crimewwp_housecar$FIRST_OCCURRENCE_DATE)

