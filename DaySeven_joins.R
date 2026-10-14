#day seven joins
#lets get a familar dataset
library(nycflights23)
flights<-flights

#fill in the list
nycflights23::airlines
weather<-nycflights23::weather
nycflights23
nycflights23


#lets try to add the airline names...
inner_join(flights, airlines, by="carrier")

#but what else do we have...
#weather...
flights %>% group_by(yday(date(time_hour))) %>% summarize(n=mean(dep_delay, na.rm=TRUE)) %>% arrange(desc(n))

#so when is that?
glimpse(weather)

#lets filter this down
A<-flights %>% 
  filter(month ==5 & day ==10)


B<-weather %>% 
  filter(month ==5 & day ==10)

C<-left_join(A,B, by="origin")

View(C)

ggplot(C, aes(dep_delay, wind_gust))+geom_jitter()

#don't try to join everything - its just too much data

ggplot(weather, aes(wind_gust, visib, colour=precip))+geom_jitter()+facet_wrap(~month)

#so when was visibility worst?
weather %>% group_by(month) %>% summarize(mean(visib, na.rm=TRUE), sd(visib, na.rm=TRUE))
flights %>% group_by(month) %>% summarize(mean(dep_delay, na.rm=TRUE))




#now lets play with a new DATASET about TV
#import the TV data

#lets make a BASIC PLOT


#lets cook up a quick dataset
Year<-c(1988, 1989, 1990, 1991, 1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)
President<-c("Regan", "Bush", "Bush","Bush","Bush", "Clinton","Clinton","Clinton","Clinton","Clinton","Clinton","Clinton","Clinton","W Bush","W Bush","W Bush","W Bush","W Bush","W Bush","W Bush","W Bush","Obama")

#of course we have a different dataset
presidential

#make a dataframe
TVprez<-data.frame(Year,President)

#now we compare our datasets
#same column name? same data type?

#make corrections...

#lets have some fun with an inner_join
#can we add on the PRESIDENT data to the TV data

