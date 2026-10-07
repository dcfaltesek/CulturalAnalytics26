#advanced ggplot2
library(ggplot2)
library(dplyr)
library(nycflights23)

#this is an idea for next tuesday - but lets start with it here - 
#continuous variables measures, discrete are categories

#sometimes this is really easy
glimpse(flights)

#various numbers make sense as they are simply numbers

#tail num and flight can be more fun as they are utterly random

#origin is much more like a FACTOR

#an important dimenison of what we want to do generally is to create new variables
flights %>% 
  group_by(origin) %>% 
  summarize(mean(dep_delay, na.rm=TRUE))

#what does na.rm mean? Lets try without it?
flights %>% 
  group_by(origin) %>% 
  summarize(mean(dep_delay))

#do you think it was fixing the result? 

#lets calculate a quick MEAN
mean(flights$dep_delay, na.rm = TRUE)

#You know this dataset well
glimpse(weddings25)

#which are continuous which are discrete?
#group by function
weddings25 %>% group_by(State) %>% summarize(mean(Budget))
weddings25 %>% group_by(State) %>% count()

#now lets hit some plots
#one continuous
ggplot(weddings25, aes(Budget))+geom_freqpoly()
ggplot(weddings25, aes(Budget))+geom_density()
ggplot(weddings25, aes(Budget))+geom_histogram()
ggplot(weddings25, aes(Budget))+geom_area(stat="bin")

#dual continuous
ggplot(weddings25, aes(Dress, Budget))+geom_point()
ggplot(weddings25, aes(Dress, Experience))+geom_smooth()

#one discrete one continuous
ggplot(weddings25, aes(State, Budget))+geom_boxplot()+guides(x = guide_axis(angle = 45))

#dual discrete
ggplot(weddings25, aes(Result, ExpGiven))+geom_count()

#continuous bivar
ggplot(weddings25, aes(Budget, Experience))+geom_density2d()

#faceting
ggplot(weddings25, aes(Dress, Venue, colour=Food))+geom_jitter()

ggplot(weddings25, aes(Dress, Venue, colour=Food))+geom_jitter()+facet_grid(~Result)

#before we start the analysis lets add a new column that should show who wins and loses, lets call the dataset weddings25B
weddings25B<-weddings25 |> 
  mutate("Total"=Experience+Food+Dress+Venue)

#lets get visual proof that we did this right
weddings25B |> 
  ggplot(aes(Total, Result, colour=Result))+geom_jitter()+scale_color_distiller(palette = "Oranges")

#lets hit a few quic statistical tests
analysis<-aov(Total ~ Dress * Venue * Food, data=weddings25B)

plot(analysis)
summary(analysis)

#there are many things we can control...
ggplot(weddings25B, aes(Food, Budget, colour=Total, size=Guests))+geom_jitter()+scale_color_distiller(palette = "Oranges")


#structured, semi-structured, unstructured

#what kind of data is this?
weddings25$Name

#what about this
weddings25$Budget

#and these?
weddings25$ExpGiven

#a fun color scheme


#lets consider this plot
ggplot(weddings25, aes(ExpGiven, ExpRank, colour=Result))+geom_jitter() + scale_color_viridis_c()

#this is too chunky, the category levels are making it hard for us to see the relationships
#dplyr gives us a lot of tools for synthesizing new data

#lets get some raw data here
weddings25C<-weddings25B %>% mutate("others" = C1+C2+C3+C4)

#so lets make that again with a continuous space
ggplot(weddings25C, aes(Experience, others, colour=Result))+geom_jitter() +scale_color_viridis_c()

#it almost feels like we need some new categorical variables

#is lets play with a factor
ggplot(weddings25C, aes(Experience, others, colour=as.factor(Result), shape=as.factor(Bride)))+geom_jitter() + scale_color_viridis_d()

#so it doesnt look like it is a show ordering function 

#lets try some mathy math
cor.test(weddings25C$Experience, weddings25C$others)
cor.test(weddings25C$ExpGiven, weddings25C$ExpRank)

#do we think the time space of the show is continuous? 
library(lubridate)
#fantastic library for easy date handling, restaurant menu on this soon...

#you tell it what you want, and within that, how the date is formatted
month(mdy(weddings25C$Date))

#BEST PRACTICE
weddings25D<-weddings25C %>% mutate("GoodDate"=mdy(Date))

#notice what GoodDate is in w25b? it is a class called DATE
#suddenly we can play with discrete time domains

ggplot(weddings25D, aes(Experience, Result, colour=as.factor(month(GoodDate))))+
  geom_jitter()+scale_color_brewer("Set 3")

#lets get some data here
weddings25D %>% 
  group_by(month(GoodDate)) %>% 
  count()


#what if season matters too
weddings25D %>% 
  group_by(month(GoodDate), Season) %>% 
  count() %>% 
  View()

#or to do that visually

weddings25D %>% 
  group_by(GD=month(GoodDate), Season) %>% 
  count() %>% 
  ggplot(aes(Season, GD, colour=n))+geom_jitter()+scale_color_viridis_c()

#so one mean way to do this is OLD vs YOUNG brides, lets assume the air dates tell us something
#this looks really scary but I promise it isnt

#store it as w25C, starting with w25b
weddings25E<-weddings25D %>% 
  #make a new column called naturalseason, where if the month is Jan or Feb, its called winter
  mutate(naturalseason = if_else(month(GoodDate)<3 & month(GoodDate)>0, 
                                 #now NEST another if_else if false, the NEXT logical test, if it is Nov or Dec, also winter
                                 "winter", if_else(month(GoodDate)>10, "winter", 
                                                   #if it is october or september it is fall
                                                   if_else(month(GoodDate)<11 & month(GoodDate)>8, "fall",
                                                           #if it is august, july, or june its summer
                                                           if_else(month(GoodDate)<9 & month(GoodDate)>5, "summer",
                                                                   #all other months are spring and 
                                                                   #then we close FOUR nested if_else and a mutate, so FIVE BIG PARENS
                                                                   "spring")))))    

#based on your new synthetic discrete, you can do summary vars                                                                                                                                                    "spring")))))
weddings25E %>% 
  group_by(naturalseason) %>% 
  summarize(mean(Experience), sd(Experience))


#lets look at our new CONTINOUS result space
ggplot(weddings25E, aes(Age, Total, colour=ExpGiven))+geom_jitter()+scale_color_distiller(palette = "Purples")
