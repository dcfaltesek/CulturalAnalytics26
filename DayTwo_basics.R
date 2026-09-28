#this is where you start your file, put some common libraries here
library(dplyr)
library(ggplot2)
library(nycflights23)

#lets keep track of a few things
#NYCflights is a package which contains DATASETS

#there are a few ways to look at data, you can start by typing in the name of the dataset and running it
flights

#that is way more than I can just quick have a look at, what about some summary stats
#first ten rows
head(flights)

#columns as rows
glimpse(flights)

#how many rows and columns
dim(flights)

#you can also select from the table using base R and the [col, row]
flights[210344,2]

#if you know where the data you want are, this is a pretty clean way to get to those cells
flights[300000:300004, 1:9]

#if you want to SEE this in our environment, go ahead and use a happy storage arrow
flights<-flights

#last but not least, what if we just want to see the data in a little window like excel?
View(flights)

#lets say you don't know where the data are and you don't want to click in that window
#filter is our friend here

#filter is a function included in dplyr, the argument on the RIGHT is a dataset name, the LEFT is what you want to find
filter(flights, carrier=="DL")

# you can STACK these in the same function 
filter(flights, carrier=="DL" & origin=="EWR")

#compare this to DELTA flights originating everywhere else in New York
#this is where our operator friends are very handy, in this case not equal !=
filter(flights, carrier=="DL" & origin!="EWR")

#why the difference?

#lets see when things get really bad in new york, big departure delays
arrange(flights, desc(dep_delay))

#or you can write that as
flights %>% arrange(desc(dep_delay))

#its going to take FOREVER to read through half a million lines, can't we use our visual processing brains?
#dataset is LEFT, aes(x,y,colour) is right, marks are tacked on with a +
ggplot(flights, aes(time_hour, dep_delay, colour=origin))+geom_point()

#lets tell some data stories here...

#those really huge numbers seem like they are weird outliers
filter(flights, dep_delay < 500)

#more specific 
filter(flights, dep_delay < 100)

#how about an hour
filter(flights, dep_delay < 10)

#so what percent is that of all flights

#now for our fun, we could just look at very delayed flights
#lets get all flights delayed for more than two hours and learn about those
#run it for a proof of concept
filter(flights, dep_delay > 120)

#run it again with a happy little storage arrow
delayed<-filter(flights, dep_delay > 120)

#very intere
ggplot(delayed, aes(time_hour, dep_delay, colour=arr_delay))+geom_point()

#here is another fun one - let's start thinking about the x and y here
ggplot(delayed, aes(origin, dep_delay))+geom_boxplot()

#lets do a few races...

