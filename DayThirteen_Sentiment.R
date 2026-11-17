#your libraries for todays class
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(textdata)
library(ggrepel)

#go ahead and find your taylor_songs data, get that into memory
#import the file called six, this is designed for training a tensor
#now lets filter for our first artist, taylor swift
#for our research lets use mostswift
#that dataset is on CANVAS
#it also has baseline data from other artists
#like who?

taylor_songs<-mostswift %>% filter(Taylor.Swift==1)

#our first step is to UNNEST the tokens
taylor_songs_counted <- taylor_songs %>%
  #for the homework this is a really important line
  #the first argument being passed is the level of chunking you want
  #the second is the column where the text is
  unnest_tokens(word, song_lyric) %>%
  #our data has an ID number for each song called "sid_id"
  #you may want to add a column like this to YOUR corpus
  count(six_id, word, sort = TRUE)%>%
  rename(per_line = n)

#get scores
afinn<-get_sentiments("afinn")
#if there is a prompt, say yes

#for today we are NOT removing stopwords
#the dimensionality reduction of the AFINN join makes this redundant
#attach the scores
with_scores<-taylor_songs_counted%>%
  #THIS IS THE INNERJOIN I WAS YELLING ABOUT!
  inner_join(afinn, by="word") 


#create a line score
scores_per_song<-with_scores%>%
  group_by(six_id)%>%
  #notice our per line strategy is SUM
  summarize(line_value=sum(value), line_var=sd(value))


#attach your sentiments
taylor_sentiments<-inner_join(taylor_songs, scores_per_song, by="six_id")

#visualize the albums
albums<-taylor_sentiments%>%
  group_by(track_name)%>%
  #adds up the scores for each line, adds the track number, and then album name
  summarize(song_value=sum(line_value), song_sd=mean(line_var))%>%
  ungroup()
#dont worry about the warning

#now lets get a quick set of album names
album_names<-data.frame(track_name=taylor_sentiments$track_name, album_name=taylor_sentiments$album_name)

#add track names
albums<-inner_join(albums, album_names, by="track_name")

View(albums)
#X is track number, Y is song value, color is album, this repel code makes it so the labels don't overlap when zoomed out
ggplot(albums, aes(track_name, song_value, colour=album_name))+
  geom_label_repel(aes(label=track_name))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))


#to view the data 
View(albums)

#is this a reasonable method?
#what if we want to see the internals of the calculation
View(with_scores)

#now that we are under the hood
with_scores %>% 
  ggplot(aes(per_line, value))+geom_point()

#so lets go back to the other data with a simple plot
ggplot(albums, aes(song_value, song_sd, colour=album_name))+geom_point()

#lets get a sense of the relationship
cor.test(albums$song_value, albums$song_sd)

#WHAT DOES THAT RESULT MEAN?
#WHAT DOES THAT MEAN ABOUT HAPPY TAY?

#more advanced tay example....
