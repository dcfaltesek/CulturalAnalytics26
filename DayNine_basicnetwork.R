#load libraries
library(ggnetwork)
library(dplyr)
library(sna)
library(tidyr)
library(ggplot2)

#import some data
#examine the data
#now we must write a pivot
music_net<-performer_net %>% pivot_longer(cols=-Song)

#lets get a few columns out which have data about which artists collabed, first two in this case...
music_net<-data.frame(SOURCE=music_net$Song, TARGET = music_net$value)

#but we don't want solos, those are sad, why not sing with more people?
music_netB<-music_net %>% 
  filter(TARGET != "NA")

#the network needs to be structured as an EDGELIST
netty_net<-network(music_netB, directed = TRUE, multiple=TRUE, matrix.type = "edgelist")

#we can then start accessing various attriutes of this special object called a network; notice it is NOT stored as a tidy object
collab_list<-data.frame("Artist"=network.vertex.names(netty_net),"collabs"=degree(netty_net))

View(collab_list)

#code is a little different yere, we are doing a destructive set on this variable
set.vertex.attribute(netty_net, "collabs", degree(netty_net))

#rendered as a 
ggplot(netty_net, aes(x, y, xend = xend, yend = yend)) +
  #change the GEOMS
  geom_edges(color = "orange") +
  geom_nodes(aes(color = collabs)) +
  #and always run theme blank
  theme_blank()


write.csv(music_net, "music_net.csv", row.names = FALSE)

