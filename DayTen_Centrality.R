library(ggnetwork)
library(dplyr)
library(sna)

#lets go through the idea with two nicely structured datasets
#greys first

#what do we notice
glimpse(greys)

#this is my greys dataset through S16

grey_net<-network(greys, directed = TRUE, multiple=TRUE, matrix.type = "edgelist")

#now we need to do some analysis and set some attributes
#this is NOT TIDY so we do modifications to the object itself, not to a DATAFRAME

#add some attributes 
set.vertex.attribute(grey_net, "games", degree(grey_net))
set.vertex.attribute(grey_net, "between", betweenness(grey_net))
set.vertex.attribute(grey_net, "prestige", prestige(grey_net, cmode="domain.proximity"))
??prestige


#get a dataframe of those
Q<-get.vertex.attribute(grey_net, "prestige")
V<-get.vertex.attribute(grey_net, "between")
R<-get.vertex.attribute(grey_net, "games")
S<-get.vertex.attribute(grey_net, "vertex.names")

W<-data.frame(S, Q,V,R)
View(W)

ggplot(grey_net, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "pink") +
  geom_nodes(aes(size = between, color = as.factor(games))) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank()+
  theme(legend.position = "bottom")



#layout experiment
n<-grey_net
#for our first by styling choice, lets take a step back to a simple graph
K<-ggnetwork(n, layout = "kamadakawai")
L<-ggnetwork(n, layout = "circrand")
M<-ggnetwork(n, layout = "geodist")
N<-ggnetwork(n, layout = "hall")
O<-ggnetwork(n, layout = "mds")
P<-ggnetwork(n, layout = "target")
Q<-ggnetwork(n, layout = "princoord")
R<-ggnetwork(n, layout = "random")
S<-ggnetwork(n, layout = "rmds")
U<-ggnetwork(n, layout = "segeo")
V<-ggnetwork(n, layout = "seham")
W<-ggnetwork(n, layout = "spring")
B<-ggnetwork(n, layout = "springrepulse")
C<-ggnetwork(n, layout = "eigen")

#replace XXXXXX with a leter from above, each of these can be controlled as well...
ggplot(K, aes(x, y, xend = xend, yend = yend)) +
  #change the GEOMS
  geom_edges(color = "orange") +
  geom_nodes(aes(color = games)) +
  #and always run theme blank
  theme_blank()

#lets try again with roads
road_net<-network(roads, directed = TRUE, multiple=TRUE, matrix.type = "edgelist")

ggplot(road_net, aes(x, y, xend = xend, yend = yend)) +
  #change the GEOMS
  geom_edges(color = "orange") +
  geom_nodes(aes(color = "red")) +
  #and always run theme blank
  theme_blank()
