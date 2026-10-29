library(rvest)
library(ggrepel)
library(ggplot2)
library(dplyr)

A<-football_data |> filter(completed==TRUE)

#lets write this bad boy to get us a datastructure where the SOURCE is the team donating the W to the TARGET
homewin<-A |> filter(home_points>away_points)
awaywin<-A |> filter(home_points<away_points)

#check to see variables
dim(homewin)
dim(awaywin)

#build basic datastructure
B<-data.frame(SOURCE=homewin$away_team, TARGET=homewin$home_team)
C<-data.frame(SOURCE=awaywin$home_team, TARGET=awaywin$away_team)

#as edge list
#combine the nicely organized B and C
feetball<-bind_rows(B,C)
View(feetball)

#for your use in GEPHI go ahead ane export now
write.csv(feetball, "feetball.csv", row.names = FALSE)

#now we have a clear and elegant edgelist of all of college football

#step one: make a network object

feet_net<-network(feetball, directed = TRUE, multiple=FALSE, matrix.type = "edgelist")

#step two: measure the network
set.vertex.attribute(feet_net, "wins", degree(feet_net, cmode="indegree"))
set.vertex.attribute(feet_net, "between", betweenness(feet_net))
set.vertex.attribute(feet_net, "closeness", closeness(feet_net))
set.vertex.attribute(feet_net, "eigenvector", evcent(feet_net))
set.vertex.attribute(feet_net, "power", bonpow(feet_net, exponent = 1))

#step 3: collect all the measurements
#closeness can't be calculated until all teams have a loss in the directed graph; you can switch to undirected if you want this measurement
Q<-get.vertex.attribute(feet_net, "closeness")
V<-get.vertex.attribute(feet_net, "between")
R<-get.vertex.attribute(feet_net, "wins")
U<-get.vertex.attribute(feet_net, "eigenvector")
S<-get.vertex.attribute(feet_net, "vertex.names")
X<-get.vertex.attribute(feet_net, "power")

#step four: combine all that data into a lovely graph
W<-data.frame(Team=S, Closeness=Q,Between=V,Eigen=U,Bonacic_Power=X,Wins=R)
View(W)

#a quick computational solution
WW<-W |>
  #to be in our analysis you need four wins and a bonacic power which suggests some power
  filter(Wins>4 & Bonacic_Power>-.1) |> 
#and arrange for betweenness
  arrange(desc(Between)) |> 
  mutate(OurRank=row_number())

View(WW)

#whoops - lets create a tie at 1
#rows 43 through 45, column 7 should say 1
WW[43:45,7]<-1

View(WW)

#step five render the graph
C<-ggnetwork(feet_net, layout = "springrepulse")

#step six make the graph
#line one aes NEVER CHANGES, EVER!
ggplot(C, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "pink") +
  geom_nodes(aes(size = wins, color = OurRank)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank()+
  theme(legend.position = "bottom")


#step seven - compare to the current T 25
html<-"https://sports.yahoo.com/college-football/rankings/"
css<-"#rankings-table"

#scrape these lovelys
current_ranks<-html |> 
  read_html() |> 
  html_elements(css=css) |> 
  html_table()

#it scrapes as list, so fix that
current_ranks<-data.frame(current_ranks)

#this will get you all but 5
inner_join(WW, current_ranks, by="Team")  
  
#lets make some corrections; this is destructive base R so be careful
current_ranks[1,2]<-"Miami"
current_ranks[2,2]<-"Ohio State"
current_ranks[12,2]<-"Ole Miss"
current_ranks[21,2]<-"Boise State"
current_ranks[24,2]<-"Oklahoma State"

current_foot<-left_join(WW, current_ranks, by="Team")
View(current_foot)

#lets fix that column name, again base R
colnames(current_foot)[8]<-"TheirRank"

#assign values 
current_foot$TheirRank[is.na(current_foot$TheirRank)] <- 40

#step eight - graph that
current_foot |> 
  ggplot(aes(OurRank, TheirRank, colour=Between))+geom_text_repel(aes(label = Team))

#why do we disagree? There is clearly some overlap, also where are them ducks?