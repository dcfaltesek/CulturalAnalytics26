library(dplyr)
library(ggplot2)

#group by state
weddings25 %>% 
  group_by(State) %>% 
  summarize(mmmmm=mean(Food)) %>% 
  arrange(desc(mmmmm))

#why is Ohio so good at food - was this like one magical wedding?
weddings25 %>% 
  group_by(State) %>% 
  count()

#oh so it was like one wedding?

#so as we start getting into some of the other questions, 
#is there a relationship between experience and outcome
weddings25 %>% 
  #jitter means it adds a little randomness; it slightly corrupts the data but adds flavor
  ggplot(aes(Experience, Result, colour=Budget))+geom_jitter()

#so we have a scale problem. 
weddings25 %>% 
  ggplot(aes(Experience, Result, colour=Budget))+geom_jitter()+
  scale_color_viridis_c()

weddings25 %>% 
  ggplot(aes(Experience, Result, colour=Budget))+geom_jitter()+
  #we can use a compressed color difference to help us see more
  scale_color_gradient2(low= "Black", mid = "Yellow", high="Blue", midpoint = mean(weddings25$Budget))

#these outliers make it no fun
weddings25 %>% 
  ggplot(aes(Experience, Result, colour=Budget))+geom_jitter()+
  scale_color_distiller(palette = "Purples")

#lets do a really complex calculation - let's figure out if any episodes have two brides with the same name
weddings25 %>% 
  group_by(Season, Episode) %>% 
  count(Bride.1) %>% 
  arrange(desc(n))

#what did we learn here - there are no epsidoes where two brides have the same name
#do they get meaner as the series goes?
weddings25 %>% 
  group_by(Season, Episode) %>% 
  summarize(n=mean(Experience)) %>% 
  arrange((n))

#lets plot that
weddings25 %>% 
  group_by(Season, Episode) %>% 
  summarize(n=mean(Experience)) %>% 
  arrange((n)) %>% 
  ggplot(aes(Episode, n))+geom_point()+
  #important new feature - faceting - allows us to make little graphs
  facet_wrap(~Season)

#what if we just want to test the meanness hypothesis?
#look through your data; why are there so many zeros in the C-region
weddings25 %>% 
  group_by(Season, Episode, Bride.1) %>% 
  summarize(mean=sum(C1+C2+C3+C4)/4) %>%
  arrange((mean))

#whats wrong with Ana Maria?

#you can carry a variable through the summarise
weddings25 %>% 
  group_by(Season, Episode, Bride.1) %>% 
  summarize(mean=sum(C1+C2+C3+C4)/4, Result=Result) %>%
  arrange((mean))  

#now if you like to party, plot that
weddings25 %>% 
  group_by(Season, Episode, Bride.1) %>% 
  summarize(mean=sum(C1+C2+C3+C4)/4, Result=Result) %>%
  arrange((mean)) %>% 
  ggplot(aes(Episode, mean, colour=Result))+geom_jitter()+facet_grid(~Season)


#so the halo effect argument might actually be true
weddings25 %>% 
  group_by(Season, Episode, Bride.1) %>% 
  summarize(mean=sum(C1+C2+C3+C4)/4, Result=Result) %>% 
  group_by(Result) %>% 
  summarize(mean(mean))

#ok do we have another theory?
weddings25 %>% 
  group_by(Season, Episode, Bride.1) %>% 
  summarize(mean=sum(C1+C2+C3+C4)/4, Budget = Budget, Result=Result) %>% 
  ggplot(aes(Budget, mean, colour=as.factor(Result)))+geom_jitter()+scale_colour_viridis_d()

#so far we have been controlling a few factors we need to take another step
#we can encode more dimensions
#lets get trashy with it
weddings25 %>% 
  ggplot(aes(Food, Dress, colour=Venue, alpha=Age-Age.1,shape=as.factor(Result)))+
  geom_jitter()+
  scale_color_distiller(palette = "Purples")+facet_grid(~ExpDiff)


#is that any good?

weddings25 %>% 
  ggplot(aes(Food, Dress, size = Budget.1,colour=Venue, alpha=Age-Age.1,shape=as.factor(Budget.1)))+
  geom_jitter()+
  scale_color_distiller(palette = "Purples")+facet_grid(~ExpDiff)+
  theme(plot.background = element_rect(fill = "lightpink"))+
  theme(panel.background = element_rect(fill = "#67c9ff"))+
  ggtitle("Wow what a cool graphic", subtitle = "An exploration of Colour!" )

#is that better?

#so lets simplify
weddings25 %>% 
  ggplot(aes(Food, Dress, colour=Venue))+geom_jitter()+facet_grid(~Result)+scale_color_godfather()

#now that is a good graphic. 

weddings25 %>% 
  ggplot(aes(Food, Dress, colour=Venue))+geom_jitter()+facet_grid(~Result)+
scale_color_distiller(palette = "Greens")+theme(plot.background = element_rect(fill = "lightpink"))+
  theme(panel.background = element_rect(fill = "#67c9ff"))+
  labs(title = "My sweet colors",
       subtitle = "to show some skills",
       caption = "and create a nice thing",
       tag = "Fig. 1")+
  theme(plot.title = element_text(family = "serif",   # Font family
                                  face = "bold",  # Font face
                                  color = 4,      # Font color
                                  size = 15,      # Font size
                                  hjust = 1,      # Horizontal adjustment
                                  vjust = 1,      # Vertical adjustment
                                  angle = -10))+
  #you can use the same sort of code to change every element
  theme(plot.subtitle = element_text(family = "serif",  # Font family
                                     face = "italic",   # Font face
                                     color = 2,   # Font color
                                     size = 15,   # Font size
                                     hjust = -1,  # Horizontal adjustment
                                     vjust = 1,   # Vertical adjustment
                                     angle = +10))


#now lets just use some themes
library(ThemePark)

#lets store this for easy play
basic<-weddings25 %>% ggplot(aes(Budget, Result, colour=Experience))+geom_jitter()

basic+theme_barbie()+scale_color_barbie()+ggtitle("Hello")
basic+theme_futurama()+scale_color_zelda()

library(ggthemes)
basic+theme_solarized_2()+
  #more scales
  scale_color_distiller(palette = "RdPu", direction = 1)

basic+scale_color_viridis_c(option = "turbo")
basic+theme_fivethirtyeight()
basic+theme_excel()
