#day six - pivots
#OK so I just gotta know, what do you think of the green ranger?
library(dplyr)
library(tidyr)
library(ggplot2)
library(ThemePark)

#go ahead an import dataset zords
zords

#Someone quick use the Dragon Dagger!

#ok can you make a GGPLOT OF IT? LIKE BY SEASON?

#this will be part of the idea for next TUESDAY
Seasons<-c("Dino", "Thunder", "Ninja","Shogun", "Zeo", "Aquatar", "SuperZeo")
Number<-c(1,2,3,4,5,6,7)
ranger_seasons<-data.frame(Seasons, Number)

#but this isn't easy at all
head(zords)

#that is because it is WIDE data, which is really nice for a human brain to look at
#computers need LONG data

zords %>% 
  #first step - we need to take everything EXCEPT the first column and make it LONGER
  pivot_longer(-Ranger)

#Well that was awesome, but it is missing some things, we need to get it to say the right stuff for name and value
zords %>% 
  pivot_longer(-Ranger, names_to="Seasons", values_to="Zord")

#well that was easy
library(dplyr)
long_rangers<-zords %>% 
  pivot_longer(-Ranger, names_to="Seasons", values_to="Zord") 

inner_join(long_rangers, ranger_seasons, by="Seasons") 

#so now we can plot that nonsense...
library(ggplot2)
inner_join(long_rangers, ranger_seasons) %>% 
  ggplot(aes(Number, Zord, colour=Ranger))+geom_point()


#for your analysis
full_rangers<-inner_join(long_rangers, ranger_seasons)

#pivots 
#why do we do them? 
#cognition - we like to look at wide data

#lets explore the function
#documentation
#https://tidyr.tidyverse.org/reference/pivot_longer.html

#now we get into the really weird fun stuff

#lets finally make a plot of this wedding level scores stuff

??pivot_longer()
weddings25 %>% 
  #I just want it to make this little wide part long
  pivot_longer(cols=c(C1,C2,C3,C4))


long_weddings<-weddings25 %>% 
  #I just want it to make this little wide part long
  pivot_longer(cols=c(C1,C2,C3,C4), 
               #the names of the columns that are being pivoted
               names_to = "BrideScored", 
               #what those values mean
               values_to="ScoreGiven")

#lets see what it named the column
glimpse(long_weddings)

#the holy grail: a scatterplot of what each bride awarded
long_weddings %>% 
  #lets get rid of those pesky zeros from their own wedding
  filter(ScoreGiven!=0) %>% 
  ggplot(aes(ScoreGiven, Experience, colour=Result))+geom_jitter()+scale_color_alien()+theme_barbie()

#lets pivot your datasets
#now lets scrape.

#scraping is a very helpful skill
#when we scrape we can use R like a web browser
#so long as you have a sense of how a webpage is built you can find where the goodies are

library(rvest)

url<-"https://en.wikipedia.org/wiki/Grammy_Award_for_Best_New_Artist"
css<-"#mwB8w"

url %>% 
  read_html() %>% 
  html_element(css=css) %>% 
  html_table()

result<-url %>% 
  read_html() %>% 
  html_element(css=css) %>% 
  html_table()

#you can also grab text by style
urlB<-"https://theonion.com/republican-infighting-erupts-over-whether-trump-bill-beautiful-or-handsome/"
css<-".theonion-dingbat"

urlB %>% 
  read_html() %>% 
  html_element(css=css) %>% 
  html_text()




