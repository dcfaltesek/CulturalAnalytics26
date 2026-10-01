#lets go ahead and work on some graphics about weddings
library(dplyr)
library(ggplot2)

#underlying any graphic is a basic structure, for now we will work with three variables
#to get more sophisticated graphics we will actually manipulate the underlying data

#lets just run the basic weddings graph

#a core ggplot
#calling the DATASET as the left argument, then X is guests, Y is budget, and the colour will be their place
ggplot(weddings25, aes(Guests, Budget, colour=Result))+geom_point()

#do we see any correlation between guests and performance?
#matybe we need a better mathematical test
#x and then y
cor.test(weddings25$Budget,weddings25$Result)


#ok, lets make sense of this graphically

ggplot(weddings25, aes(Result, Budget, colour=Guests))+geom_point()

#lets play with the geoms
#for math reasons, we are going to start doing the math correctly - in this case Result is a FACTOR
ggplot(weddings25, aes(as.factor(Result), Budget, colour=Guests))+geom_violin()
ggplot(weddings25, aes(as.factor(Result), Budget, colour=Guests))+geom_boxplot()

#now so you can show a little more control, lets consider ways to control the color a little more
#this is a continuous color variable
#default blue
ggplot(weddings25, aes(Food, Budget, colour=Guests))+geom_point()

#now lets change that up
ggplot(weddings25, aes(as.factor(Result), Budget, colour=Guests))+geom_point()+scale_color_distiller(palette = "Greens")
ggplot(weddings25, aes(as.factor(Result), Budget, colour=Guests))+geom_point()+scale_color_distiller(palette = "Purples")
#based on the graphic set a good midpoint of like 350
ggplot(weddings25, aes(as.factor(Result), Budget, colour=Guests))+geom_point()+scale_color_gradient2(low = "red", high = "yellow",
                                                                                                     mid = "orange", midpoint = 350)


#now lets do a little piping
#what if we want to know the most expensive state for weddings
weddings25 |> 
  group_by(State) |> 
  #create three nicely labeled summary categories
  summarise(cost=mean(Budget), guests=mean(Guests), kindness=mean(Experience)) |> 
  ggplot(aes(cost, guests, colour=kindness))+
  #lets label it nicely
  geom_text(aes(label=State))+
  #and lets throw a fun color scheme
  scale_color_gradient2(low = "purple", high = "orange",midpoint = 18)
