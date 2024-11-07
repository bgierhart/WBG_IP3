#Install packages
install.packages('plyr')

#Load necessary libraries
library(tidyverse)
library(plyr)
library(tidyr)

#Load the 'starwars' dataset
data<-starwars

#Initial exploration
head(data)
str(data)
summary(data)

#Check if there are any missing values
sapply(data, function(x) sum(is.na(x)))

#Clean data
#Replace empty numerical values with median and delete rows with empty values
clean_starwars<-data %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE)))) %>% na.omit()
sapply(clean_starwars, function(x) sum(is.na(x)))

#Exploration of clean data
#Initial exploration
head(clean_starwars)
str(clean_starwars)
summary(clean_starwars)

#Frequencies of Categorical Variables
count(clean_starwars, 'hair_color')
count(clean_starwars, 'skin_color')
count(clean_starwars, 'eye_color')
count(clean_starwars, 'sex')
count(clean_starwars, 'gender')
count(clean_starwars, 'homeworld')
count(clean_starwars, 'species')
clean_starwars %>%
  unnest('films') %>%      # In order to count within list
  count('films')  
clean_starwars %>%
  unnest('vehicles') %>%      # In order to count within list
  count('vehicles') 
clean_starwars %>%
  unnest('starships') %>%      # In order to count within list
  count('starships')
#Lots of single instances of species, homeworld, etc. Male characters much more present than female characters.

#Correlation Analysis
cor_matrix<-cor(clean_starwars %>% select_if(is.numeric))
print(cor_matrix)  

ggplot(data=as.data.frame(as.table(cor_matrix)), aes(Var1, Var2, fill=Freq))+geom_tile(color="white")+
  scale_fill_gradient2(low='blue', high='red', mid='white', midpoint=0, limit=c(-1,1))+
  theme_minimal()
#As expected, there is a strong positive correlation between height and mass.

#Univariate Analysis
#Histogram plot of height
ggplot(clean_starwars, aes(x=height))+
  geom_histogram(bins=15,fill='skyblue', color='black')+
  labs(title="Histogram Plot of height", x="Height", y="Count")

#Histogram plot of mass
ggplot(clean_starwars, aes(x=mass))+
  geom_histogram(bins=15,fill='green', color='black')+
  labs(title="Histogram Plot of mass", x="Mass", y="Count")
#The above histograms measure distribution of height and mass of Star Wars characters. Both histograms show that the distribution is approximately normal; however, many of the datapoints lie at the mean/median.

# Bar plot for sex (categorical)
ggplot(clean_starwars, aes(x=sex))+
  geom_bar(bins=15,fill='skyblue', color='black')+
  labs(title="Bar Chart of Sex of Star Wars Characters", x="Sex", y="Count")
#We can see from this bar chart that there are nearly four times as many male characters than female characters in the dataset.

# Bar plot for species (categorical)
ggplot(clean_starwars, aes(x = species)) +
  geom_bar(fill = 'green', color = 'black') +
  labs(title = "Bar Chart of Star Wars Character Species", x = "Species", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#This bar chart shows the number of characters representative of different species in the dataset. By far the most represented species. In this franchise, many characters are tokens of their race.

# Bar plot for films (categorical)
clean_starwars %>%
  unnest(films) %>%
  ggplot(aes(x = films)) +
  geom_bar(fill = 'red', color = 'black') +
  labs(title = "Bar Chart of Star Wars Character Film Appearances", x = "Films", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#This bar chart shows the number of character appearances for most films in the series. The film with the most character appearances is Attack of the Clones. Revenge of the Sith and The Phantom Menace are closed behind. The prequel trilogy features many more character appearances than the other films in the series. The Force Awakens, the only sequel trilogy film represented in this dataset, has by far the least.

#bivariate analysis
ggplot(clean_starwars) +
  geom_point(aes(x = height, y = mass), color = "red", size = 2) +
  labs(
    title = "Scatter Plot of height vs. mass of Star Wars Characters",  # Title
    x = "Height (cm)",                                # X-axis label
    y = "Mass (kg)"                                   # Y-axis label
  ) +
  theme_minimal()
#Above is a scatter plot of height vs. mass of Star Wars characters. Many characters are clustered in the means of height and mass, which makes sense given the distribution shown earlier. There is still a slight positive linear relationship present, indicating that, generally, taller characters have more mass.

#Boxplot species for mass
ggplot(clean_starwars, aes(x = species, y=mass)) +
  geom_boxplot(fill = 'purple', color = 'black') +
  labs(title = "Box Plot of Star Wars Character Species Mass", x = "Species", y = "Mass") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#This box plot shows the distribution of mass for each species in the dataset. We have the most data on humans, and we can see that most of them are at the mean. That species also has the most outliers. Wookies are the species with the most mass, and they and Twi’leks have means at the center. Gunguns have a mean higher in their distribution, meaning that two of the three gungun characters in the dataset are heavier than the third and influencing the mean.

#Conclusions
#The Star Wars franchise is considered one that is massive and full of imaginative characters. I still believe this is true, especially for its time. The data shows, however, that the majority of characters are human and characters of other species are usually the only ones given significant roles in the narrative. This makes sense as people probably connect more with human characters in a sci-fi story. The majority of the characters are also male. Even when looking at humans, traits like mass and height are homogenous. Popular opinion is that the prequel trilogy is not as good as the original trilogy from an artistic standpoint. However, in terms of imagination and world-building, the prequel films have the highest distribution of different characters.

