install.packages('palmerpenguins')
library(palmerpenguins)
library(ggplot2)

ggplot(data=penguins) + geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species))

ggplot(data=penguins) + geom_smooth(mapping = aes(x=flipper_length_mm, y = body_mass_g), color = 'purple') +
  geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g))

ggplot(data=penguins) + geom_smooth(mapping = aes(x=flipper_length_mm, y = body_mass_g, linetype=species), color = 'purple')

ggplot(data=penguins) + geom_jitter(mapping = aes(x=flipper_length_mm, y = body_mass_g))

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill = clarity))

ggplot(data=penguins) + geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species)) + 
  facet_grid(sex~species)

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=color, fill = cut)) +
  facet_wrap(~cut)

ggplot(data=penguins) + geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species)) + 
  labs(title= "Palmer Penguins: Body mass vs. Flipper Length", subtitle = 'Sample of Three Penguin Species',
       caption = 'Data collected by Dr.Kristen Gorman') + 
  annotate('text', x =220, y=3500, label = 'The Gentoos are the largest', color = 'purple', fontface = 'bold', size = 4.5, angle = 25)


p<-ggplot(data=penguins) + geom_point(mapping = aes(x=flipper_length_mm, y = body_mass_g, color = species)) + 
  labs(title= "Palmer Penguins: Body mass vs. Flipper Length", subtitle = 'Sample of Three Penguin Species',
       caption = 'Data collected by Dr.Kristen Gorman') 
p + annotate('text', x =220, y=3500, label = 'The Gentoos are the largest', color = 'purple', fontface = 'bold', size = 4.5, angle = 25)
