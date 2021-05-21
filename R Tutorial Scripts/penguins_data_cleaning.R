penguins %>% 
  select(-species)

penguins %>% 
  rename(island_new = island)

rename_with(penguins,tolower)
clean_names(penguins)

penguins %>% 
  arrange(-bill_length_mm)

penguins2 <- penguins %>% 
  arrange(-bill_length_mm)
View(penguins2)

penguins %>%  group_by(island) %>% 
  drop_na() %>% 
  summarise(mean_bill_lengh = mean(bill_length_mm))

penguins %>%  group_by(island) %>% 
  drop_na() %>% 
  summarise(max_bill_lengh = max(bill_length_mm))

penguins %>% 
  group_by(species, island) %>% 
  drop_na() %>% 
  summarize(max_bl = max(bill_length_mm), mean_bl = mean(bill_length_mm))

penguins %>% 
  filter(species == "Adeline")

penguins %>% 
  mutate(body_mass_kg = body_mass_g/1000, flipper_length_m = flipper_length_mm/1000)
