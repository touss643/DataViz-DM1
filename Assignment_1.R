# Chargement des données
library(readxl)
df <- read_excel("nobel_winners.xlsx")

##########################################################
############### Représentations graphiques ############### 
##########################################################

library(ggplot2)
library(tidyverse)
library(tidytext)

## Graphique de l'IA


### 1. Nuage de mots des motivations ###

library(ggwordcloud)

df %>%
  unnest_tokens(word, motivation) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  head(30) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud_area(shape = "circle") +
  scale_size_area(max_size = 20) +
  theme_minimal()


### 2. Répartition par genre et catégorie ###

df %>% 
  filter(gender %in% c("Male", "Female")) %>% 
  ggplot(aes(x = category, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Répartition hommes/femmes par catégorie",
       x = "Catégorie", y = "Nombre de lauréats") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")

### 3. Top 10 des pays d'origine des lauréats ###

df %>% 
  filter(!is.na(birth_country)) %>% 
  count(birth_country, sort = TRUE) %>% 
  head(10) %>% 
  mutate(birth_country = fct_reorder(birth_country, n)) %>% 
  ggplot(aes(x = birth_country, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 des pays d'origine des lauréats",
       x = "", y = "Nombre de prix")

### 4. Distribution d'âge des lauréats par catégorie ###

df %>% 
  mutate(age = prize_year - lubridate::year(birth_date)) %>% 
  ggplot(aes(x = category, y = age)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribution d'âge des lauréats par catégorie",
       x = "", y = "Âge au moment du prix") +
  coord_flip()

### 5. Répartition géographique des institutions lauréates ###

library(maps)
world <- map_data("world")

df %>% 
  filter(!is.na(organization_country)) %>% 
  count(organization_country) %>% 
  right_join(world, by = c("organization_country" = "region")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(color = "white") +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Répartition géographique des institutions lauréates")

### 6. Part des institutions dans les prix par catégorie ###

df %>% 
  mutate(org_present = ifelse(is.na(organization_name), "Indépendant", "Institution")) %>% 
  ggplot(aes(x = category, fill = org_present)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("grey70", "darkblue")) +
  labs(title = "Part des institutions dans les prix par catégorie",
       x = "", y = "Proportion") +
  coord_flip()

## Choix de graphique et améliorations

## Le graphique portant sur la répartition hommes/femmes par catégorie
## a été retunu et amélioré

library(tidyverse)

# Créer un vecteur de traduction
nobel_categories_fr <- c(
  "Physics" = "Physique",
  "Chemistry" = "Chimie",
  "Medicine" = "Médecine",
  "Literature" = "Littérature",
  "Peace" = "Paix",
  "Economics" = "Économie"
)

df %>% 
  filter(gender %in% c("Male", "Female")) %>% 
  ggplot(aes(x = category, fill = gender)) +
  geom_bar(position = "fill") +  # Normalisation des barres (100% par catégorie)
  scale_y_continuous(labels = scales::percent_format()) +  # Afficher en pourcentage
  geom_text(stat = "count", aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 4) + # Ajout des labels
  labs(title = "Prix Nobel : Répartition des hommes et des femmes par catégorie (1901-2016)",
       subtitle = "La paix et la littérature comptent le plus de femmes lauréates",
       caption = "Source : Nobel Prize Archive",
       x = "Catégorie", 
       y = "Proportion") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  # Ajout de la traduction
  scale_x_discrete(labels = nobel_categories_fr) +
  scale_fill_brewer(
    palette = "Set2",
    name = "Genre",  # Titre
    labels = c("Female" = "Femmes", "Male" = "Hommes")  # Étiquettes
  )
