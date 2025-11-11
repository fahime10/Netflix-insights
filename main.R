# Import the packages and set up
install.packages("tidyverse")
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))

library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

data <- read.csv("./data/netflix_titles.csv", na.strings = c("", "NA", "N/A"))

View(data)
str(data)
glimpse(data)

colSums(is.na(data))


# Data cleanup
data <- data %>%
  mutate(
    director = replace_na(director, "Unknown"),
    cast = replace_na(cast, "Unknown")
    )

df <- data %>% drop_na(-director, -cast)

df <- df %>%
  mutate(date_added = mdy(date_added), year_added = year(date_added))


## Content type pie chart
count_types = df %>%
  count(type) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         label = paste0(type, " (", percent, "%)"))

ggplot(count_types, aes(x = "", y = n, fill = type)) +
  geom_col(width = 1, col = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3) +
  labs(title = "Netflix Content Types percentage", fill = "Type") +
  theme_void()

## Content contributions around the world
countries <- df %>%
  separate_rows(country, sep = ", ") %>%
  mutate(country, str_trim(country),
         country = str_remove_all(country, ","),
         country = recode(country,
                          "United States" = "United States of America",
                          "Czech Republic" = "Czechia",
                          "West Germany" = "Germany",
                          "East Germany" = "Germany",
                          "Soviet Union" = "Russia",
                          "Vatican City" = "Vatican",
                          "Cayman Islands" = "Cayman Islands",
                          "Dominican Republic" = "Dominican Republic"))

View(countries)
### Check that all countries match the names in the world data
countries_data <- unique(countries$country)
world_countries_data <- unique(world$name)
setdiff(countries_data, world_countries_data)


country_types_count <- countries %>%
  group_by(type, country) %>%
  summarise(total_titles = n(), .groups = "drop")

View(country_types_count)

world <- ne_countries(scale = "medium", returnclass = "sf")

View(world)

world_movie <- world %>%
  left_join(country_types_count %>% filter(type == "Movie"), 
            by = c("name" = "country"))

ggplot(world_movie) +
  geom_sf(aes(fill = total_titles), color = "gray30", size = 0.1) +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "white")+
  labs(title = "Movie Content Availability by Country", 
       fill = "Number of Titles")

world_tv <- world %>%
  left_join(country_types_count %>% filter(type == "TV Show"), 
            by = c("name" = "country"))

ggplot(world_tv) +
  geom_sf(aes(fill = total_titles), color = "gray30", size = 0.1) +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "white")+
  labs(title = "TV Show Content Availability by Country", 
       fill = "Number of Titles")


world_movie_no_US <- world_movie %>%
  mutate(total_titles = if_else(name == "United States of America",
                                NA_real_,
                                total_titles))

ggplot(world_movie_no_US) +
  geom_sf(aes(fill = total_titles), color = "gray30", size = 0.1) +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "white")+
  labs(title = "Movie Content Availability by Country (excluding USA)", 
       fill = "Number of Titles")

world_tv_no_US <- world_tv %>%
  mutate(total_titles = if_else(name == "United States of America",
                                NA_real_,
                                total_titles))

ggplot(world_tv_no_US) +
  geom_sf(aes(fill = total_titles), color = "gray30", size = 0.1) +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "white")+
  labs(title = "TV Show Content Availability by Country (excluding USA)", 
       fill = "Number of Titles")

## Highest rated movies by genre
genres <- df %>%
  separate_rows(listed_in, sep = ", ") %>%
  mutate(listed_in, str_trim(listed_in)) %>%
  rename(genre = listed_in)

View(genres)

### Create a new data frame by type, including genres
genre_type_counts <- genres %>%
  group_by(type, genre) %>%
  summarise(total_titles = n(), .groups = "drop")

View(genre_type_counts)

top_movies <- genre_type_counts %>%
  filter(type == "Movie") %>%
  slice_max(total_titles, n = 10)

top_tv <- genre_type_counts %>%
  filter(type == "TV Show") %>%
  slice_max(total_titles, n = 10)


ggplot(top_movies, aes(x = reorder(genre, total_titles), y = total_titles, 
                       fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 5 Movie Genres on Netflix", x = "Genre", 
       y = "Number of Titles") +
  theme_minimal()
### In this case it is fine if duplicates are considered

ggplot(top_tv, aes(x = reorder(genre, total_titles), y = total_titles, 
                       fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 5 TV Shows Genres on Netflix", x = "Genre", 
       y = "Number of Titles") +
  theme_minimal()


year_counts <- df %>%
  group_by(year_added) %>%
  summarise(total_titles = n(), .groups = "drop")

ggplot(year_counts, aes(x = year_added, y = total_titles)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Number of Netflix Titles Released per Year",
    x = "Year Added",
    y = "Number of Titles"
  )

