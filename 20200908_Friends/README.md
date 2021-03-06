Key Crop Yields
================

## Step 1: Load Data

``` r
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')
```

## Step 2: Tidy Data

``` r
friends_ratings <- friends_info %>%
  select(season, episode, air_date, us_views_millions, imdb_rating) %>%
  mutate(across(c(season, episode), as.factor))

friends_speakers <- friends %>%
  filter(!is.na(speaker) & speaker != 'Scene Directions') %>%
  mutate(across(c(season, episode, speaker), as.factor)) %>%
  mutate(speaker = fct_lump_n(speaker, n = 20)) %>%
  group_by(season, episode, speaker) %>%
  summarise(lines = n()) %>%
  pivot_wider(names_from = 'speaker', values_from = 'lines',
              values_fill = 0) %>%
  janitor::clean_names() %>%
  rename(all_characters = number_all_number)

  
friends_directors <- friends_info %>%
  select(season, episode, directed_by) %>%
  mutate(across(c(season, episode), as.factor)) %>%
  add_count(directed_by) %>%
  filter(n > 20) %>%
  distinct(season, episode, directed_by) %>%
  mutate(director_value = 1) %>%
  pivot_wider(names_from = 'directed_by', values_from = 'director_value',
              values_fill = 0)%>%
  janitor::clean_names()

friends_writers <- friends_info %>%
  select(season, episode, written_by) %>%
  mutate(across(c(season, episode), as.factor)) %>%
  separate_rows(written_by, sep=" & ") %>%
  add_count(written_by) %>%
  filter(n > 20) %>%
  distinct(season, episode, written_by) %>%
  mutate(writer_value = 1)  %>%
  pivot_wider(names_from = 'written_by', values_from = 'writer_value',
              values_fill = 0)%>%
  janitor::clean_names()


friends_model <- friends_ratings %>%
  left_join(friends_speakers) %>%
  left_join(friends_writers) %>%
  left_join(friends_directors)
```

## Step 3: Some EDA

Is there a relationship with ratings over time?

``` r
friends_model %>%
  ggplot(aes(x=air_date, y=imdb_rating)) +
  geom_point(aes(colour=season), show.legend=F) +
  geom_smooth(se=F) +
  labs(x=NULL, y='IMDB Rating')
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
friends_model %>%
  ggplot(aes(x=season, y=imdb_rating)) +
  geom_boxplot(aes(colour=season), show.legend=F) +
  labs(x=NULL, y='IMDB Rating')
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
