Key Crop Yields
================

Let’s look at key crop yields, and see if we can build some models
around the data.

``` r
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')
```

Let’s look at production over time of our key crops:

``` r
key_crops <- c("Potatoes", "Banans", "Maize", "Rice", "Wheat", "Barley")

key_crop_yields %>%
  pivot_longer(ends_with("per hectare)"),
    names_to = "Crop",
    values_to = "Amount"
  ) %>%
  mutate(Crop = str_replace(Crop, coll(" (tonnes per hectare)"), "")) %>%
  group_by(Crop, Year) %>%
  summarise(TotalProduction = sum(Amount, na.rm = T)) %>%
  filter(!is.na(TotalProduction) & Crop %in% key_crops) %>%
  ggplot(aes(x = Year, y = TotalProduction, colour = Crop)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Production (tonnes per hectare)"
  ) +
  theme(
    legend.position = "none"
  ) +
  facet_wrap(~Crop, scales = "free_y")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- --> Let’s take a
look now by continent:

``` r
library(countrycode)

country_to_continent <- key_crop_yields %>%
  distinct(Entity) %>%
  mutate(Continent = countrycode(
    sourcevar = Entity, origin = "country.name",
    destination = "continent"
  )) %>%
  filter(!is.na(Continent))
```

    ## Warning: Problem with `mutate()` input `Continent`.
    ## x Some values were not matched unambiguously: Africa, Americas, Asia, Asia, Central, Australia & New Zealand, Belgium-Luxembourg, Caribbean, Central America, Czechoslovakia, Eastern Africa, Eastern Asia, Eastern Europe, Europe, Europe, Western, European Union, Land Locked Developing Countries, Least Developed Countries, Low Income Food Deficit Countries, Melanesia, Micronesia (country), Micronesia (region), Middle Africa, Net Food Importing Developing Countries, Northern Africa, Northern America, Northern Europe, Oceania, Pacific Islands Trust Territory, Polynesia, Serbia and Montenegro, Small island developing States, South America, South Eastern Asia, Southern Africa, Southern Asia, Southern Europe, Timor, Western Africa, Western Asia, World, Yugoslavia
    ## i Input `Continent` is `countrycode(sourcevar = Entity, origin = "country.name", destination = "continent")`.

    ## Warning in countrycode(sourcevar = Entity, origin = "country.name", destination = "continent"): Some values were not matched unambiguously: Africa, Americas, Asia, Asia, Central, Australia & New Zealand, Belgium-Luxembourg, Caribbean, Central America, Czechoslovakia, Eastern Africa, Eastern Asia, Eastern Europe, Europe, Europe, Western, European Union, Land Locked Developing Countries, Least Developed Countries, Low Income Food Deficit Countries, Melanesia, Micronesia (country), Micronesia (region), Middle Africa, Net Food Importing Developing Countries, Northern Africa, Northern America, Northern Europe, Oceania, Pacific Islands Trust Territory, Polynesia, Serbia and Montenegro, Small island developing States, South America, South Eastern Asia, Southern Africa, Southern Asia, Southern Europe, Timor, Western Africa, Western Asia, World, Yugoslavia

    ## Warning: Problem with `mutate()` input `Continent`.
    ## x Some strings were matched more than once, and therefore set to <NA> in the result: Australia & New Zealand,Oceania,Oceania
    ## i Input `Continent` is `countrycode(sourcevar = Entity, origin = "country.name", destination = "continent")`.

    ## Warning in countrycode(sourcevar = Entity, origin = "country.name", destination = "continent"): Some strings were matched more than once, and therefore set to <NA> in the result: Australia & New Zealand,Oceania,Oceania

``` r
key_crop_yields %>%
  inner_join(country_to_continent) %>%
  pivot_longer(ends_with("per hectare)"),
    names_to = "Crop",
    values_to = "Amount"
  ) %>%
  mutate(Crop = str_replace(Crop, coll(" (tonnes per hectare)"), "")) %>%
  filter(Crop %in% key_crops) %>%
  group_by(Year, Continent, Crop) %>%
  summarise(Total_Production = sum(Amount, na.rm = T)) %>%
  ggplot(aes(x = Year, y = Total_Production, colour = Crop)) +
  geom_line(size = 1.5) +
  facet_wrap(~Continent, scales = "free_y") +
  labs(
    x = NULL,
    y = "Total Production (tonnes per hectare)"
  )
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## So, what impacts production of potatoes?

``` r
potato_data <- key_crop_yields %>%
  inner_join(country_to_continent) %>%
  transmute(
    Entity,
    Year,
    Continent,
    Potatoes = `Potatoes (tonnes per hectare)`
  )


arable_land <- arable_land %>%
  transmute(Entity, Year, ArableLand = `Arable land needed to produce a fixed quantity of crops ((1.0 = 1961))`)

fertilizer_use <- fertilizer %>%
  transmute(Entity, Year, FertilizerUse = `Nitrogen fertilizer use (kilograms per hectare)`) 

tractor_use_pop <- tractors %>%
  transmute(Entity, Year = as.integer(Year),
            TractorsPer100SqKm = `Tractors per 100 sq km arable land`,
            Population = `Total population (Gapminder)`)
```

    ## Warning: Problem with `mutate()` input `Year`.
    ## x NAs introduced by coercion
    ## i Input `Year` is `as.integer(Year)`.

    ## Warning in mask$eval_all_mutate(dots[[i]]): NAs introduced by coercion

``` r
potato_full_data <- potato_data %>%
  inner_join(arable_land) %>%
  inner_join(fertilizer_use) %>%
  inner_join(tractor_use_pop) %>%
  mutate(across(where(is.numeric), replace_na, replace=0))
```

Let’s do a quick correlation plot

``` r
library(corrplot)

potato_full_data %>%
  select(-Entity, -Continent) %>%
  cor() %>%
  corrplot()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Linear regression time

``` r
library(tidymodels)

potato_recp <- recipe(Potatoes ~ ., data = potato_full_data) %>%
  update_role(Entity, Year, new_role="id") %>%
  step_dummy(Continent) %>%
  step_normalize(all_predictors()) %>%
  step_zv(all_predictors()) 

potato_prep <- prep(potato_recp)

potato_prep
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##         id          2
    ##    outcome          1
    ##  predictor          5
    ## 
    ## Training data contained 8120 data points and no missing data.
    ## 
    ## Operations:
    ## 
    ## Dummy variables from Continent [trained]
    ## Centering and scaling for ArableLand, FertilizerUse, ... [trained]
    ## Zero variance filter removed no terms [trained]

``` r
potato_wf <- workflow() %>%
  add_recipe(potato_recp) %>%
  add_model(linear_reg() %>% set_engine('lm'))

potato_wf
```

    ## == Workflow ====================================================================================
    ## Preprocessor: Recipe
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor --------------------------------------------------------------------------------
    ## 3 Recipe Steps
    ## 
    ## * step_dummy()
    ## * step_normalize()
    ## * step_zv()
    ## 
    ## -- Model ---------------------------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
potato_fit <- potato_wf %>%
  fit(data = potato_full_data)

potato_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  mutate(across(where(is.numeric), round, digits=3)) %>%
  arrange(-estimate) %>%
  knitr::kable()
```

| term                | estimate | std.error | statistic | p.value |
| :------------------ | -------: | --------: | --------: | ------: |
| (Intercept)         |   11.404 |     0.092 |   124.213 |       0 |
| Continent\_Europe   |    3.906 |     0.119 |    32.848 |       0 |
| FertilizerUse       |    2.599 |     0.095 |    27.215 |       0 |
| TractorsPer100SqKm  |    2.339 |     0.106 |    22.005 |       0 |
| Continent\_Asia     |    2.102 |     0.109 |    19.199 |       0 |
| Continent\_Americas |    0.763 |     0.107 |     7.137 |       0 |
| Population          |    0.505 |     0.096 |     5.237 |       0 |
| Continent\_Oceania  |    0.412 |     0.098 |     4.219 |       0 |
| ArableLand          |  \-0.634 |     0.092 |   \-6.891 |       0 |
