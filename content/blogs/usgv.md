---
title: "A data visualisation exercise from Homework 2"
author: "Brent Lewis"
date: "2023-06-18"
image: guns.jpg
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
  pdf_document:
    toc: yes
---
---



# Mass shootings in the US


```
## Rows: 125
## Columns: 14
## $ case                 <chr> "Oxford High School shooting", "San Jose VTA shoo…
## $ year                 <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2020, 2020, 2…
## $ month                <chr> "Nov", "May", "Apr", "Mar", "Mar", "Mar", "Mar", …
## $ day                  <dbl> 30, 26, 15, 31, 22, 16, 16, 26, 10, 6, 31, 4, 3, …
## $ location             <chr> "Oxford, Michigan", "San Jose, California", "Indi…
## $ summary              <chr> "Ethan Crumbley, a 15-year-old student at Oxford …
## $ fatalities           <dbl> 4, 9, 8, 4, 10, 8, 4, 5, 4, 3, 7, 9, 22, 3, 12, 5…
## $ injured              <dbl> 7, 0, 7, 1, 0, 1, 0, 0, 3, 8, 25, 27, 26, 12, 4, …
## $ total_victims        <dbl> 11, 9, 15, 5, 10, 9, 4, 5, 7, 11, 32, 36, 48, 15,…
## $ location_type        <chr> "School", "Workplace", "Workplace", "Workplace", …
## $ male                 <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
## $ age_of_shooter       <dbl> 15, 57, 19, NA, 21, 21, 31, 51, NA, NA, 36, 24, 2…
## $ race                 <chr> NA, NA, "White", NA, NA, "White", NA, "Black", "B…
## $ prior_mental_illness <chr> NA, "Yes", "Yes", NA, "Yes", NA, NA, NA, NA, NA, …
```

| column(variable)     | description                                                                 |
|--------------------------|----------------------------------------------|
| case                 | short name of incident                                                      |
| year, month, day     | year, month, day in which the shooting occurred                             |
| location             | city and state where the shooting occcurred                                 |
| summary              | brief description of the incident                                           |
| fatalities           | Number of fatalities in the incident, excluding the shooter                 |
| injured              | Number of injured, non-fatal victims in the incident, excluding the shooter |
| total_victims        | number of total victims in the incident, excluding the shooter              |
| location_type        | generic location in which the shooting took place                           |
| male                 | logical value, indicating whether the shooter was male                      |
| age_of_shooter       | age of the shooter when the incident occured                                |
| race                 | race of the shooter                                                         |
| prior_mental_illness | did the shooter show evidence of mental illness prior to the incident?      |


-   Generate a data frame that summarizes the number of mass shootings per year.


```r
yearly_shootings_summary <- mass_shootings %>% 
  group_by(year) %>% 
  summarise(shootings=n()) %>% 
  arrange(year)

print(yearly_shootings_summary)
```

```
## # A tibble: 37 × 2
##     year shootings
##    <dbl>     <int>
##  1  1982         1
##  2  1984         2
##  3  1986         1
##  4  1987         1
##  5  1988         1
##  6  1989         2
##  7  1990         1
##  8  1991         3
##  9  1992         2
## 10  1993         4
## # ℹ 27 more rows
```


```r
library(ggplot2)
# Extract race column. Since we will be converting this to a factor, it is best to create a new dataframe with just the required data
shooter_race <- mass_shootings %>%
  select(race) %>% 
  filter(!is.na(race))

print(shooter_race)
```

```
## # A tibble: 114 × 1
##    race 
##    <chr>
##  1 White
##  2 White
##  3 Black
##  4 Black
##  5 White
##  6 White
##  7 White
##  8 Black
##  9 Black
## 10 White
## # ℹ 104 more rows
```

```r
# Convert race column to factor (this this is a categorical variable)
shooter_race$race <- as.factor(shooter_race$race)
# Count the number of mass shooters per race category
shooter_counts <- shooter_race %>%
  group_by(race) %>%
  summarize(Shooters = n()) %>%
  arrange(desc(Shooters))
  
ggplot(shooter_counts, aes(x = reorder(race, -Shooters), y = Shooters)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Shooters), vjust = -0.5, size = 3, color = "black") +  # Add data labels
  labs(x = "Race", y = "Number of Mass Shooters") +
  ggtitle("Most mass shooters in the US are white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-4-1.png" width="672" />

-   Generate a boxplot visualizing the number of total victims, by type of location.


```r
ggplot(mass_shootings, aes(x = location_type, y = total_victims)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(x = "Location", y = "Total Victims") +
  ggtitle("Number of Total Victims by Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-5-1.png" width="672" />

```r
# We can see that the outliers distort this graph and undermine its value
```

-   Redraw the same plot, but remove the Las Vegas Strip massacre from the dataset.


```r
mass_shootings %>% 
  filter(case != "Las Vegas Strip massacre") %>% 
  ggplot(aes(x = location_type, y = total_victims)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(x = "Location type", y = "Total Victims") +
  ggtitle("Shootings at schools and military bases tend to be the most deadly") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-6-1.png" width="672" />


-   How many white males with prior signs of mental illness initiated a mass shooting after 2000?


```r
library(dplyr)

# Filter the dataset for white males with prior signs of mental illness after 2000
white_males_with_mental_issues <- mass_shootings %>%
  filter(male == "TRUE" & race == "White" & prior_mental_illness == "Yes" & year >= 2000, na.rm=TRUE) %>% 
  count(shooter = n())

# Print the result
print(paste("Number of white males with prior signs of mental illness initiating a mass shooting after 2000:", white_males_with_mental_issues))
```

```
## [1] "Number of white males with prior signs of mental illness initiating a mass shooting after 2000: 23"
## [2] "Number of white males with prior signs of mental illness initiating a mass shooting after 2000: 23"
```

-   Which month of the year has the most mass shootings? Generate a bar chart sorted in chronological (natural) order (Jan-Feb-Mar- etc) to provide evidence of your answer.


```r
library(lubridate)

# Count the number of mass shootings per month
shootings_per_month <- mass_shootings %>%
  group_by(month) %>%
  summarise(shootings = n()) %>% 
  mutate(month_num = as.numeric(match(month, month.abb))) %>% 
  mutate(month = fct_reorder(month, month_num, max)) %>%
  arrange(month_num) %>% 
  ggplot(aes(x = month, y = shootings)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Month", y = "Number of Mass Shootings") +
  ggtitle("Most mass shootings occur in February") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(shootings_per_month)
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-8-1.png" width="672" />

-   How does the distribution of mass shooting fatalities differ between White and Black shooters? What about White and Latino shooters?


```r
library(ggplot2)

# Filter the data for white and black shooters
white_black_split <- mass_shootings %>%
  filter(race %in% c("White", "Black") & !is.na(race) & race != "Latino")  %>%
  mutate(month_num = as.numeric(match(month, month.abb))) %>% 
  mutate(month = fct_reorder(month, month_num, max)) %>% 
  select(race, fatalities, month, month_num) 


# Create a bar plot to visualize the distribution
ggplot(white_black_split, aes(x = month, fill = race)) +
  geom_bar(position = "stack", stat = "sum", aes(y = fatalities)) +
  scale_fill_manual(values = c("White" = "blue", "Black" = "red")) +
  labs(x = "Month", y = "Number of Fatalities", fill = "Race") +
  ggtitle("Distribution of Fatalities for White and Black Shootings by Month")
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-9-1.png" width="672" />

```r
# The bar chart below represents the racial breakdown of shooters between white and black
ggplot(white_black_split, aes(x = month, fill = race)) +
  geom_bar(position = "fill", aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = c("White" = "blue", "Black" = "red")) +
  labs(x = "Month", y = "Percentage", fill = "Race") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("White shooters generally outnumber black shooters, except in February")
```

```
## Warning: The dot-dot notation (`..count..`) was deprecated in ggplot2 3.4.0.
## ℹ Please use `after_stat(count)` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-9-2.png" width="672" />

```r
# Recreate the code above, this time filtering for white and latino shooters and removing black shooters.
white_latino_split <- mass_shootings %>%
  filter(race %in% c("White", "Latino") & !is.na(race) & race != "Black")  %>%
  mutate(month_num = as.numeric(match(month, month.abb))) %>% 
  mutate(month = fct_reorder(month, month_num, max))

# Create a bar plot to visualize the distribution
ggplot(white_latino_split, aes(x = month, fill = race)) +
  geom_bar(position = "stack", stat = "sum", aes(y = fatalities)) +
  scale_fill_manual(values = c("White" = "blue", "Latino" = "red")) +
  labs(x = "Month", y = "Number of Fatalities", fill = "Race") +
  ggtitle("Distribution of Fatalities for White and Black Shootings by Month")
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-9-3.png" width="672" />

-   Are mass shootings with shooters suffering from mental illness different from mass shootings with no signs of mental illness in the shooter?


```r
# Filter the data for rows with known mental illness status
mass_shootings_with_illness <- mass_shootings[!is.na(mass_shootings$prior_mental_illness), ]

# Reshape the data into long format
reshaped_data <- pivot_longer(mass_shootings_with_illness, cols = c(fatalities, injured, total_victims),
                              names_to = "Variable", values_to = "Count")

print(reshaped_data)
```

```
## # A tibble: 237 × 13
##    case     year month   day location summary location_type male  age_of_shooter
##    <chr>   <dbl> <chr> <dbl> <chr>    <chr>   <chr>         <lgl>          <dbl>
##  1 San Jo…  2021 May      26 San Jos… "Samue… Workplace     TRUE              57
##  2 San Jo…  2021 May      26 San Jos… "Samue… Workplace     TRUE              57
##  3 San Jo…  2021 May      26 San Jos… "Samue… Workplace     TRUE              57
##  4 FedEx …  2021 Apr      15 Indiana… "Brand… Workplace     TRUE              19
##  5 FedEx …  2021 Apr      15 Indiana… "Brand… Workplace     TRUE              19
##  6 FedEx …  2021 Apr      15 Indiana… "Brand… Workplace     TRUE              19
##  7 Boulde…  2021 Mar      22 Boulder… "Ahmad… Workplace     TRUE              21
##  8 Boulde…  2021 Mar      22 Boulder… "Ahmad… Workplace     TRUE              21
##  9 Boulde…  2021 Mar      22 Boulder… "Ahmad… Workplace     TRUE              21
## 10 Odessa…  2019 Aug      31 Odessa,… "Seth … Other         TRUE              36
## # ℹ 227 more rows
## # ℹ 4 more variables: race <chr>, prior_mental_illness <chr>, Variable <chr>,
## #   Count <dbl>
```

```r
# Create a bar plot facet-wrapped by fatalities, injuries, and total victims
ggplot(reshaped_data, aes(x = prior_mental_illness, y = Count, fill = prior_mental_illness)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "Prior Mental Illness", y = "Mean", fill = "Prior mental illness") +
  ggtitle("On average, shooters with prior-reported mental illnesses are more dangerous")
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-10-1.png" width="672" />

-   Assess the relationship between mental illness and total victims, mental illness and location type, and the intersection of all three variables.


```r
# Filter the data for rows with known prior_mental_illness status and location_type
mass_shootings_updated <- mass_shootings[!is.na(mass_shootings$prior_mental_illness) & !is.na(mass_shootings$location_type), ]

# Assess the relationship between prior_mental_illness and total_victims
ggplot(mass_shootings_updated, aes(x = prior_mental_illness, y = total_victims)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Prior Mental Illness", y = "Total Victims") +
  ggtitle("Shooters with mental illnesses tend to claim more victims")
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-11-1.png" width="672" />

```r
# We can see from this graph that both the average number of victims and variability of victims is higher for shooters with mental illness

# Assess the relationship between prior_mental_illness and location_type
ggplot(mass_shootings_updated, aes(x = prior_mental_illness, fill = location_type)) +
  geom_bar(position = "fill") +
  labs(x = "Prior Mental Illness", y = "Proportion", fill = "Location Type") +
  ggtitle("Mentally ill shooters are more likely to target airports, military bases and religious site")
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-11-2.png" width="672" />

```r
# We can see here that there is a fairly even distribution of shooting locations among mentally ill and non-mentally ill shooters, however mentally ill shooters are target these other niche locations while non-mentally ill shooters do not 

# Assess the intersection of prior_mental_illness, location_type, and total_victims
ggplot(mass_shootings_updated, aes(x = prior_mental_illness, fill = location_type, y = total_victims)) +
  geom_boxplot() +
  labs(x = "Prior Mental Illness", y = "Total Victims", fill = "Location Type") +
  ggtitle("Mentally ill shooters are more lethal")
```

<img src="/blogs/usgv_files/figure-html/unnamed-chunk-11-3.png" width="672" />

```r
# We can see here that mean victims and standard deviations of total victims is higher at all location types for mentally ill shooters as compared with shooters with no prior reported mental illness (with emphasis on "reported"). We can also see that most shooters - either mentally ill or not - tend to cause the most damage at schools.
```

