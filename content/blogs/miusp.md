---
title: "A web-scraping exercise from Homework 3"
author: "Brent"
date: "2023-06-18"
image: dirtymoney.jpg
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



# Money in US politics

In the United States, [*"only American citizens (and immigrants with green cards) can contribute to federal politics, but the American divisions of foreign companies can form political action committees (PACs) and collect contributions from their American employees."*](https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs)

We will scrape and work with data foreign connected PACs that donate to US political campaigns. The data for foreign connected PAC contributions in the 2022 election cycle can be found at https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022. Then, we will use a similar approach to get data such contributions from previous years so that we can examine trends over time.

All data come from [OpenSecrets.org](https://www.opensecrets.org), a *"website tracking the influence of money on U.S. politics, and how that money affects policy and citizens' lives"*.


```r
library(robotstxt)
paths_allowed("https://www.opensecrets.org")

base_url <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022"

contributions_tables <- base_url %>%
  read_html() 

contributions <- contributions_tables %>% 
  html_element(".DataTable-Partial") %>% ## select table element
  html_table()

class(contributions)
```

- First, make sure you can scrape the data for 2022. Use janitor::clean_names() to rename variables scraped using `snake_case` naming. 

```

- Clean the data: 

    -   Write a function that converts contribution amounts in `total`, `dems`, and `repubs` from character strings to numeric values.
    -   Separate the `country_of_origin_parent_company` into two such that country and parent company appear in different columns for country-level analysis.


```r
# write a function to parse_currency
parse_currency <- function(x){
  x %>%
    
    # remove dollar signs
    str_remove("\\$") %>%
    
    # remove all occurrences of commas
    str_remove_all(",") %>%
    
    # convert to numeric
    as.numeric()
}

# clean country/parent co and contributions 
contributions <- contributions %>%
  janitor::clean_names() %>% 
  separate(country_of_origin_parent_company, 
           into = c("country", "parent"), 
           sep = "/", 
           extra = "merge") %>%
  mutate(
    total = parse_currency(total),
    dems = parse_currency(dems),
    repubs = parse_currency(repubs)
  )

print(contributions)
```

-   Write a function called `scrape_pac()` that scrapes information from the Open Secrets webpage for foreign-connected PAC contributions in a given year. This function should

    -   have one input: the URL of the webpage and should return a data frame.
    -   add a new column to the data frame for `year`. We will want this information when we ultimately have data from all years, so this is a good time to keep track of it. Our function doesn't take a year argument, but the year is embedded in the URL, so we can extract it out of there, and add it as a new column. Use the `str_sub()` function to extract the last 4 characters from the URL. You will probably want to look at the help for this function to figure out how to specify "last 4 characters".
    

```r
scrape_pac<-function(base_url){
  # Get the year in the URL
  year<-str_sub(base_url,-4)
  # Read the html file in the URL
  webpage<-read_html(base_url)
  # Turn the html file in the URL into a dataframe
  data<-html_table(webpage)
  
  data_clean<- data[[1]] %>% 
    # Clean the column titles using snake case
    janitor::clean_names(case="snake") %>% 
    # Insert a year column using the year in the URL
    mutate(year=year)
  
  return(data_clean)
}
```


-   Define the URLs for 2022, 2020, and 2000 contributions. Then, test your function using these URLs as inputs. Does the function seem to do what you expected it to do?


```r
# URLs

url_2022<-"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022"
url_2020<-"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2020"
url_2000<-"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2000"

# Testing urls as inputs

result_2022 <- scrape_pac(url_2022)
result_2020 <- scrape_pac(url_2020)
result_2000 <- scrape_pac(url_2000)

# View the results
head(result_2022)
```

```
## # A tibble: 6 × 6
##   pac_name_affiliate             country_of_origin_pa…¹ total dems  repubs year 
##   <chr>                          <chr>                  <chr> <chr> <chr>  <chr>
## 1 Accenture (Accenture)          Ireland/Accenture plc  $3,0… $0    $3,000 2022 
## 2 Acreage Holdings               Canada/Acreage Holdin… $0    $0    $0     2022 
## 3 Air Liquide America            France/L'Air Liquide … $17,… $14,… $2,500 2022 
## 4 Airbus Group                   Netherlands/Airbus Gr… $193… $82,… $111,… 2022 
## 5 Alexion Pharmaceuticals (Astr… UK/AstraZeneca PLC     $186… $104… $82,2… 2022 
## 6 Alkermes Inc                   Ireland/Alkermes Plc   $84,… $34,… $50,0… 2022 
## # ℹ abbreviated name: ¹​country_of_origin_parent_company
```

```r
head(result_2020)
```

```
## # A tibble: 6 × 6
##   pac_name_affiliate    country_of_origin_parent_comp…¹ total dems  repubs year 
##   <chr>                 <chr>                           <chr> <chr> <chr>  <chr>
## 1 7-Eleven              Japan/Seven & I Holdings        $20,… $1,0… $19,0… 2020 
## 2 ABB Group (ABB Group) Switzerland/Asea Brown Boveri   $16,… $6,8… $10,1… 2020 
## 3 Accenture (Accenture) Ireland/Accenture plc           $83,… $50,… $33,0… 2020 
## 4 Air Liquide America   France/L'Air Liquide SA         $37,… $15,… $22,0… 2020 
## 5 Airbus Group          Netherlands/Airbus Group        $182… $79,… $103,… 2020 
## 6 Alkermes Inc          Ireland/Alkermes Plc            $94,… $30,… $64,0… 2020 
## # ℹ abbreviated name: ¹​country_of_origin_parent_company
```

```r
head(result_2000)
```

```
## # A tibble: 6 × 6
##   pac_name_affiliate        country_of_origin_parent_…¹ total dems  repubs year 
##   <chr>                     <chr>                       <chr> <chr> <chr>  <chr>
## 1 7-Eleven                  Japan/Ito-Yokado            $8,5… $1,5… $7,000 2000 
## 2 ABB Group                 Switzerland/Asea Brown Bov… $46,… $17,… $28,5… 2000 
## 3 Accenture                 UK/Accenture plc            $75,… $23,… $52,9… 2000 
## 4 ACE INA                   UK/ACE Group                $38,… $12,… $26,0… 2000 
## 5 Acuson Corp (Siemens AG)  Germany/Siemens AG          $2,0… $2,0… $0     2000 
## 6 Adtranz (DaimlerChrysler) Germany/DaimlerChrysler AG  $10,… $10,… $500   2000 
## # ℹ abbreviated name: ¹​country_of_origin_parent_company
```


-   Construct a vector called `urls` that contains the URLs for each webpage that contains information on foreign-connected PAC contributions for a given year.


```r
years<-c(2000,2020,2022)

urls<- str_c("https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/",years)
```
-   Map the `scrape_pac()` function over `urls` in a way that will result in a data frame called `contributions_all`.

```r
library(purrr)

contributions_all <- map_df(urls, scrape_pac)

print(contributions_all)
```

```
## # A tibble: 590 × 6
##    pac_name_affiliate            country_of_origin_pa…¹ total dems  repubs year 
##    <chr>                         <chr>                  <chr> <chr> <chr>  <chr>
##  1 7-Eleven                      Japan/Ito-Yokado       $8,5… $1,5… $7,000 2000 
##  2 ABB Group                     Switzerland/Asea Brow… $46,… $17,… $28,5… 2000 
##  3 Accenture                     UK/Accenture plc       $75,… $23,… $52,9… 2000 
##  4 ACE INA                       UK/ACE Group           $38,… $12,… $26,0… 2000 
##  5 Acuson Corp (Siemens AG)      Germany/Siemens AG     $2,0… $2,0… $0     2000 
##  6 Adtranz (DaimlerChrysler)     Germany/DaimlerChrysl… $10,… $10,… $500   2000 
##  7 AE Staley Manufacturing (Tat… UK/Tate & Lyle         $24,… $10,… $14,0… 2000 
##  8 AEGON USA (AEGON NV)          Netherlands/Aegon NV   $58,… $10,… $47,7… 2000 
##  9 AIM Management Group          UK/AMVESCAP            $25,… $10,… $15,0… 2000 
## 10 Air Liquide America           France/L'Air Liquide … $0    $0    $0     2000 
## # ℹ 580 more rows
## # ℹ abbreviated name: ¹​country_of_origin_parent_company
```

