---
title: "Capstone - Data Story"
author: "Andre Velez"
output:
  html_document: 
    keep_md: true
---
# __Predicting The Effects Of Unemployment And Education On Violent Crime Rate__

### __Introduction__

Violent crime is one of the most concerning topics of discussion in society; it can lead to the deterioration of a community's health, prosperity, and economic mobility.  Historical data has shown as a city's violent crime rate increases more financially secure families will move away towards a safer location, taking economic strength and mobility from these cities with them.  This leaves those left behind, typically less wealthy and less educated, falling into economic hardship and into a city with a higher crime rate.

### __Problem__

The families that have the privilege to leave a city with increasing crime rate for "greener" pastures include the business owners and job creators. This leaves those left behind, typically the less wealthy and less educated, falling into economic hardship and into a city with a higher crime rate.  

Violent crime rate has been anecdotally shown to be closely tied to a city's economy, and thus its policies.  Policy makers and businesses who contribute to community wellness need to be informed of this correlation with a data driven plan to specifically tailor their limited resources and target the factors that can reduce a city's violent crime rate.  To further convey the impact these factors have on violent crime, comparisons will be made between the top 100 most violent and the top 100 least violent cities.

The goal of this project is to statistically determine the factors that affect violent crime rate, what is the significance are those effects, and to compare them between the most violent and least violent cities.  Models will then be built upon these results to predict the change in violent crime rate better when investing resources into the targeted significant factors.

### __Data Set__

This project uses violent crime rate as the response/dependent variable for our models, so publically available crime data sets were collected from the FBI's website from the years 2013 - 2015.  These data sets contained the city and their respective violent crime total count.  A data set containing population totals for U.S. cities was merged with the crime data set to calculate the violent crime rate variable.  Specifically, violent crime rate (VCR) was calculated by per capita, therefore only cities with >100,000 were used in the data sets to remove outlier high crime/low population cities to create more normalized regression models.

The predictors/independent variables used in this project are as follows:

* UR = Unemployment Rate
* GR = College Graduation Rate
* Median Income
* Median Debt
* Retention Rate = College Retention Rate
* Cost = Cost of College

Data is available from the following links:

"U.S. Unemployment Rate by County, 1990-2016" (U.S. Bureau of Labor Statistics)
https://www.kaggle.com/jayrav13/unemployment-by-county-us

"FBI Uniform Crime Report" (FBI)
https://ucr.fbi.gov/crime-in-the-u.s/2016/preliminary-semiannual-uniform-crime-report-januaryjune-2016

"College Scorecard Data" (U.S. Department of Education)
https://collegescorecard.ed.gov/data/

### __Data Limitations__

Violent crime rate is not a simple analytical variable; it is neither based upon financial fundamentals nor physical mathematics.  It is a societal and human construct.  Crime is a result of breaking current law, and law is decided by a societies own moral compass and governmental structure.  All these factors not only differ from one county to the next, but also from city to city.  To expect to discover a highly accurate model that can predict the crime rate for a nation and account for every societal, personal, economic, and global effect is beyond this project, and most likely beyond present data science methodology.

The raw data sets collected for crime, unemployment, and college data contain many missing values, as well as self-reported numbers.  The FBI doesn't have the manpower nor systems to log every crime that occurs across the nation, therefore there will be inherit error contained in these data sets.  The same limitation applies to the U.S. Bureau of Labor Statistics and its ability to collect unemployment data, as well as the college data from the Department of Education.

This project is not looking to address every variable that effects violent crime rate and have a best fitting model with an R2 of >95%.  The scope of this project is to evaluate the effects education and unemployment can have upon violent crime rate, and to compare the differences of significant factors between the most violent and least violent cities.

### __Data Extraction__


```r
# Load needed packages
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(magrittr)
library(stringi)
library(GGally)
library(lme4)
library(ggplot2)
library(caret)
library(sjPlot)
library(sjmisc)
library(leaps)
library(gridExtra)

# Import raw data as objects, remove rows w/o observations
crime_13_raw <- read_excel("Raw Data/2013 Crime Data/Table_8_Offenses_Known_to_Law_Enforcement_by_State_by_City_2013.xls", skip = 2)
crime_14_raw <- read_excel("Raw Data/2014 Crime Data/Table_8_Offenses_Known_to_Law_Enforcement_by_State_by_City_2014.xls", skip = 2)
crime_15_raw <- read_excel("Raw Data/2015 Crime Data/Table_8_Offenses_Known_to_Law_Enforcement_by_State_by_City_2015.xls", skip = 2)
college_score_raw <- read_csv("Raw Data/College Score Data/college_score_raw.csv")
unemployment_raw <- read_csv("Raw Data/Unemployment Data/unemployment_raw.csv")
population_raw <- read_csv("Raw Data/City Population/sub-est2016_all.csv")
city_county_raw <- read_delim("Raw Data/City by County/us_cities_states_counties.csv", delim = "|")
```

The data sets examined were formatted very poorly for data analysis in their raw forms.  The crime data sets were especially difficult to arrange into a coherent and easily accessible data frame structure, and many custom wrangling parameters had to be implemented.  A thorough stepwise review of the code used in this project can be located in the "Capstone Final Code" markdown document. 

After importing all the raw data into R, many of the rows in the data were design choices for a custom excel layout and were removed to get into a tidy format (all columns are variables, all rows are observations) for easier analysis downstream.  Columns also had to be renamed for clarity due to ambiguous names in the excel document i.e. X1, X2, etc. Missing values and correct data types of variables were assigned to be able to conduct EDA, modeling, and data visualization. The crime data was simplified into a data frame with only the necessary response variables and descriptors necessary (violent crime rate and city) and iterated for each year examined.


```r
## --------------------------------------------
## Crime Data
## --------------------------------------------

# Rename header and remove nonsense rows 
crime_13_detailed <- crime_13_raw %>%
  select(., 1:14) %>%
  set_colnames(.[1,]) %>% 
  slice(2:9293) %>%
  
  # Assign the correct data type
  mutate_at(., 1:2, funs(as.character)) %>%
  mutate_at(., 3:14, funs(as.integer)) %>% 
  
  # Replace and Fill missing State values  
  fill(State, .direction = c("down")) %>% 
  
  # Sum crime data into one variable
  mutate(Total_crime_13 = rowSums(.[4:14], na.rm = T, dims = 1))

# Tidy format, clean up names
crime_13_tidy <- crime_13_detailed %>% 
  select(., 1:2, 4, 15) %>% 
  mutate(City = gsub("\\d.*", "", .$City)) %>% 
  mutate(State = stri_trans_totitle(State))

names(crime_13_tidy)[3] <- c("Violent_crime_13")

## --------------------------------------------
## Iterate for 2014 and 2015 crime data
## --------------------------------------------

# Tidy 2014 Crime Data 
crime_14_detailed <- crime_14_raw %>%
  select(., 1:14) %>%
  set_colnames(.[1,]) %>% 
  slice(2:9348) %>% 
  mutate_at(., 1:2, funs(as.character)) %>%
  mutate_at(., 3:14, funs(as.integer)) %>% 
  fill(State, .direction = c("down")) %>% 
  mutate(Total_crime_14 = rowSums(.[4:14], na.rm = T, dims = 1))

crime_14_tidy <- crime_14_detailed %>% 
  select(., 1:2, 4, 15) %>% 
  mutate(City = gsub("\\d.*", "", .$City)) %>% 
  mutate(State = stri_trans_totitle(State))

names(crime_14_tidy)[3] <- c("Violent_crime_14")

# Tidy 2015 Crime Data 
crime_15_detailed <- crime_15_raw %>%
  select(., 1:14) %>%
  set_colnames(.[1,]) %>% 
  slice(2:9396) %>% 
  mutate_at(., 1:2, funs(as.character)) %>%
  mutate_at(., 3:14, funs(as.integer)) %>% 
  fill(State, .direction = c("down")) %>% 
  mutate(Total_crime_15 = rowSums(.[4:14], na.rm = T, dims = 1))

crime_15_tidy <- crime_15_detailed %>% 
  select(., 1:2, 4, 15) %>% 
  mutate(City = gsub("\\d.*", "", .$City)) %>% 
  mutate(State = stri_trans_totitle(State))

names(crime_15_tidy)[3] <- c("Violent_crime_15")
```


A data set of the counties to which U.S. cities belonged was imported and formatted because the unemployment data set used only recorded the unemployment rate by county, not city.  The column containing state abbreviations and city alias was removed because the unemployment county data was formatted by full state name and the crime data was formatted by official city name.  A distinct() function was passed through to remove duplicate data entries.
  

```r
## --------------------------------------------
## City and County Data
## --------------------------------------------

# Remove unnecessary variables / observations, and place into tidy format 
city_county_tidy <- city_county_raw %>% 
  select(., 1, 3:4) %>% 
  rename(., State = "State full") %>% 
  distinct(.) %>%
  mutate_at(., 1:3, funs(as.character)) %>% 
  mutate(County = stri_trans_totitle(County))
```

The raw unemployment data was organized by individual entries of the unemployment rate for each county for every year from 1990 to 2016. The spread() function was used to get the data into a tidy format where each observation was the county, rather than the unemployment rate record.  The unemployment rate data from 2013 to 2015 was filtered and renamed for each county to match and merge with the crime and college data sets.


```r
## --------------------------------------------
## Unemployment Data
## --------------------------------------------

# Remove county names
unemployment_tidy <- unemployment_raw %>% 
  mutate(County = gsub("\\sCounty.*", "", .$County)) %>% 
  
  # Spread data to determine annual rate, tidy up 
  spread(., Month, Rate) %>% 
  mutate(Annual_rate = rowMeans(.[4:15], na.rm = T, dims = 1)) %>% 
  filter(., Year > 2012 & Year < 2016) %>% 
  select(., 1:3, 16) %>% 
  spread(., Year, Annual_rate) %>% 
  rename(., Unemp_rate_13 = "2013", Unemp_rate_14 = "2014", Unemp_rate_15 = "2015")
```

A number of unused variables were given in the college score data set and are available for observation in the link provided above.  Only the variables deemed impactful to this project were selected for further analysis i.e. Graduation rate, Average cost of tuition, etc.  Values for each variable were renamed for clarity.


```r
## --------------------------------------------
## College Score Data
## --------------------------------------------

# Remove unnecessary variables
college_score_tidy <- college_score_raw %>% 
  select(., 4:5, 7, 10:12, 85, 97:99, 110:111, 115, 117:119, 123) %>% 
  
  # Rename varibles for readability
  rename(., Uni = INSTNM, 
         City = CITY, 
         State = STATE, 
         Under_Inv = HCM2, 
         Degree_Level = PREDDEG, 
         Type = CONTROL, 
         Online = DISTANCEONLY, 
         Certified = CURROPER,
         Avg_Price_Pub = NPT4_PUB,
         Avg_Price_Priv = NPT4_PRIV,
         Pell_Grant_Rate = PCTPELL,
         Retention_Rate = RET_FT4,
         Fed_Loan_Rate = PCTFLOAN,
         Median_Earnings_10years = MD_EARN_WNE_P10,
         Over_25K_6years = GT_25K_P6,
         Median_Debt_Grads = GRAD_DEBT_MDN_SUPP,
         Grad_Rate = C150_4_POOLED_SUPP) %>%
  
  mutate_at(., 9:17, funs(as.numeric))
```


Population data for each city was needed in order to filter down the violent crime data set and calculated the violent crime rate per capita (VCR). Renaming of the selected variables was executed for clarity. Multiple custom gsub() functions were needed because many of the observations for the "City" variable had unnecessary descriptors i.e. Bakerstown town, New York City city, etc.


```r
## --------------------------------------------
## Population Data
## --------------------------------------------

# Remove unnecessary variables
population_tidy <- population_raw %>% 
  select(., 9:10, 16:18) %>% 
  mutate_at(., 1:2, funs(as.character)) %>% 
  
# Tidy variable and observation names
  rename(., City = NAME, 
         State = STNAME, 
         pop_13 = POPESTIMATE2013,
         pop_14 = POPESTIMATE2014,
         pop_15 = POPESTIMATE2015) %>% 
  mutate(City = gsub("\\scity", "", .$City)) %>% 
  mutate(City = gsub("\\stown", "", .$City)) %>% 
  mutate(City = gsub("\\sborough", "", .$City)) %>% 
  mutate(City = gsub("\\svillage", "", .$City)) %>% 
  mutate(City = gsub("\\sship", "", .$City)) %>% 
  mutate(City = gsub(".*\\(pt.)", "", .$City)) %>% 
  distinct(.)
```

Finally, the individual tidy data frames were merged into one data frame via joins by the City and State variables.  This ensured no duplicate values were introduced into the data and our analysis between the top 100 most violent and least violent cities and their respective predictors would be as accurate as possible.  An ifelse statement was used to calculated the response/dependent variable VCR by dividing the violent crime total by the population total and multiplying it by 100 for cities with a minimum population of 100,000.


```r
## --------------------------------------------
## Merged Tidy Dataframe
## --------------------------------------------

crime_1 <- left_join(crime_13_tidy, crime_14_tidy, by = c("State", "City"))

crime_total <- left_join(crime_1, crime_15_tidy, by = c("State", "City"))

crime_pop <- left_join(crime_total, population_tidy, by = c("State", "City"))

crime_county <- left_join(crime_pop, city_county_tidy, by = c("State", "City"))

risk_tidy <- left_join(crime_county, unemployment_tidy, by = c("State", "County"))

# Violent crime rate(VCR) per capita of cities > or = to 100,000 population size
risk_VCR <- risk_tidy %>% 
  mutate(VCR_13 = ifelse(.$pop_13 < 100000, NA, (.$Violent_crime_13 / .$pop_13 * 100))) %>% 
  mutate(VCR_14 = ifelse(.$pop_14 < 100000, NA, (.$Violent_crime_14 / .$pop_14 * 100))) %>% 
  mutate(VCR_15 = ifelse(.$pop_15 < 100000, NA, (.$Violent_crime_15 / .$pop_15 * 100))) %>% 
  drop_na(.)

# Separate expanded df to incorporate college data
risk_VCR_final <- left_join(risk_VCR, college_score_tidy, by = c("State", "City"))
```

### __Exploratory Data Analysis__

EDA is used to identify potential trends in the data and determine which variables appear to have significance in regards to the problem being asked.  In this case, we are looking for independent variables that have statically significant correlation to VCR, and interesting trends in sample distribution when comparing the most violent and least violent cities.

There was not enough yearly data for a time series analysis, so the mean of the 2013-2015 data was taken for VCR, Unemployment Rate (UR), and Graduation Rate (GR).


```r
# Find the mean of VCR and Unemployment data from 2013-2015
risk_VCR_avg <- risk_VCR %>%
  mutate(VCR_avg = rowMeans(.[16:18], na.rm = T, dims = 1)) %>% 
  mutate(Unemp_rate_avg = rowMeans(.[13:15], na.rm = T, dims = 1))
```

A new categorical variable "rank" was created to identify and split the data into the top 100 most violent(MV) and least violent(LV) cities by VCR.  Comparisons can now be made between these two data sets' statically analysis, including the regression models.


```r
# Creat a new variable to facet plot between top and bottom 100 cities
top100_UR <- risk_VCR_avg %>%
  arrange(., desc(VCR_avg)) %>% 
  distinct(., VCR_avg, .keep_all = T) %>% 
  top_n(., 100, VCR_avg) %>% 
  mutate(rank = "Most_Violent")

bot100_UR <- risk_VCR_avg %>%
  arrange(., desc(VCR_avg)) %>% 
  distinct(., VCR_avg, .keep_all = T) %>% 
  top_n(., -100, VCR_avg) %>% 
  mutate(rank = "Least_Violent")
```

An initial comparison of the data to VCR was only performed on GR and UR based upon our initial hypothesis that the raising of unemployment and lowering college graduation rate would alter the response of the violent crime rate in a city. 

Scatterplots were used to visualize VCR vs. GR and VCR vs. UR.  The results showed a significant difference between the MV cities and the LV cities.  Both GR and UR seem to have no statistical significance in LV cities.  The horizontal trend line indicates that the variance in data is random.  However, high VCR cities appear to have a slightly positive correlation with UR, and a slightly negative correlation with GR.  The error appears large in both plots, and will be investigated more thoroughly with a summary analysis of the regression models.


```r
# Plot the mean VCR against Uemployment rate for top and bottom 100 cities
VCR_UR <- bind_rows(top100_UR, bot100_UR)

VCR_UR_scat <- ggplot(VCR_UR, aes(x = Unemp_rate_avg, y = VCR_avg, col = rank)) +
  geom_point(size = 2, shape = 1, na.rm = T, show.legend = F) +
  geom_smooth(method = "lm", se = T, show.legend = F) +
  facet_grid(. ~ rank) +
  labs(title = "Violent Crime Rate by Unemployment Rate", 
       x = "Mean Unemployment Rate (2013-2015)", y = "Mean Violent Crime Rate (2013-2015)")
```

![](Data_Story_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
# Find avg grad rate by city
GR_by_city <- risk_VCR_final %>% 
  group_by(., City, State) %>% 
  summarise(Grad_rate_avg = mean(Grad_Rate, na.rm = T))

risk_VCR_avg_GR <- left_join(risk_VCR_avg, GR_by_city, by = c("State", "City"))

# Plot mean VCR against college grad rate
top100_GR <- risk_VCR_avg_GR %>% 
  arrange(., desc(VCR_avg)) %>% 
  distinct(., VCR_avg, .keep_all = T) %>% 
  top_n(., 100, VCR_avg) %>% 
  mutate(rank = "Most_Violent")

bot100_GR <- risk_VCR_avg_GR %>%
  arrange(., desc(VCR_avg)) %>% 
  distinct(., VCR_avg, .keep_all = T) %>% 
  top_n(., -100, VCR_avg) %>% 
  mutate(rank = "Least_Violent")

VCR_GR <- bind_rows(top100_GR, bot100_GR)

VCR_GR_scat <- ggplot(VCR_GR, aes(x = Grad_rate_avg, y = VCR_avg, col = rank)) +
  geom_point(size = 2, shape = 1, na.rm = T, show.legend = F) +
  geom_smooth(method = "lm", se = T, show.legend = F) +
  facet_grid(. ~ rank) +
  labs(title = "Violent Crime Rate by Graduation Rate", 
       x = "Mean Graduation Rate (2013-2015)", y = "Mean Violent Crime Rate (2013-2015)")
```

![](Data_Story_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Histograms were created to evaluate the distribution differences between all 3 variables.  VCR reveals that the range for the LV cities is much smaller than the right tailed distribution of the MV cities.  This indicates there are more extreme examples of higher VCR than lower VCR. The GR histograms don't provide much detail in visualizing the differences between the two sets of data.  There appears to be a tighter concentration of GR on the lower end for more violent cities that will be examined closer.  The UR histograms reveal more violent cities have a higher distribution of unemployment.  All these factors seem to coincide with the hypothesis that unemployment rate has a positive correlation with violent crime rate, whereas graduation rate has a negative correlation.


```r
# Plot VCR, GR, and UR distributions for top and bot 100 VCR cities
VCR_hist <- ggplot(VCR_GR, aes(x = VCR_avg, fill = rank)) +
  geom_histogram(binwidth = 0.04, alpha = 0.6, na.rm = T) +
  labs(title = "Violent Crime Rate Sample Distribution", 
       x = "Mean Violent Crime Rate (2013-2015)", y = "# of Cities") +
  guides(fill=guide_legend(title = "Top 100 Cities"))

GR_hist <- ggplot(VCR_GR, aes(x = Grad_rate_avg, fill = rank)) +
  geom_histogram(binwidth = 0.02, alpha = 0.6, show.legend = F, na.rm = T) +
  facet_grid(. ~ rank) +
  labs(title = "Graduation Rate Sample Distribution", 
       x = "Mean Graduation Rate (2013-2015)", y = "# of Cities")

UR_hist <- ggplot(VCR_UR, aes(x = Unemp_rate_avg, fill = rank)) +
  geom_histogram(binwidth = 0.25, alpha = 0.6, show.legend = F, na.rm = T) +
  facet_grid(. ~ rank) +
  labs(title = "Unemployment Rate Sample Distribution", 
       x = "Mean Unemployment Rate (2013-2015)", y = "# of Cities")
```

![](Data_Story_files/figure-html/unnamed-chunk-15-1.png)<!-- -->![](Data_Story_files/figure-html/unnamed-chunk-15-2.png)<!-- -->![](Data_Story_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

Density plots were evaluated to easily compare the distributions on a relative scale rather than a direct count.  The histograms the VCR for the LV cities is tightly concentrated from approximately 0 to 0.5%, and the MV cities ranges from approximately 0.5% to 2%.  The GR density distributions appear to show the central tendency of the MV cities is lower and more tightly concentrated between the interquartile range, whereas the GR for the LV cities appears to be a normal distribution.  The UR density distributions were similar between the sets, with the MV central tendency to be slightly higher, however interestingly both distributions are bimodal.  This characteristic could be the result of a number of unforeseen factors, i.e. the data set isn't large enough. This effect is beyond the scope of this study and will not be further investigated.


```r
# Plot overlapping densities for VCR, GR, and UR
VCR_den <- ggplot(VCR_GR, aes(x = VCR_avg, fill = rank)) +
  geom_density(na.rm = T, alpha = 0.6) +
  labs(title = "Violent Crime Rate Density Comparison", 
       x = "Mean Violent Crime Rate (2013-2015)", y = "Density") +
  guides(fill=guide_legend(title = "Top 100 Cities"))

GR_den <- ggplot(VCR_GR, aes(x = Grad_rate_avg, fill = rank)) +
  geom_density(na.rm = T, alpha = 0.6) +
  labs(title = "Graduation Rate Density Comparison", 
       x = "Mean Graduation Rate (2013-2015)", y = "Density") +
  guides(fill=guide_legend(title = "Top 100 Cities"))

UR_den <- ggplot(VCR_UR, aes(x = Unemp_rate_avg, fill = rank)) +
  geom_density(na.rm = T, alpha = 0.6) +
  labs(title = "Unemployment Rate Density Comparison", 
       x = "Mean Unemployment Rate (2013-2015)", y = "Density") +
  guides(fill=guide_legend(title = "Top 100 Cities"))
```

![](Data_Story_files/figure-html/unnamed-chunk-17-1.png)<!-- -->![](Data_Story_files/figure-html/unnamed-chunk-17-2.png)<!-- -->![](Data_Story_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

A matrix of the factors in question was created to evaluate common statistical methods and create a correlation matrix of all variables used for this study.  The central tendency statistical methods evaluated were the mean, range, and quantile coefficients.  

Because the data sets were ranked by violent crime, the differences in VCR were expected. The mean of MV_VCR was ~0.84%, compared to the mean of the LV_VCR at ~0.23%.  The interquartile ranges follow the distributions previously examined. The significant value lies in the differences in range.  The LV_VCR is from ~0.05% to ~0.38% (a 0.33% total range), and the MV_VCR is from ~0.47% to ~2.1% (a 1.63% total range).  This shows a 5x difference in the extremes when comparing the two sets for VCR.  The GR statistics reveal the expected left shift to lower values of the MV set compared to the LV set.  The interquartile range does not show a significant difference in the distributions of the data as hypothesized from the density distribution. The MV_GR set was between ~39% and ~55%, and the LV_GR set from ~43% to ~60%. The UR statistical values confirm the right shift of the MV set towards higher values when compared to the LV set.  For example, the mean of the MV_UR is ~6.5% and the mean of the LV_UR is ~5.9%.


```r
# Group city and state to find averages of college data
risk_VCR_final_grouped <- risk_VCR_final %>% 
  mutate(Cost_total = rowMeans(.[25:26], na.rm = T, dims = 1)) %>% 
  group_by(., City, State) %>% 
  summarise(., "Median_income_avg" = mean(Median_Earnings_10years, na.rm = T),
            "Median_debt_avg" = mean(Median_Debt_Grads, na.rm = T),
            "Retention_rate_avg" = mean(Retention_Rate, na.rm = T),
            "Cost_avg" = mean(Cost_total, na.rm = T))

# Retain only averages of data from data exploration plots
matrix_1 <- VCR_GR %>% 
  select(., 1:2, 12, 19:22)

matrix_2 <- left_join(matrix_1, risk_VCR_final_grouped, by = c("State", "City"))
matrix_2 <- as_data_frame(matrix_2) %>% 
  mutate_at(., 1:3, funs(as.factor)) %>% 
  mutate_at(., 7, funs(as.factor))

# Central Tendancy comparisons of VCR, UR, and GR by rank
matrix_1_MV <- subset(matrix_1, subset = rank == "Most_Violent")
summary(matrix_1_MV)
```

```
##     State               City              County             VCR_avg      
##  Length:100         Length:100         Length:100         Min.   :0.4689  
##  Class :character   Class :character   Class :character   1st Qu.:0.5797  
##  Mode  :character   Mode  :character   Mode  :character   Median :0.7540  
##                                                           Mean   :0.8354  
##                                                           3rd Qu.:0.9674  
##                                                           Max.   :2.0999  
##                                                                           
##  Unemp_rate_avg   Grad_rate_avg        rank          
##  Min.   : 3.028   Min.   :0.2203   Length:100        
##  1st Qu.: 5.332   1st Qu.:0.3927   Class :character  
##  Median : 6.250   Median :0.4816   Mode  :character  
##  Mean   : 6.518   Mean   :0.4791                     
##  3rd Qu.: 7.747   3rd Qu.:0.5474                     
##  Max.   :11.667   Max.   :0.8515                     
##                   NA's   :12
```

```r
matrix_1_LV <- subset(matrix_1, subset = rank == "Least_Violent")
summary(matrix_1_LV)
```

```
##     State               City              County         
##  Length:100         Length:100         Length:100        
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##     VCR_avg        Unemp_rate_avg  Grad_rate_avg        rank          
##  Min.   :0.05086   Min.   :2.467   Min.   :0.1715   Length:100        
##  1st Qu.:0.15056   1st Qu.:4.763   1st Qu.:0.4299   Class :character  
##  Median :0.23140   Median :5.787   Median :0.5106   Mode  :character  
##  Mean   :0.23266   Mean   :5.929   Mean   :0.5221                     
##  3rd Qu.:0.30786   3rd Qu.:7.168   3rd Qu.:0.6000                     
##  Max.   :0.38591   Max.   :8.450   Max.   :0.9159                     
##                                    NA's   :42
```

```r
# Plot matrix data comparing all variables
comparison_plot <- ggpairs(matrix_2, 
                           columns = c(4:6, 8:11),
                           title = "100 Most Violent Cities vs 100 Least Violent Cities",
                           upper = list(
                             continuous = "cor",
                             mapping = aes(color = rank, alpha = 0.6)
                           ),
                           lower = list(
                             continuous = "smooth",
                             mapping = aes(color = rank, alpha = 0.6)
                           ),
                           diag = list(
                             continuous = "densityDiag",
                             mapping = aes(color = rank, alpha = 0.6)
                           )
)
```

![](Data_Story_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
