### -------------------------------------------- ###
###               Data Extraction                ###
### -------------------------------------------- ###

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

### -------------------------------------------- ###
###         Exploratory Data Analysis            ###
### -------------------------------------------- ###

# Find the mean of VCR and Unemployment data from 2013-2015
risk_VCR_avg <- risk_VCR %>%
  mutate(VCR_avg = rowMeans(.[16:18], na.rm = T, dims = 1)) %>% 
  mutate(Unemp_rate_avg = rowMeans(.[13:15], na.rm = T, dims = 1))

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

# Plot the mean VCR against Uemployment rate for top and bottom 100 cities
VCR_UR <- bind_rows(top100_UR, bot100_UR)

VCR_UR_scat <- ggplot(VCR_UR, aes(x = Unemp_rate_avg, y = VCR_avg, col = rank)) +
  geom_point(size = 2, shape = 1, na.rm = T, show.legend = F) +
  geom_smooth(method = "lm", se = T, show.legend = F) +
  facet_grid(. ~ rank) +
  labs(title = "Violent Crime Rate by Unemployment Rate", 
       x = "Mean Unemployment Rate (2013-2015)", y = "Mean Violent Crime Rate (2013-2015)")

plot(VCR_UR_scat)

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

plot(VCR_GR_scat)

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

plot(VCR_hist)
plot(GR_hist)
plot(UR_hist)

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

plot(VCR_den)
plot(GR_den)
plot(UR_den)

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

matrix_1_LV <- subset(matrix_1, subset = rank == "Least_Violent")
summary(matrix_1_LV)
                      
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

print(comparison_plot)


### -------------------------------------------- ###
###           Models and Predictions             ###
### -------------------------------------------- ###

# Separate by rank variable into 2 separate data frames to create 2 separate linear regressions for 
# comparison purposes
MV_df <- filter(matrix_2, rank == "Most_Violent")

LV_df <- filter(matrix_2, rank == "Least_Violent")

## --------------------------------------------
## Stepwise Mulitple Linear Regression Feature Selection (Most Violent Series)
## --------------------------------------------

# Regession using all predictors available
lm_MV_VCR_1 <- lm(VCR_avg ~ Unemp_rate_avg + Grad_rate_avg + Median_income_avg + Median_debt_avg 
                 + Retention_rate_avg + Cost_avg,
                 data = MV_df)

print(summary(lm_MV_VCR_1))

# Reduce the model by removing least correlated predictor, the avgerage cost of colleges
lm_MV_VCR_2 <- lm(VCR_avg ~ Unemp_rate_avg + Grad_rate_avg + Median_income_avg + Median_debt_avg 
                 + Retention_rate_avg,
                 data = MV_df)

print(summary(lm_MV_VCR_2))

# Remove median income due to high multicolinearty with median debt
lm_MV_VCR_3 <- lm(VCR_avg ~ Unemp_rate_avg + Grad_rate_avg + Median_debt_avg + Retention_rate_avg,
                 data = MV_df)

print(summary(lm_MV_VCR_3))

# Repalce median debt with median income and compare to model 3
lm_MV_VCR_4 <- lm(VCR_avg ~ Unemp_rate_avg + Grad_rate_avg + Median_income_avg + Retention_rate_avg,
                 data = MV_df)

print(summary(lm_MV_VCR_4))

# Debt is more stat. sig., reduce model 3 by removing retention rate of college 
# students due to multicolinearity with graduation rate 
lm_MV_VCR_5 <- lm(VCR_avg ~ Unemp_rate_avg + Grad_rate_avg + Median_debt_avg,
                 data = MV_df)

print(summary(lm_MV_VCR_5))

## --------------------------------------------
## Stepwise Multiple Linear Regression Feature Selection (Least Violent Series)
## --------------------------------------------

# Regression using all predictors available
lm_LV_VCR_1 <- lm(VCR_avg ~ Unemp_rate_avg + Grad_rate_avg + Median_income_avg + Median_debt_avg 
                 + Retention_rate_avg + Cost_avg,
                 data = LV_df)

print(summary(lm_LV_VCR_1))

# Reduce by removing least signifanct predictor, grauation rate of college students
lm_LV_VCR_2 <- lm(VCR_avg ~ Unemp_rate_avg + Median_income_avg + Median_debt_avg 
                 + Retention_rate_avg + Cost_avg,
                 data = LV_df)

print(summary(lm_LV_VCR_2))

# Remove next least significant predictor, unemployment rate
lm_LV_VCR_3 <- lm(VCR_avg ~ Median_income_avg + Median_debt_avg + Retention_rate_avg + Cost_avg,
                 data = LV_df)

print(summary(lm_LV_VCR_3))

# Remove median income due to high multicolinearity with median debt
lm_LV_VCR_4 <- lm(VCR_avg ~ Median_debt_avg + Retention_rate_avg + Cost_avg,
                 data = LV_df)

print(summary(lm_LV_VCR_4))

## --------------------------------------------
## Best Subsets Regression Feature Selection
## --------------------------------------------

# Use Best subsets regression to plot adjusted R2 of all variables
MV_df_na <- MV_df[complete.cases(MV_df),]
MV_df_na <- MV_df_na[c(4:6, 8:11)]

LV_df_na <- LV_df[complete.cases(LV_df),]
LV_df_na <- LV_df_na[c(4:6, 8:11)]

best_lm_MV_VCR <- regsubsets(VCR_avg ~ ., data = MV_df_na)

best_lm_LV_VCR <- regsubsets(VCR_avg ~ ., data = LV_df_na)

plot(best_lm_MV_VCR, scale = "adjr2")

plot(best_lm_LV_VCR, scale = "adjr2")

# Predicted R2 for all models using Tom Hopper's function
pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

MV_pred_names <- c("MV_Model_1", "MV_Model_2", "MV_Model_3", "MV_Model_4", "MV_Model_5")

MV_pred_r2_results <- c(pred_r_squared(lm_MV_VCR_1),
                        pred_r_squared(lm_MV_VCR_2),
                        pred_r_squared(lm_MV_VCR_3),
                        pred_r_squared(lm_MV_VCR_4),
                        pred_r_squared(lm_MV_VCR_5))

MV_model_results <- data.frame(MV_pred_names, MV_pred_r2_results)

LV_pred_names <- c("LV_Model_1", "LV_Model_2", "LV_Model_3", "LV_Model_4")

LV_pred_r2_results <- c(pred_r_squared(lm_LV_VCR_1),
                        pred_r_squared(lm_LV_VCR_2),
                        pred_r_squared(lm_LV_VCR_3),
                        pred_r_squared(lm_LV_VCR_4)) 

LV_model_results <- data.frame(LV_pred_names, LV_pred_r2_results)

## --------------------------------------------
## Cross Validation of Linear Models
## --------------------------------------------

# Train and test linear regressions via a repeated k-fold cross validation
tcont <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

cv_lm_MV_VCR_5 <- train(VCR_avg ~ Unemp_rate_avg + Grad_rate_avg + Median_debt_avg,
                        data = na.omit(MV_df),
                        trControl = tcont,
                        method = "lm")

print(cv_lm_MV_VCR_5)

cv_lm_LV_VCR_3 <- train(VCR_avg ~ Median_debt_avg + Retention_rate_avg + Cost_avg + Median_income_avg,
                        data = na.omit(LV_df),
                        trControl = tcont,
                        method = "lm")

print(cv_lm_LV_VCR_3)

# Calculate RMSE for comparison against k-fold cross validation results of final models 
# (highest F statistic, lowest p-value)
RMSE_lm_MV_VCR_5 <- sqrt(sum((residuals(lm_MV_VCR_5)/(1-hatvalues(lm_MV_VCR_5)))^2)/length(lm_MV_VCR_5$residuals))

print(RMSE_lm_MV_VCR_5)

RMSE_lm_LV_VCR_3 <- sqrt(sum((residuals(lm_LV_VCR_3)/(1-hatvalues(lm_LV_VCR_3)))^2)/length(lm_LV_VCR_3$residuals))

print(RMSE_lm_LV_VCR_3)


### -------------------------------------------- ###
###               Data Visualization             ###
### -------------------------------------------- ###

# Table of correlation matrix
sjt.corr(MV_df_na, title = "Correlation Matrix of Variables (MV data set)")

sjt.corr(LV_df_na, title = "Correlation Matrix of Variables (LV data set)")

# Table of regression model coefficients
sjt.lm(lm_MV_VCR_5, lm_MV_VCR_4, lm_MV_VCR_3, lm_MV_VCR_2, lm_MV_VCR_1,
       group.pred = F,
       p.numeric = F,
       emph.p = T,
       show.ci = F,
       show.se = T,
       show.header = T,
       show.fstat = T,
       string.dv = "Model Coefficients to predict VCR for Most Violent Cities") 

sjt.lm(lm_LV_VCR_4, lm_LV_VCR_3, lm_LV_VCR_2, lm_LV_VCR_1,
       group.pred = F,
       p.numeric = F,
       emph.p = T,
       show.ci = F,
       show.se = T,
       show.header = T,
       show.fstat = T,
       string.dv = "Model Coefficients to predict VCR for Least Violent Cities") 

# List of Predicted R2 for model comparison
MV_model_results <- data.frame(MV_pred_names, MV_pred_r2_results)

LV_model_results <- data.frame(LV_pred_names, LV_pred_r2_results)

# Trendlines of regression predictors
sjp.lm(lm_MV_VCR_5, 
       type = "eff", 
       facet.grid = T, 
       title = "Most Violent Model Predictor Trends")

sjp.lm(lm_LV_VCR_3, 
       type = "eff", 
       facet.grid = T,
       title = "Least Violent Model Predictor Trends")

# Effects of regression predictors
sjp.lm(lm_MV_VCR_5, 
       type = "lm", 
       show.values = T, 
       show.p = T,
       title = "Most Violent Model Predictor Effects",
       axis.title = "VCR_avg Estimates")

sjp.lm(lm_LV_VCR_3,
       type = "lm", 
       show.values = T, 
       show.p = T,
       title = "Least Violent Model Predictor Effects",
       axis.title = "VCR_avg Estimates")

# Residual plots to visualize randomness of data
plot_model(lm_MV_VCR_5, type = c("diag"))


plot_model(lm_LV_VCR_3, type = c("diag"))

