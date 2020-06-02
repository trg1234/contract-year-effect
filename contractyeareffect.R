## Charles Shi, Jonathan Liu, Terry Culpepper, and Sean Choi
## Contract Year Effect in the NBA
##
## -------------------------------------------------------------------------
##
## Data
##
## The raw file can be downloaded at 
## https://github.com/Skyrior/contract-year-effect/blob/master/player_stats.csv
##
## -------------------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(janitor)
library(stringr)
library(hdm) ## for double lasso
library(ggplot2)

## -------------------------------------------------------------------------
##
## Cleaning
##
## Remember to set working directory to source file location.
##
## -------------------------------------------------------------------------

data <- read.csv("player_stats.csv", header = TRUE)
contract.year <- read.csv("contractyeardata.csv", header = TRUE)

## Removes NA

cleaned <- na.omit(data)
contract.year$Salary.Current <- as.numeric(gsub('[$]', '', contract.year$Salary.Current))
contract.year$Salary.Next <- as.numeric(gsub('[$]', '', contract.year$Salary.Next))

## Note that N/A on next year salary is still valid data! They simply
## do not have a contract signed yet.

contract.year$Salary.Next[is.na(contract.year$Salary.Next)] <- 0
cleaned.contract <- na.omit(contract.year)

## 1. Removes trailing and leading whitespace, 
## 2. Replace inner whitespaces with underscores,
## 3. Replace dots with underscores.
## 4. Removes weird chars.

cleaned <- cleaned %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(name = str_trim(name)) %>%
  mutate(name = str_replace_all(name, "[^[:alnum:] ]", "")) %>%
  mutate(name = str_replace(name, " ", "_"))

cleaned.contract <- cleaned.contract %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(player = str_trim(player)) %>%
  mutate(player = str_replace_all(player, "[^[:alnum:] ]", "")) %>%
  mutate(player = str_replace(player, " ", "_"))

## Removes duplicates

cleaned <- unique(cleaned)
cleaned.contract <- unique(cleaned.contract)

## Subsets data with no player floating between teams

cleaned.notot <- cleaned %>%
  filter(team != "TOT")

## -------------------------------------------------------------------------
##
## Merging the Dataset
##
## We proceed to merge the two datasets together.
##
## -------------------------------------------------------------------------

names(cleaned.contract)[names(cleaned.contract) == 'player'] <- 'name'
names(cleaned.contract)[names(cleaned.contract) == 'year'] <- 'season'
cleaned$season <- as.numeric(str_replace(cleaned$season, "-\\d+", ""))
cleaned$season <- cleaned$season + 1
cleaned$season <- as.factor(cleaned$season)
cleaned.contract$season <- as.factor(cleaned.contract$season)
nba <- left_join(cleaned, cleaned.contract, by = c("name", "season"))
nba <- na.omit(nba)

## Drop duplicate columns. Note: this includes duplicates due to
## different units etc.

drop.col <- c("gp", "dist_feet", "dist_miles_off", "dist_miles_def", "avg_speed_off",
          "avg_speed_def", "salary_next")
nba <- nba %>% select(-one_of(drop.col))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS
##
## -------------------------------------------------------------------------

reg <- lm(formula = AVG.SPEED ~ contract_year + MIN + 
            Player + as.factor(Year) + Salary.Current, data = nba, weights = nba$MIN)
summary(reg)


drops <- c("AVG.SPEED")
X <- as.matrix(nba[ , !(names(nba) %in% drops)])

fm = paste("AVG.SPEED ~", paste(colnames(X), collapse="+"))
fm = as.formula(fm)

lasso.effect <- rlassoEffects(X, AVG.SPEED, index=c(18))
lasso.effect <- rlassoEffects(fm, I = ~ contract_year, data = nba)
print(lasso.effect)
summary(lasso.effect)
confint(lasso.effect)
plot(lasso.effect)