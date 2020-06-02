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

## Removes NA

cleaned <- na.omit(data)

## Removes duplicates

cleaned <- unique(cleaned)

## 1. Removes trailing and leading whitespace, 
## 2. Replace inner whitespaces with underscores,
## 3. Replace dots with underscores.

cleaned <- cleaned %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(name = str_trim(name)) %>%
  mutate(name = str_replace(name, " ", "_"))

## Subsets data with no player floating between teams

cleaned.notot <- cleaned %>%
  filter(team != "TOT")



names(nba)<-make.names(names(nba), unique=TRUE)

reg <- lm(formula = AVG.SPEED ~ contract_year + MIN + 
            Player + as.factor(Year) + Salary.Current, data = nba, weights = nba$MIN)
summary(reg)


library(hdm)
library(ggplot2)

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