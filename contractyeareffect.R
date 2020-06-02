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
## 
## Cleaning



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