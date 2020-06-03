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
## Suppress LM summary
##
## https://stackoverflow.com/questions/35388010/hide-some-coefficients-in-regression-summary-while-still-returning-call-r-squar
##
## -------------------------------------------------------------------------

my.summary.lm = function (x, digits = max(3L, getOption("digits") - 3L), 
                          symbolic.cor = x$symbolic.cor, 
                          signif.stars = getOption("show.signif.stars"), 
                          my.rows, ...)                     # NOTE NEW my.rows ARGUMENT
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  cat(if (!is.null(x$weights) && diff(range(x$weights))) 
    "Weighted ", "Residuals:\n", sep = "")
  if (rdf > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- if (length(dim(resid)) == 2L) 
      structure(apply(t(resid), 1L, quantile), dimnames = list(nam, 
                                                               dimnames(resid)[[2L]]))
    else {
      zz <- zapsmall(quantile(resid), digits + 1L)
      structure(zz, names = nam)
    }
    print(rq, digits = digits, ...)
  }
  else if (rdf > 0L) {
    print(resid, digits = digits, ...)
  }
  else {
    cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    if (nsingular <- df[3L] - df[1L]) 
      cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
          sep = "")
    else cat("\nCoefficients:\n")
    coefs <- x$coefficients[my.rows,]                      # SUBSET my.rows
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, 
                                                              colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
                 na.print = "NA", ...)
  }
  cat("\nResidual standard error:", format(signif(x$sigma, 
                                                  digits)), "on", rdf, "degrees of freedom")
  cat("\n")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
                                           digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
                                                                                       digits = digits), "on", x$fstatistic[2L], "and", 
        x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                                                          x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                                                       digits = digits))
    cat("\n")
  }
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl, 2), nsmall = 2, 
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}

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

## Export our current dataframe as CSV.

write.csv(nba, file = "nba.csv")

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Speed
##
## -------------------------------------------------------------------------

reg <- lm(formula = avg_speed ~ contract_year + min + as.factor(name) + as.factor(season) + salary_current, data = nba, weights = nba$MIN)
summary.reg <- my.summary.lm(summary(reg), 
                             my.rows=grep("contract_year|min|season|salary_current", names(coef(reg))))

## -------------------------------------------------------------------------
##
## Double LASSO: Average Speed
##
## -------------------------------------------------------------------------

X <- c("contract_year", "height", "weight", "season", "pos", "g", "mp", "rk", "salary_current")
nba.lasso <- nba %>%
  select(all_of(X))
nba.y <- nba %>%
  select("avg_speed")
nba.d <- nba %>%
  select("contract_year")
formula <- paste("avg_speed ~", paste(colnames(nba.lasso), collapse = "+"))
formula <- as.formula(formula)
dlasso <- rlassoEffects(formula, I=~contract_year, data=nba)

print(dlasso)
summary(dlasso)
confint(dlasso)
plot(dlasso)

## -------------------------------------------------------------------------
##
## Double LASSO test
##
## -------------------------------------------------------------------------

n = 100 #sample size
p = 100 # number of variables
s = 3 # nubmer of non-zero variables
X = matrix(rnorm(n*p), ncol=p)
colnames(X) <- paste("X", 1:p, sep="")
beta = c(rep(3,s), rep(0,p-s))
y = 1 + X%*%beta + rnorm(n)
data = data.frame(cbind(y,X))
colnames(data)[1] <- "y"
fm = paste("y ~", paste(colnames(X), collapse="+"))
fm = as.formula(fm)
lasso.effect = rlassoEffects(X, y, index=c(1,2,3,50))
lasso.effect = rlassoEffects(fm, I = ~ X1 + X2 + X3 + X50, data=data)
print(lasso.effect)
summary(lasso.effect)
confint(lasso.effect)
plot(lasso.effect)

data("GrowthData") # = use ?GrowthData for more information = #
dataset=GrowthData[,-2]
lasso = rlasso(Outcome~., data = dataset, post = FALSE) # = Run the Rigorous LASSO = #
selected = which(coef(lasso)[-c(1:2)] !=0) # = Select relevant variables = #
formula = paste(c("Outcome ~ gdpsh465", names(selected)), collapse = "+")
SS = summary(lm(formula, data = dataset))$coefficients[1, ]
DS=rlassoEffects(Outcome~. , I=~gdpsh465, data=dataset)
