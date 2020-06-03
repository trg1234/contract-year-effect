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
library(stargazer)

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
boxout <- read.csv("boxouts.csv", header = TRUE)
touches <- read.csv("touches.csv", header = TRUE)

## Removes NA

cleaned <- na.omit(data)
cboxout <- na.omit(boxout)
ctouches <- na.omit(touches)
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

cboxout <- cboxout %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(player = str_trim(player)) %>%
  mutate(player = str_replace_all(player, "[^[:alnum:] ]", "")) %>%
  mutate(player = str_replace(player, " ", "_"))

ctouches <- ctouches %>%
  clean_names() %>%
  rename_all(~str_replace_all(.,"\\.","_")) %>%
  mutate(player = str_trim(player)) %>%
  mutate(player = str_replace_all(player, "[^[:alnum:] ]", "")) %>%
  mutate(player = str_replace(player, " ", "_"))

## Removes duplicates

cleaned <- unique(cleaned)
cleaned.contract <- unique(cleaned.contract)
cboxout <- unique(cboxout)
ctouches <- unique(ctouches)

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
names(cboxout)[names(cboxout) == 'player'] <- 'name'
names(cboxout)[names(cboxout) == 'year'] <- 'season'
names(ctouches)[names(ctouches) == 'player'] <- 'name'
names(ctouches)[names(ctouches) == 'year'] <- 'season'
cleaned$season <- as.numeric(str_replace(cleaned$season, "-\\d+", ""))
cleaned$season <- cleaned$season + 1
cleaned$season <- as.factor(cleaned$season)
cleaned.contract$season <- as.factor(cleaned.contract$season)
cboxout$season <- as.factor(cboxout$season)
ctouches$season <- as.factor(ctouches$season)

## Merges contract and advanced stats

nba.advancedstats <- left_join(cleaned, cleaned.contract, by = c("name", "season"))
nba.advancedstats <- na.omit(nba.advancedstats)

write.csv(nba.advancedstats, file = "nbaadvancedstats.csv")

## Merges contract and boxout

nba.boxout <- left_join(cleaned.contract, cboxout, by = c("name", "season"))
nba.boxout <- na.omit(nba.boxout)

write.csv(nba.boxout, file = "nbaboxout.csv")

## Merges contract and touch

nba.touch <- left_join(cleaned.contract, ctouches, by = c("name", "season"))
nba.touch <- na.omit(nba.touch)

write.csv(nba.touch, file = "nbatouches.csv")

## Full merged merges all 4 datasets.

nba.fullmerged <- left_join(cleaned, cleaned.contract, by = c("name", "season"))
nba.fullmerged <- left_join(nba.fullmerged, cboxout, by = c("name", "season"))
nba.fullmerged <- left_join(nba.fullmerged, ctouches, by = c("name", "season"))
nba.fullmerged <- na.omit(nba.fullmerged)

write.csv(nba.all, file = "nbaall.csv")

## Drop duplicate columns. Note: this includes duplicates due to
## different units etc.

drop.col <- c("gp", "dist_feet", "dist_miles_off", "dist_miles_def", "avg_speed_off",
          "avg_speed_def", "salary_next")
nba <- nba %>% select(-one_of(drop.col))

## Export our current dataframe as CSV.

write.csv(nba, file = "nba.csv")

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Defensive Win Shares
##
## -------------------------------------------------------------------------

reg.dws <- lm(formula = dws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.dws <- my.summary.lm(summary(reg.dws), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.dws))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Offensive Win Shares
##
## -------------------------------------------------------------------------

reg.ows <- lm(formula = ows ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.ows <- my.summary.lm(summary(reg.ows), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.ows))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Total Win Shares
##
## -------------------------------------------------------------------------

reg.ws <- lm(formula = ws ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.ws <- my.summary.lm(summary(reg.ws), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.ws))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Distance (feet)
##
## -------------------------------------------------------------------------

reg.distfeet <- lm(formula = dist_feet ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
             + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.distfeet <- my.summary.lm(summary(reg.distfeet), 
                            my.rows=grep("contract_year|min|season|salary_current",
                                         names(coef(reg.distfeet))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Distance (feet): Defensive
##
## -------------------------------------------------------------------------

reg.distdef <- lm(formula = dist_miles_def ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                   + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.distdef <- my.summary.lm(summary(reg.distdef), 
                                  my.rows=grep("contract_year|min|season|salary_current",
                                               names(coef(reg.distdef))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Distance (feet): Offensive
##
## -------------------------------------------------------------------------

reg.distoff <- lm(formula = dist_miles_off ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
                  + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.distoff <- my.summary.lm(summary(reg.distoff), 
                                 my.rows=grep("contract_year|min|season|salary_current",
                                              names(coef(reg.distoff))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Speed
##
## -------------------------------------------------------------------------

reg.avgs <- lm(formula = avg_speed ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.avgs <- my.summary.lm(summary(reg.avgs), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.avgs))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Speed: Defense
##
## -------------------------------------------------------------------------

reg.sdef <- lm(formula = avg_speed_def ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.sdef <- my.summary.lm(summary(reg.sdef), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.sdef))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Speed: Offense
##
## -------------------------------------------------------------------------

reg.soff <- lm(formula = avg_speed_off ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.soff <- my.summary.lm(summary(reg.soff), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.soff))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Win Shares per 48 Mins
##
## -------------------------------------------------------------------------

reg.ws48 <- lm(formula = ws_48 ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
               + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.ws48 <- my.summary.lm(summary(reg.ws48), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.ws48))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Box Outs
##
## -------------------------------------------------------------------------

reg.box <- lm(formula = box_outs ~ contract_year + min.y + as.factor(name) + as.factor(season) 
               + salary_current, data = nba.boxout, weights = nba.boxout$min.y)
summary.box <- my.summary.lm(summary(reg.box), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.box))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Off Box Outs
##
## -------------------------------------------------------------------------

reg.obox <- lm(formula = off_box_outs ~ contract_year + min.y + as.factor(name) + as.factor(season) 
              + salary_current, data = nba.boxout, weights = nba.boxout$min.y)
summary.obox <- my.summary.lm(summary(reg.obox), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.obox))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Def Box Outs
##
## -------------------------------------------------------------------------

reg.dbox <- lm(formula = def_box_outs ~ contract_year + min.y + as.factor(name) + as.factor(season) 
               + salary_current, data = nba.boxout, weights = nba.boxout$min.y)
summary.dbox <- my.summary.lm(summary(reg.dbox), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.dbox))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Seconds per Touch
##
## -------------------------------------------------------------------------

reg.avgt <- lm(formula = avg_sec_per_touch ~ contract_year + min.y + as.factor(name) + as.factor(season) 
               + salary_current, data = nba.touch, weights = nba.touch$min.y)
summary.avgt <- my.summary.lm(summary(reg.avgt), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.avgt))))

## -------------------------------------------------------------------------
##
## Fixed Effect OLS: Average Dribbles per Touch
##
## -------------------------------------------------------------------------

reg.avgd <- lm(formula = avg_drib_per_touch ~ contract_year + min.y + as.factor(name) + as.factor(season) 
               + salary_current, data = nba.touch, weights = nba.touch$min.y)
summary.avgd <- my.summary.lm(summary(reg.avgd), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(reg.avgd))))

## -------------------------------------------------------------------------
##
## Export Fixed Effect OLS as tables (w/ Stargazer)
##
## -------------------------------------------------------------------------

cat(stargazer(reg.dbox, dep.var.labels = "Defensive Box Outs",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Defensive Box Outs as the Dependent Variable"), file = "dbox.txt")
cat(stargazer(reg.avgs, dep.var.labels = "Average Speed",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Average Speed as the Dependent Variable"), file = "avgs.txt")
cat(stargazer(reg.box, dep.var.labels = "Box Outs",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Box Outs as the Dependent Variable"), file = "box.txt")
cat(stargazer(reg.distdef, dep.var.labels = "Distance: Defensive",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Distance: Defensive as the Dependent Variable"), file = "distdef.txt")
cat(stargazer(reg.distfeet, dep.var.labels = "Distance",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Distance as the Dependent Variable"), file = "distfeet.txt")
cat(stargazer(reg.distoff, dep.var.labels = "Distance: Offensive",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Distance: Offensive as the Dependent Variable"), file = "distoff.txt")
cat(stargazer(reg.dws, dep.var.labels = "Defensive Win Shares",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Defensive Win Shares as the Dependent Variable"), file = "dws.txt")
cat(stargazer(reg.obox, dep.var.labels = "Offensive Boxouts",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Offensive Boxouts as the Dependent Variable"), file = "obox.txt")
cat(stargazer(reg.ows, dep.var.labels = "Offensive Win Shares",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Offensive Win Shares as the Dependent Variable"), file = "ows.txt")
cat(stargazer(reg.sdef, dep.var.labels = "Defensive Speed",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Defensive Speed as the Dependent Variable"), file = "sdef.txt")
cat(stargazer(reg.soff, dep.var.labels = "Offensive Speed",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Offensive Speed as the Dependent Variable"), file = "soff.txt")
cat(stargazer(reg.ws, dep.var.labels = "Win Shares",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Win Shares as the Dependent Variable"), file = "ws.txt")
cat(stargazer(reg.ws48, dep.var.labels = "Win Shares per 48 minutes",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Win Shares per 48 minutes as the Dependent Variable"), file = "ws48.txt")
cat(stargazer(reg.avgd, dep.var.labels = "Average Seconds per Dribble",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Average Seconds per Dribble as the Dependent Variable"), file = "avgd.txt")
cat(stargazer(reg.avgt, dep.var.labels = "Average Seconds per Touch",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Average Seconds per Touch as the Dependent Variable"), file = "avgt.txt")


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
Y <- c("avg_speed", "contract_year", "height", "weight", "season", "pos", "g", "mp", "rk", "salary_current")
nba.subset <- nba %>%
  select(all_of(Y))
formula <- paste("avg_speed ~", paste(colnames(nba.lasso), collapse = "+"))
formula <- as.formula(formula)
dlasso <- rlassoEffects(formula, I=~contract_year, data=nba.subset)

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
