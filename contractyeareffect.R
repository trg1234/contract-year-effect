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
library(fastDummies)

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
## Fixed Effect OLS: Usage Rate
##
## -------------------------------------------------------------------------

reg.usage <- lm(formula = usg ~ contract_year + min + as.factor(name) + pos + as.factor(season) 
              + salary_current, data = nba.advancedstats, weights = nba.advancedstats$min)
summary.usage <- my.summary.lm(summary(reg.usage), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(reg.usage))))

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

cat(stargazer(reg.dbox, reg.obox, reg.box, dep.var.labels = c("Defensive Box Outs",
                                                              "Offensive Box Outs",
                                                              "Box Outs"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Box Outs as the Dependent Variables"), sep = '\n', file = "tables/boxout.txt")
cat(stargazer(reg.avgs, reg.soff, reg.sdef, dep.var.labels = c("Average Speed", 
                                                               "Offensive Speed", "Defensive Speed"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Speed Metrics as the Dependent Variable"), sep = '\n', file = "tables/speed.txt")
cat(stargazer(reg.distdef, reg.distoff, reg.distfeet,
              dep.var.labels = c("Distance: Defensive", "Distance: Offensive", "Distance"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Distance as the Dependent Variable"), sep = '\n', file = "tables/distdef.txt")
cat(stargazer(reg.dws, reg.ows, reg.ws, reg.ws48,
              dep.var.labels = c("Defensive Win Shares", "Offensive Win Shares",
                                 "Win Shares", "Win Shares per 48 minutes"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Win Shares as the Dependent Variable"),sep = '\n',  file = "tables/dws.txt")
cat(stargazer(reg.avgd, dep.var.labels = "Average Seconds per Dribble",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Average Seconds per Dribble as the Dependent Variable"), sep = '\n', file = "tables/avgd.txt")
cat(stargazer(reg.avgt, dep.var.labels = "Average Seconds per Touch",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Average Seconds per Touch as the Dependent Variable"), sep = '\n', file = "tables/avgt.txt")
cat(stargazer(reg.usage, dep.var.labels = "Usage Rate",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played", "Position",
                                   "Current Salary"), omit = c("name", "year"),
              add.lines = list(c("Player Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Usage Rate"), sep = '\n', file = "tables/usage.txt")

## -------------------------------------------------------------------------
##
## Grouping it up by teams
##
## To prevent heterogeneity of inter-correlation of players
##
## -------------------------------------------------------------------------

todropa <- c("lg", "team.y", "name", "pos")

nba.a2 <- nba.advancedstats %>%
  select(-one_of(todropa))

todropt <- c("x", "name", "team.y")
  
nba.t2 <- nba.touch %>%
  select(-one_of(todropt))

todropb <- c("x", "name", "team.y")

nba.b2 <- nba.boxout %>%
  select(-one_of(todropb))

todropf <- c("name", "lg", "pos", "team.y", "team.x.x", "team.y.y")

nba.f2 <- nba.fullmerged %>%
  select(-one_of(todropf))

nba.a2 <- nba.a2 %>%
  filter(team.x != "TOT") %>%
  group_by(team.x, season) %>%
  mutate_all(funs(weighted.mean(.,min))) %>%
  summarize_all(mean)

nba.t2 <- nba.t2 %>%
  filter(team.x != "TOT") %>%
  group_by(team.x, season) %>%
  mutate_all(funs(weighted.mean(.,min.x))) %>%
  summarize_all(mean)

nba.b2 <- nba.b2 %>%
  filter(team.x != "TOT") %>%
  group_by(team.x, season) %>%
  mutate_all(funs(weighted.mean(.,min.x))) %>%
  summarize_all(mean)

nba.f2 <- nba.f2 %>%
  filter(team.x != "TOT") %>%
  group_by(team.x, season) %>%
  mutate_all(funs(weighted.mean(.,min))) %>%
  summarize_all(mean)

## -------------------------------------------------------------------------
##
## Fixed Effect OLS For Teams:
##
## -------------------------------------------------------------------------


team.usage <- lm(formula = usg ~ contract_year + min + as.factor(team.x) + as.factor(season) 
                + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamusage <- my.summary.lm(summary(team.usage), 
                               my.rows=grep("contract_year|min|season|salary_current",
                                            names(coef(team.usage))))

team.dws <- lm(formula = dws ~ contract_year + min + as.factor(team.x) + as.factor(season) 
              + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamdws <- my.summary.lm(summary(team.dws), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(team.dws))))

team.ows <- lm(formula = ows ~ contract_year + min + as.factor(team.x) + as.factor(season) 
              + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamows <- my.summary.lm(summary(team.ows), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(team.ows))))

team.ws <- lm(formula = ws ~ contract_year + min + as.factor(team.x) + as.factor(season) 
             + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamws <- my.summary.lm(summary(team.ws), 
                            my.rows=grep("contract_year|min|season|salary_current",
                                         names(coef(team.ws))))


team.distfeet <- lm(formula = dist_feet ~ contract_year + min + as.factor(team.x) + as.factor(season) 
                   + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamdistfeet <- my.summary.lm(summary(team.distfeet), 
                                  my.rows=grep("contract_year|min|season|salary_current",
                                               names(coef(team.distfeet))))

team.distdef <- lm(formula = dist_miles_def ~ contract_year + min + as.factor(team.x) + as.factor(season) 
                  + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamdistdef <- my.summary.lm(summary(team.distdef), 
                                 my.rows=grep("contract_year|min|season|salary_current",
                                              names(coef(team.distdef))))

team.distoff <- lm(formula = dist_miles_off ~ contract_year + min + as.factor(team.x) + as.factor(season) 
                  + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamdistoff <- my.summary.lm(summary(team.distoff), 
                                 my.rows=grep("contract_year|min|season|salary_current",
                                              names(coef(team.distoff))))

team.avgs <- lm(formula = avg_speed ~ contract_year + min + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamavgs <- my.summary.lm(summary(team.avgs), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.avgs))))

team.sdef <- lm(formula = avg_speed_def ~ contract_year + min + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamsdef <- my.summary.lm(summary(team.sdef), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.sdef))))

team.soff <- lm(formula = avg_speed_off ~ contract_year + min + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamsoff <- my.summary.lm(summary(team.soff), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.soff))))

team.ws48 <- lm(formula = ws_48 ~ contract_year + min + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.a2, weights = nba.a2$min)
summary.teamws48 <- my.summary.lm(summary(team.ws48), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.ws48))))

team.box <- lm(formula = box_outs ~ contract_year + min.y + as.factor(team.x) + as.factor(season) 
              + salary_current, data = nba.b2, weights = nba.b2$min.y)
summary.teambox <- my.summary.lm(summary(team.box), 
                             my.rows=grep("contract_year|min|season|salary_current",
                                          names(coef(team.box))))

team.obox <- lm(formula = off_box_outs ~ contract_year + min.y + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.b2, weights = nba.b2$min.y)
summary.teamobox <- my.summary.lm(summary(team.obox), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.obox))))

team.dbox <- lm(formula = def_box_outs ~ contract_year + min.y + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.b2, weights = nba.b2$min.y)
summary.teamdbox <- my.summary.lm(summary(team.dbox), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.dbox))))

team.avgt <- lm(formula = avg_sec_per_touch ~ contract_year + min.y + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.t2, weights = nba.t2$min.y)
summary.teamavgt <- my.summary.lm(summary(team.avgt), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.avgt))))

team.avgd <- lm(formula = avg_drib_per_touch ~ contract_year + min.y + as.factor(team.x) + as.factor(season) 
               + salary_current, data = nba.t2, weights = nba.t2$min.y)
summary.teamavgd <- my.summary.lm(summary(team.avgd), 
                              my.rows=grep("contract_year|min|season|salary_current",
                                           names(coef(team.avgd))))


## -------------------------------------------------------------------------
##
## Export Fixed Team Effect OLS as tables (w/ Stargazer)
##
## -------------------------------------------------------------------------

cat(stargazer(team.dbox, team.obox, team.box, dep.var.labels = c("Defensive Box Outs",
                                                              "Offensive Box Outs",
                                                              "Box Outs"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("team.x", "season"),
              add.lines = list(c("Team Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Box Outs as the Dependent Variables"), sep = '\n', file = "tables/tboxout.txt")
cat(stargazer(team.avgs, team.soff, team.sdef, dep.var.labels = c("Average Speed", 
                                                               "Offensive Speed", "Defensive Speed"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("team.x", "season"),
              add.lines = list(c("Team Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Speed Metrics as the Dependent Variable"), sep = '\n', file = "tables/tspeed.txt")
cat(stargazer(team.distdef, team.distoff, team.distfeet,
              dep.var.labels = c("Distance: Defensive", "Distance: Offensive", "Distance"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("team.x", "season"),
              add.lines = list(c("Team Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Distance as the Dependent Variable"), sep = '\n', file = "tables/tdistdef.txt")
cat(stargazer(team.dws, team.ows, team.ws, team.ws48,
              dep.var.labels = c("Defensive Win Shares", "Offensive Win Shares",
                                 "Win Shares", "Win Shares per 48 minutes"),
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",  "Position",
                                   "Current Salary"), omit = c("team.x", "season"),
              add.lines = list(c("Team Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Win Shares as the Dependent Variable"),sep = '\n',  file = "tables/tdws.txt")
cat(stargazer(team.avgd, dep.var.labels = "Average Seconds per Dribble",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("team.x", "season"),
              add.lines = list(c("Team Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Average Seconds per Dribble as the Dependent Variable"),
    sep = '\n', file = "tables/tavgd.txt")
cat(stargazer(team.avgt, dep.var.labels = "Average Seconds per Touch",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played",
                                   "Current Salary"), omit = c("team.x", "season"),
              add.lines = list(c("Team Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Using Average Seconds per Touch as the Dependent Variable"),
    sep = '\n', file = "tables/tavgt.txt")
cat(stargazer(team.usage, dep.var.labels = "Usage Rate",
              covariate.labels = c("Contract Year",
                                   "Average Minutes Played", "Position",
                                   "Current Salary"), omit = c("team.x", "season"),
              add.lines = list(c("Team Fixed Effects", "Yes"), 
                               c("Year Fixed Effects", "Yes")),
              title="Usage Rate"), sep = '\n', file = "tables/tusage.txt")


## -------------------------------------------------------------------------
##
## Double LASSO: OWS
##
## -------------------------------------------------------------------------

nba.fullmerged <- nba.fullmerged %>%
  filter(team.x != "TOT")

X <- c("contract_year", "name", "height", "weight", "season",
       "age.x", "pos", "g", "mp", "rk", "salary_current", "team.x", "w.y", "l.y")
X2 <- c("name", "team.x", "pos", "season")
nba.lasso <- nba.fullmerged %>%
  select(all_of(X))
nba.lasso <- dummy_cols(nba.lasso)
nba.lasso <- nba.lasso %>%
  select(-one_of(X2))
nba.lasso <- as.matrix(nba.lasso)

Z <- c("name", "height", "weight", "season",
       "age.x", "pos", "g", "mp", "rk", "salary_current", "team.x", "w.y", "l.y")
nba.z <- nba.fullmerged %>%
  select(all_of(Z))
nba.z <- dummy_cols(nba.z)
nba.z <- nba.z %>%
  select(-one_of(X2))
nba.z <- as.matrix(nba.z)

nba.y <- nba.fullmerged %>%
  select("ows")
nba.y <- as.vector(nba.y)

nba.d <- nba.fullmerged %>%
  select("contract_year")
nba.d <- as.vector(nba.d)

Y <- c("ows", "contract_year", "name", "height", "weight", "season",
       "age.x", "pos", "g", "mp", "rk", "salary_current", "team.x", "w.y", "l.y")
nba.subset <- nba %>%
  select(all_of(Y))
formula <- paste("avg_speed ~", paste(colnames(nba.subset), collapse = "+"))
formula <- as.formula(formula)

dlasso <- rlassoEffects(x = nba.lasso, y = nba.y, index=c(1:12),
                        method = "partialling out",
                        I3 = NULL,
                        post = TRUE)

lassoeff <- rlassoEffect(x = nba.z, y = nba.y, d = nba.d, method = "double selection",
                         I3 = NULL,
                         post = TRUE)

print(lassoeff)
summary(lassoeff)
confint(lassoeff)
plot(lassoeff)

print(dlasso)
summary(dlasso)

confint(dlasso)
plot(dlasso)

todropa <- c("lg", "team.y", "name", "pos")

nba.a2 <- nba.advancedstats %>%
  select(-one_of(todropa))

todropt <- c("x", "name", "team.y")

nba.t2 <- nba.touch %>%
  select(-one_of(todropt))

todropb <- c("x", "name", "team.y")

nba.b2 <- nba.boxout %>%
  select(-one_of(todropb))

todropf <- c("name", "lg", "pos", "team.y", "team.x.x", "team.y.y")

nba.f2 <- nba.fullmerged %>%
  select(-one_of(todropf))

nba.a2 <- nba.a2 %>%
  filter(team.x != "TOT") %>%
  group_by(team.x, season) %>%
  mutate_all(funs(weighted.mean(.,min))) %>%
  summarize_all(mean)

